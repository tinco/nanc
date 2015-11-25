module Nanc.IR.Expression where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Char

import Control.Monad

import Language.C
import Language.C.Data.Ident
import Language.C.Pretty
import Language.C.Syntax.AST

import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.IntegerPredicate as I
import qualified LLVM.General.AST.FloatingPointPredicate as F

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Instructions
import qualified LLVM.General.AST.Constant as C

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

generateExpression :: TypeTable -> CExpr -> Codegen (
		AST.Operand,      -- return value
		Maybe AST.Operand, -- address if value has an address
		QualifiedType -- Type of return value
	)
-- var()
generateExpression ts (CCall fn' args' _) = do
	args <- mapM (generateExpression ts) args'
	(fn,_,t) <- generateExpression ts fn'
	result <- call fn (map fst3 args)
	return (result, Nothing, returnType t)

-- var
generateExpression ts (CVar (Ident name _ _) _) = do
	(address, typ) <- getvar name
	case typ of
		QualifiedType (FT _) _ -> return (address, Nothing, typ)
		_ -> do
			let t = qualifiedTypeToType ts typ
			value <- load t address
			return (value, Just address, typ)

-- var = bar
generateExpression ts (CAssign CAssignOp leftExpr rightExpr _) = do
	exprResult <- generateExpression ts leftExpr
	let addr = case exprResult of
		(_, Just addr, _) -> addr
		-- TODO typecheck if addr really is a pointer
		(addr, Nothing, _typ) -> addr

	(val, _, typ) <- generateExpression ts rightExpr
	let t = qualifiedTypeToType ts typ
	store t addr val
	return (val, Nothing, typ)

-- *var
generateExpression ts (CUnary CIndOp expr _) = do
	traceM ("Dereferencing expression: " ++ (show expr)) 
	exprResult <- generateExpression ts expr
	return $ case exprResult of
		(_, Just addr, typ) -> (addr, Nothing, typ)
		-- TODO typecheck if addr really is a pointer
		(addr, Nothing, typ) -> (addr, Nothing, typ)

-- var++
generateExpression ts (CUnary CPostIncOp expr _) = do
	(val, Just addr, typ) <- generateExpression ts expr
	let t = qualifiedTypeToType ts typ
	inc_val <- add t val (intConst 1)
	store t addr inc_val
	return (val, Nothing, typ)

-- --var;
generateExpression ts (CUnary CPreDecOp expr _) = do
	(val, Just addr, typ) <- generateExpression ts expr
	let t = qualifiedTypeToType ts typ
	dec_val <- add t val (intConst (-1))
	store t addr dec_val
	return (dec_val, Nothing, typ)

--  Binary expressions
generateExpression ts (CBinary op leftExpr rightExpr _) = do
	(leftVal, _, typ) <- generateExpression ts leftExpr
	(rightVal, _, typ2) <- generateExpression ts rightExpr
	(result, t) <- binaryOp op (leftVal, typ) (rightVal, typ2)
	return (result, Nothing, t)

-- CVar (Ident "_p" 14431 n) n
generateExpression ts (CMember subjectExpr (Ident memName _ _) _bool _) = do
	(_, Just addr, typ) <- generateExpression ts subjectExpr
	let (i, resultType) = lookupMember ts typ memName

	let t = qualifiedTypeToType ts resultType
	let idx = intConst $ fromIntegral i

	resultAddr <- instr t (AST.GetElementPtr True addr [idx] [])
	value <- load t resultAddr
	return (value, Just resultAddr, resultType)

-- (CConst (CCharConst '\n' ()))
-- (CConst (CIntConst 0 ())) ())
generateExpression _ (CConst (CIntConst (CInteger i _ _) _)) = return (result, Nothing, typ)
	where
		result = intConst64 $ fromIntegral i
		typ = QualifiedType (ST SignedInt) (defaultTypeQualifiers { typeIsConst = True })

generateExpression ts (CConst (CStrConst (CString str _) _)) = do
		name <- literal (cnst, typ)
		let t = qualifiedTypeToType ts typ
		let addr = global (AST.Name name) t
		result <- instr (AST.PointerType (AST.IntegerType 8) (AddrSpace 0)) (AST.GetElementPtr True addr [intConst64 0, intConst64 0] [])
		return (result, Nothing, typ)
	where
		cnst = C.Array (AST.IntegerType 8) (map ((C.Int 8).fromIntegral.ord) str)
		typ = QualifiedType (Arr (fromIntegral $ length str) (QualifiedType (ST Char) constTypeQualifiers)) constTypeQualifiers

generateExpression ts (CConst (CCharConst (CChar chr _) _)) = return (result, Nothing, typ)
	where
		result = intConst8 $ fromIntegral $ ord chr
		typ = QualifiedType (ST Char) (defaultTypeQualifiers { typeIsConst = True })
		
generateExpression _ (CConst c) = trace ("\n\nI don't know how to do CConst: " ++ (show c) ++ "\n\n") undefined
generateExpression ts (CCast decl expr _) = do
		(value, addr, _) <- generateExpression ts expr
		return (value, addr, t)
	where
		t = declarationType.buildDeclaration $ decl

generateExpression _ expr = trace ("encountered expr: " ++ (show expr)) undefined

lookupMember :: TypeTable -> QualifiedType -> String -> (Int, QualifiedType)
lookupMember ts typ memName = (i, resultType)
	where
		members = extractMembers ts typ
		i = head $ elemIndices memName $ map (declarationName) members
		resultType = declarationType $ members !! i

-- Extracts the members from a Struct, TD or Ptr to a Struct
extractMembers :: TypeTable -> QualifiedType -> [Declaration]
extractMembers ts (QualifiedType (CT (Struct _ members _)) _) = members
extractMembers ts (QualifiedType (CT (TD n)) _)  = case lookup n ts of
	Just t -> extractMembers ts t
	Nothing -> trace ("Could not find struct type: " ++ (show n)) undefined
-- this doesn't really make sense..
extractMembers ts (QualifiedType (Ptr s) _) = extractMembers ts s
extractMembers ts s = trace ("Unexptected struct type: " ++ (show s)) undefined

-- choose between icmp and fcmp
binaryOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen (AST.Operand, QualifiedType)
binaryOp CLorOp a b = liftM2 (,) (binInstr AST.Or a b) (return $ snd a) 
binaryOp CLndOp a b = liftM2 (,) (binInstr AST.And a b) (return $ snd a)
binaryOp CNeqOp a b = liftM2 (,) (cmpOp CNeqOp a b) (return defaultBooleanType)
binaryOp CGeqOp a b = liftM2 (,) (cmpOp CGeqOp a b) (return defaultBooleanType)
binaryOp CGrOp  a b = liftM2 (,) (cmpOp CGrOp a b) (return defaultBooleanType)
binaryOp op _ _ = trace ("Don't know how to binaryOp: " ++ (show op)) undefined


cmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
cmpOp cmp a@(a',_) b | isInteger a' = iCmpOp cmp a b
                     | otherwise = fCmpOp cmp a b

iCmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
iCmpOp cmp (a,t) (b,_) = icmp (iOpToPred (isSigned t) cmp) a b

fCmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
fCmpOp cmp (a,t) (b,_) = fcmp (fOpToPred cmp) a b 

data FloatOrInt = Floaty | Intty
iOpToPred :: Bool -> CBinaryOp -> I.IntegerPredicate
iOpToPred _ CNeqOp = I.NE
iOpToPred True CGeqOp = I.SGE
iOpToPred False CGeqOp = I.UGE
iOpToPred True CGrOp = I.SGT
iOpToPred False CGrOp = I.UGT

fOpToPred :: CBinaryOp -> F.FloatingPointPredicate
fOpToPred CGeqOp = F.UEQ
fOpToPred CNeqOp = F.UNE
fOpToPred CGrOp = F.UGT

intConst :: Integer -> AST.Operand
intConst = intConst32

intConst8 :: Integer -> AST.Operand
intConst8 = AST.ConstantOperand . C.Int 8

intConst32 :: Integer -> AST.Operand
intConst32 = AST.ConstantOperand . C.Int 32

intConst64 :: Integer -> AST.Operand
intConst64 = AST.ConstantOperand . C.Int 64

isInteger :: AST.Operand -> Bool
isInteger (AST.LocalReference (AST.IntegerType _) _) = True
isInteger (AST.ConstantOperand (C.Int _ _)) = True
isInteger _ = False

isFloat :: AST.Operand -> Bool
isFloat (AST.LocalReference (AST.FloatingPointType _ _) _) = True
isFloat (AST.ConstantOperand (C.Float _)) = True
isFloat _ = False
