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

import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.IntegerPredicate as I
import qualified LLVM.General.AST.FloatingPointPredicate as F

import Nanc.CodeGenState
import Nanc.AST

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
			value <- load (AST.pointerReferent t) address
			return (value, Just address, typ)

-- var = bar
generateExpression ts (CAssign CAssignOp leftExpr rightExpr _) = do
	(_, Just addr, _) <- generateExpression ts leftExpr
	(val, _, typ) <- generateExpression ts rightExpr
	let t = qualifiedTypeToType ts typ
	store t addr val
	return (val, Nothing, typ)

-- *var
generateExpression ts (CUnary CIndOp expr _) = do
	(_, Just addr, typ) <- generateExpression ts expr
	return (addr, Nothing, typ)

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
	let (QualifiedType  (CT (Struct _ members _)) _) = typ
	let i = head $ elemIndices memName $ map (declarationName) members
	let resultType = declarationType $ members !! i
	let t = qualifiedTypeToType ts resultType
	let idx = intConst (fromIntegral i)
	resultAddr <- instr (trace ("\n\nGoing to get reference to: " ++ (show ts)) $ AST.pointerReferent t) (AST.GetElementPtr True addr [idx] [])
	value <- load (trace ("\n\nGoing to get reference to: " ++ (show ts)) $ AST.pointerReferent t) resultAddr
	return (value, Just resultAddr, resultType)

-- (CConst (CCharConst '\n' ()))
-- (CConst (CIntConst 0 ())) ())
generateExpression _ (CConst (CIntConst (CInteger i _ _) _)) = return (result, Nothing, typ)
	where
		result = intConst64 $ fromIntegral i
		typ = QualifiedType (ST SignedInt) (defaultTypeQualifiers { typeIsConst = True })

generateExpression _ (CConst (CStrConst (CString str _) _)) = return (result, Nothing, typ)
	where
		result = AST.ConstantOperand $ C.Array (AST.IntegerType 8) (map ((C.Int 8).fromIntegral.ord) str)
		typ = QualifiedType (Arr (fromIntegral $ length str) (QualifiedType (ST Char) constTypeQualifiers)) constTypeQualifiers

generateExpression _ (CConst c) = trace ("\n\nI don't know how to do CConst: " ++ (show c) ++ "\n\n") undefined
generateExpression _ (CCast decl expr _) = trace "I don't know how to do CCast: " undefined

generateExpression _ expr = trace ("encountered expr: " ++ (show expr)) undefined


-- choose between icmp and fcmp
binaryOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen (AST.Operand, QualifiedType)
binaryOp CLorOp a b = liftM2 (,) (binInstr AST.Or a b) (return $ snd a) 
binaryOp CLndOp a b = liftM2 (,) (binInstr AST.And a b) (return $ snd a)
binaryOp CNeqOp a b = liftM2 (,) (cmpOp CNeqOp a b) (return defaultBooleanType)
binaryOp CGeqOp a b = liftM2 (,) (cmpOp CGeqOp a b) (return defaultBooleanType)

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

fOpToPred :: CBinaryOp -> F.FloatingPointPredicate
fOpToPred CGeqOp = F.UEQ
fOpToPred CNeqOp = F.UNE

intConst :: Integer -> AST.Operand
intConst = intConst32

intConst32 :: Integer -> AST.Operand
intConst32 = AST.ConstantOperand . (C.Int 32)

intConst64 :: Integer -> AST.Operand
intConst64 = AST.ConstantOperand . (C.Int 64)

isInteger :: AST.Operand -> Bool
isInteger (AST.LocalReference (AST.IntegerType _) _) = True
isInteger (AST.ConstantOperand (C.Int _ _)) = True
isInteger _ = False

isFloat :: AST.Operand -> Bool
isFloat (AST.LocalReference (AST.FloatingPointType _ _) _) = True
isFloat (AST.ConstantOperand (C.Float _)) = True
isFloat _ = False