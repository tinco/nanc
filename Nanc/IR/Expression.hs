module Nanc.IR.Expression where

import Debug.Trace
import Data.Maybe
import Data.List

import Control.Monad

import Language.C
import Language.C.Data.Ident
import Language.C.Pretty

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types
import Nanc.IR.Instructions
import qualified LLVM.General.AST.Constant as C

generateExpression :: CExpr -> Codegen (
		AST.Operand,      -- return value
		Maybe AST.Operand, -- address if value has an address
		QualifiedType -- Type of return value
	)
-- var()
generateExpression (CCall fn' args' _) = do
	_args <- mapM generateExpression args'
	_fn <- generateExpression fn'
	-- call fn args
	return $ trace "Generating expression: " undefined

-- var
generateExpression (CVar (Ident name _ _) _) = do
	(address, typ) <- getvar name
	let t = qualifiedTypeToType undefined typ
	value <- load (AST.pointerReferent t) address
	return (value, Just address, typ)

-- var = bar
generateExpression (CAssign CAssignOp leftExpr rightExpr _) = do
	(_, Just addr, _) <- generateExpression leftExpr
	(val, _, typ) <- generateExpression rightExpr
	let t = qualifiedTypeToType undefined typ
	store t addr val
	return (val, Nothing, typ)

-- *var
generateExpression (CUnary CIndOp expr _) = do
	(_, Just addr, typ) <- generateExpression expr
	return (addr, Nothing, typ)

-- var++
generateExpression (CUnary CPostIncOp expr _) = do
	(val, Just addr, typ) <- generateExpression expr
	let t = qualifiedTypeToType undefined typ
	inc_val <- add t val (intConst 1)
	store t addr inc_val
	return (val, Nothing, typ)

-- --var;
generateExpression (CUnary CPreDecOp expr _) = do
	(val, Just addr, typ) <- generateExpression expr
	let t = qualifiedTypeToType undefined typ
	dec_val <- add t val (intConst (-1))
	store t addr dec_val
	return (dec_val, Nothing, typ)

--  Binary expressions
generateExpression (CBinary op leftExpr rightExpr _) = do
	(leftVal, _, typ) <- generateExpression leftExpr
	(rightVal, _, typ2) <- generateExpression rightExpr
	-- TODO: refactor binaryop to return the type
	(result, t) <- binaryOp op (leftVal, typ) (rightVal, typ2)
	return (result, Nothing, t)

-- CVar (Ident "_p" 14431 n) n
generateExpression (CMember subjectExpr (Ident memName _ _) _bool _) = do
	(_, Just addr, typ) <- generateExpression subjectExpr
	let (QualifiedType  (CT (Struct _ members _)) _) = typ
	let i = head $ elemIndices memName $ map (declarationName) members
	let resultType = declarationType $ members !! i
	let t = qualifiedTypeToType undefined resultType
	let idx = intConst (fromIntegral i)
	resultAddr <- instr (AST.pointerReferent t) (AST.GetElementPtr True addr [idx] [])
	value <- load (AST.pointerReferent t) resultAddr
	return (value, Just resultAddr, resultType)

-- (CConst (CCharConst '\n' ()))
-- (CConst (CIntConst 0 ())) ())
generateExpression (CConst (CIntConst (CInteger i _ _) _)) = return (intConst64 $ fromIntegral i, Nothing, QualifiedType (ST SignedInt) (defaultTypeQualifiers { typeIsConst = True }))
generateExpression (CConst c) = trace ("\n\nI don't know how to do CConst: " ++ (show c) ++ "\n\n") undefined
generateExpression (CCast decl expr _) = trace "I don't know how to do CCast: " undefined

generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined


-- choose between icmp and fcmp
binaryOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen (AST.Operand, QualifiedType)
binaryOp CLorOp a b = liftM2 (,) (binInstr AST.Or a b) (return $ snd a) 
binaryOp CLndOp a b = liftM2 (,) (binInstr AST.And a b) (return $ snd a)
binaryOp CNeqOp a'@(a,t) b'@(b,_)
	| isInteger a && isInteger b = liftM2 (,) (intNeq) (return t)
	| isFloat a && isFloat b = liftM2 (,) (fNeq) (return t)
	| otherwise = trace ("Binary expression on non-float non-integer types or mixed:") undefined
	where
		intNeq = do
			r <- binInstr AST.And a' b'
			(notInstr r)
		fNeq = trace ("I dont know how to compare floats yet: ") undefined

binaryOp CGeqOp a b = trace ("I don't know how to do CGeqOp: ") undefined

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