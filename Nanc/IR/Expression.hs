module Nanc.IR.Expression where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types
import Nanc.IR.Instructions
import qualified LLVM.General.AST.Constant as C

generateExpression :: CExpr -> Codegen (
		AST.Operand,      -- return value
		Maybe AST.Operand -- address if value has an address
	)
-- var()
generateExpression (CCall fn' args' _) = do
	_args <- mapM generateExpression args'
	_fn <- generateExpression fn'
	-- call fn args
	return $ trace "Generating expression: " undefined

-- var
generateExpression (CVar (Ident name _ _) _) = do
	address <- getvar name
	let t = operandToType address
	value <- load (AST.pointerReferent t) address
	return (value, Just address)

-- var = bar
generateExpression (CAssign CAssignOp leftExpr rightExpr _) = do
	(_, Just addr) <- generateExpression leftExpr
	(val, _) <- generateExpression rightExpr
	let t = operandToType val
	store t addr val
	return (val, Nothing)

-- *var
generateExpression (CUnary CIndOp expr _) = do
	(_, Just addr) <- generateExpression expr
	return (addr, Nothing)

-- var++
generateExpression (CUnary CPostIncOp expr _) = do
	(val, Just addr) <- generateExpression expr
	let t = operandToType val
	inc_val <- add t val (AST.ConstantOperand $ C.Int 32 1)
	store t addr inc_val
	return (val, Nothing)

-- --var;
generateExpression (CUnary CPreDecOp expr _) = do
	(val, Just addr) <- generateExpression expr
	let t = operandToType val
	dec_val <- add t val (AST.ConstantOperand $ C.Int 32 (-1))
	store t addr dec_val
	return (dec_val, Nothing)

--  Binary expressions
generateExpression (CBinary op leftExpr rightExpr _) = do
	(leftVal, _) <- generateExpression leftExpr
	(rightVal, _) <- generateExpression rightExpr
	result <- binaryOp op leftVal rightVal
	return (result, Nothing)

generateExpression (CMember subjectExpr (Ident memname _ _) _bool _) = undefined 

-- (CConst (CCharConst '\n' ()))
-- (CConst (CIntConst 0 ())) ())
generateExpression (CConst c) = undefined
generateExpression (CCast decl expr _) = undefined

generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined


-- choose between icmp and fcmp
binaryOp :: CBinaryOp -> AST.Operand -> AST.Operand -> Codegen AST.Operand
binaryOp CLorOp a b = binInstr AST.Or a b
binaryOp CLndOp a b = binInstr AST.And a b
binaryOp CNeqOp a b
	| isInteger a && isInteger b = intNeq
	| isFloat a && isFloat b = fNeq
	| otherwise = trace ("Binary expression on non-float non-integer types or mixed") 
	where
		intNeq = notInstr $ binInstr AST.And a b
		fNeq = undefined

binaryOp CGeqOp a b = undefined

isInteger :: AST.Operand -> Bool
isInteger (AST.LocalReference (AST.IntegerType _) _) = True
isInteger (AST.ConstantOperand (C.Int _ _)) = True
isInteger _ = False

isFloat :: AST.Operand -> Bool
isFloat (AST.LocalReference (AST.FloatingPointType _ _) _) = True
isFloat (AST.ConstantOperand (C.Float _)) = True
isFloat _ = False