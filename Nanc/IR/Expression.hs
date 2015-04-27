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

-- So the problem here is that we're going to mutate expr
-- in C the ++ is only legal when expr is assignable.
-- we solve that by making CVar expressions return pointers
-- instead of direct values. But then all other expressions
-- need to explicitly load the pointers, but this would break
-- in case of real pointer arithmetic. So it's better to have
-- the assignments as an exception. Could we store both the
-- address and the Operand in the return value?

-- var++
generateExpression (CUnary CPostIncOp expr _) = do
	(val, Just addr) <- generateExpression expr
	let t = operandToType val
	inc_val <- add t val (AST.ConstantOperand $ C.Int 32 1)
	store t addr inc_val
	return (val, Nothing)

-- 1: CUnary CIndOp (CUnary CPostIncOp (CMember (CVar (Ident "_p" 14431 (_)) (_)) (Ident "_p" 14431 (_)) True (_)) (_)) (_)
-- 2: CVar (Ident "_c" 12767 (_)) (_)
generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined