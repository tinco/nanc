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

generateExpression :: CExpr -> Codegen (AST.Operand)
generateExpression (CCall fn' args' _) = do
	_args <- mapM generateExpression args'
	_fn <- generateExpression fn'
	-- call fn args
	return $ trace "Generating expression: " undefined

-- TODO: we are going to need types of expressions to generate this code.
-- TODO: why compile declarations we might never reference? We should
-- make this lazy. So a declaration only gets compiled when it's referenced
-- from main or is externally visible.
generateExpression (CVar (Ident name _ _) _) = getvar name
generateExpression (CAssign CAssignOp leftExpr rightExpr _) = do
	addr <- generateExpression leftExpr
	val <- generateExpression rightExpr
	t <- undefined
	store t addr val
	return val
generateExpression (CUnary CIndOp expr _) = do
	val <- generateExpression expr
	intToPtr undefined val

-- 1: CUnary CIndOp (CUnary CPostIncOp (CMember (CVar (Ident "_p" 14431 (_)) (_)) (Ident "_p" 14431 (_)) True (_)) (_)) (_)
-- 2: CVar (Ident "_c" 12767 (_)) (_)
generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined