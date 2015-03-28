module Nanc.IR.Expression where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types

generateExpression :: CExpr -> Codegen (AST.Operand)
generateExpression (CCall fn' args' _) = do
	_args <- mapM generateExpression args'
	_fn <- generateExpression fn'
	-- call fn args
	return undefined

generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined