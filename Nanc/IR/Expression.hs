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

-- 	CVar (Ident "__swbuf" 253143745 (NodeInfo ("/usr/include/stdio.h": line 352) (("/usr/include/stdio.h": line 352),7) (Name {nameId = 1455}))) (NodeInfo ("/usr/include/stdio.h": line 352) (("/usr/include/stdio.h": line 352),7) (Name {nameId = 1456}))
generateExpression (CVar (Ident name _ _) _) = trace ("Variable reference: " ++ (show name)) undefined

generateExpression expr = trace ("encountered expr: " ++ (show expr)) undefined