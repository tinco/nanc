module Nanc.IR.CodeGen where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types
import Nanc.IR.Statement
import Nanc.IR.Expression
import Nanc.IR.Declarations

{-
 A module consists of a list of external declarations.
-}
generate :: String -> CTranslUnit -> AST.Module
generate name (CTranslUnit decls _) = llvmModuleState $ runModule (emptyModule name) (mapM_ generateExtDecl decls)
