module Nanc.IR.Types where

import LLVM.General.AST hiding (Module)

import Nanc.AST

import Debug.Trace

generateType :: TypeSpec -> Type
generateType (ST Void) = VoidType
generateType (ST SignedInt) = IntegerType 32
generateType t = trace ("Unimplemented type: " ++ (show t)) VoidType