module Nanc.IR.Types where

import LLVM.General.AST hiding (Module)

import Nanc.AST

import Debug.Trace

generateType :: QualifiedType -> Type
generateType (QualifiedType (ST Void) _) = VoidType
generateType (QualifiedType (ST SignedInt) _) = IntegerType 32
generateType (QualifiedType NoTypeSpec _) = IntegerType 32
generateType t = trace ("Unimplemented type: " ++ (show t)) VoidType