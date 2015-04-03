module Nanc.IR.Types where

import LLVM.General.AST hiding (Module)

import Nanc.AST

import Debug.Trace

qualifiedTypeToType :: QualifiedType -> Type
qualifiedTypeToType (QualifiedType (ST t) qs) = simpleTypeToType t qs
qualifiedTypeToType (QualifiedType (CT t) qs) = complexTypeToType t qs
qualifiedTypeToType (QualifiedType NoTypeSpec _) = IntegerType 32
qualifiedTypeToType qt = trace ("Unimplemented type: " ++ show qt) undefined

simpleTypeToType :: SimpleType -> TypeQualifiers -> Type
simpleTypeToType SignedInt _ = IntegerType 32
simpleTypeToType Void _ = VoidType
simpleTypeToType t qs = trace ("Unimplemented simple type: " ++ (show t)) undefined

complexTypeToType :: ComplexType -> TypeQualifiers -> Type
complexTypeToType (CSU _ _) _ = StructureType False []
complexTypeToType t _ = trace ("Unimplemented simple type: " ++ (show t)) undefined