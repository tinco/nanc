module Nanc.IR.Types where

import LLVM.General.AST hiding (Module)
import LLVM.General.AST.AddrSpace

import Nanc.AST

import Debug.Trace

qualifiedTypeToType :: [(String, QualifiedType)] -> QualifiedType -> Type
qualifiedTypeToType _ (QualifiedType (ST t) qs) = simpleTypeToType t qs
qualifiedTypeToType m (QualifiedType (CT t) qs) = complexTypeToType m t qs
qualifiedTypeToType m (QualifiedType (Ptr qt) qs) = PointerType (qualifiedTypeToType m qt) (AddrSpace 0)
qualifiedTypeToType _ (QualifiedType NoTypeSpec _) = IntegerType 32
qualifiedTypeToType _ qt = trace ("Unimplemented type: " ++ show qt) undefined

simpleTypeToType :: SimpleType -> TypeQualifiers -> Type
simpleTypeToType SignedInt _ = IntegerType 32
simpleTypeToType Void _ = VoidType
simpleTypeToType SignedLongLongInt _ = IntegerType 64
simpleTypeToType Char _ = IntegerType 8
simpleTypeToType t qs = trace ("Unimplemented simple type: " ++ (show t)) undefined

complexTypeToType :: [(String, QualifiedType)] -> ComplexType -> TypeQualifiers -> Type
complexTypeToType _ (CSU _ _) _ = StructureType False []
complexTypeToType defs (TD name) _ = qualifiedTypeToType defs $ lookupType defs name
complexTypeToType _ t _ = trace ("Unimplemented complex type: " ++ (show t)) undefined

lookupType :: [(String, QualifiedType)] -> String -> QualifiedType
lookupType [] n = error ("Referenced undeclared type: " ++ n)
lookupType ((name, typ):rest) n 
	| name == n = typ
	| otherwise = lookupType rest n