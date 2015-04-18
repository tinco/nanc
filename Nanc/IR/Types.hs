module Nanc.IR.Types where

import LLVM.General.AST hiding (Module)
import LLVM.General.AST.AddrSpace

import Nanc.AST

import Debug.Trace

qualifiedTypeToType :: [Definition] -> QualifiedType -> Type
qualifiedTypeToType _ (QualifiedType (ST t) qs) = simpleTypeToType t qs
qualifiedTypeToType m (QualifiedType (CT t) qs) = complexTypeToType m t qs
qualifiedTypeToType m (QualifiedType (Ptr qt) qs) = PointerType (qualifiedTypeToType m qt) (AddrSpace 0)
qualifiedTypeToType _ (QualifiedType NoTypeSpec _) = IntegerType 32
qualifiedTypeToType _ qt = trace ("Unimplemented type: " ++ show qt) undefined

simpleTypeToType :: SimpleType -> TypeQualifiers -> Type
simpleTypeToType SignedInt _ = IntegerType 32
simpleTypeToType Void _ = VoidType
simpleTypeToType t qs = trace ("Unimplemented simple type: " ++ (show t)) undefined

complexTypeToType :: [Definition] -> ComplexType -> TypeQualifiers -> Type
complexTypeToType _ (CSU _ _) _ = StructureType False []
complexTypeToType defs (TD name) _ = lookupType defs name
complexTypeToType _ t _ = trace ("Unimplemented complex type: " ++ (show t)) undefined

lookupType :: [Definition] -> String -> Type
lookupType [] n = error ("Referenced undeclared type: " ++ n)
lookupType ((TypeDefinition (Name name) (Just typ)):rest) n 
	| name == n = typ
	| otherwise = lookupType rest n
lookupType (_:rest) n = lookupType rest n