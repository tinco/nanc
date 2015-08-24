module Nanc.IR.Types where

import Language.C.Data.Ident
import Language.C.Syntax
import GHC.Stack

import LLVM.General.AST hiding (Module)
import LLVM.General.AST.AddrSpace

import Nanc.AST
import Data.List

import Debug.Trace

type TypeDefinitions = [(String, QualifiedType)]

qualifiedTypeToType :: TypeDefinitions -> QualifiedType -> Type
qualifiedTypeToType _ (QualifiedType (ST t) qs) = simpleTypeToType t qs
qualifiedTypeToType m (QualifiedType (CT t) qs) = complexTypeToType m t qs
qualifiedTypeToType m (QualifiedType (Ptr qt) qs) = PointerType (qualifiedTypeToType m qt) (AddrSpace 0)
qualifiedTypeToType _ (QualifiedType NoTypeSpec _) = IntegerType 32
qualifiedTypeToType m (QualifiedType (Arr l qt) qs) = ArrayType l (qualifiedTypeToType m qt)
qualifiedTypeToType _ qt = trace ("Unimplemented type: " ++ show qt) undefined

simpleTypeToType :: SimpleType -> TypeQualifiers -> Type
simpleTypeToType SignedInt _ = IntegerType 32
simpleTypeToType Void _ = VoidType
simpleTypeToType SignedLongLongInt _ = IntegerType 64
simpleTypeToType Char _ = IntegerType 8
simpleTypeToType UnsignedLongInt _ = IntegerType 64
simpleTypeToType t qs = trace ("Unimplemented simple type: " ++ (show t)) undefined

{- Hint about implementing unions:
-  01:10 < o11c> but union {int i; double d; }; -> struct { int i[0]; double d[0]; char _data[max(sizeof(int), sizeof(double))]; };
--}

complexTypeToType :: TypeDefinitions -> ComplexType -> TypeQualifiers -> Type
complexTypeToType defs t@(Struct _ decls _) _ = StructureType False (map ((qualifiedTypeToType defs).declarationType) decls)
complexTypeToType defs (TD name) _ = qualifiedTypeToType defs $ lookupType defs name
complexTypeToType defs (E e@(CEnum _ (Just entries) _ a)) _ = IntegerType 32
complexTypeToType _ t _ = trace ("Unimplemented complex type: " ++ (show t)) undefined

lookupType :: TypeDefinitions -> String -> QualifiedType
lookupType types name = case find (\ (n, typ) -> n == name) types of
	Just (name, typ) -> typ
	Nothing ->  errorWithStackTrace ("Referenced undeclared type: " ++ name ++ "\n in: " ++ (show types))