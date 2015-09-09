module Nanc.IR.Types where

import Language.C.Data.Ident
import Language.C.Syntax
import GHC.Stack

import LLVM.General.AST as LLVM hiding (Module)
import LLVM.General.AST.AddrSpace

import Nanc.AST
import Data.List
import Data.Word

import Debug.Trace

type TypeDefinitions = [(String, QualifiedType)]

booleanType = LLVM.IntegerType 1

qualifiedTypeToType :: TypeDefinitions -> QualifiedType -> Type
qualifiedTypeToType _ (QualifiedType (ST t) qs) = simpleTypeToType t qs
qualifiedTypeToType m (QualifiedType (CT t) qs) = complexTypeToType m t qs
qualifiedTypeToType m (QualifiedType (Ptr (QualifiedType (ST Void) _)) _) = PointerType (IntegerType 8) (AddrSpace 0)
qualifiedTypeToType m (QualifiedType (Ptr qt@(QualifiedType (FT _) _)) qs) = trace "Dodgy function pointer" $ PointerType (IntegerType 64) (AddrSpace 0)
qualifiedTypeToType m (QualifiedType (Ptr qt) qs) = PointerType (qualifiedTypeToType m qt) (AddrSpace 0)
qualifiedTypeToType _ (QualifiedType NoTypeSpec _) = IntegerType 32
qualifiedTypeToType m (QualifiedType (Arr l qt) qs) = ArrayType l (qualifiedTypeToType m qt)
qualifiedTypeToType m (QualifiedType (FT ft) qs) = functionTypeToType m ft qs
qualifiedTypeToType m (QualifiedType (TypeAlias n) qs) = NamedTypeReference (Name n)
qualifiedTypeToType _ qt = trace ("Unimplemented type: " ++ show qt) undefined

simpleTypeToType :: SimpleType -> TypeQualifiers -> Type
simpleTypeToType SignedInt _ = IntegerType 32
simpleTypeToType Void _ = VoidType
simpleTypeToType SignedLongLongInt _ = IntegerType 64
simpleTypeToType SignedShortInt _ = IntegerType 16
simpleTypeToType Char _ = IntegerType 8
simpleTypeToType UnsignedLongInt _ = IntegerType 64
simpleTypeToType UnsignedInt _ = IntegerType 32
simpleTypeToType UnsignedShortInt _ = IntegerType 16
simpleTypeToType t qs = trace ("Unimplemented simple type: " ++ (show t)) undefined

{- Hint about implementing unions:
-  01:10 < o11c> but union {int i; double d; }; -> struct { int i[0]; double d[0]; char _data[max(sizeof(int), sizeof(double))]; };
--}

complexTypeToType :: TypeDefinitions -> ComplexType -> TypeQualifiers -> Type
complexTypeToType defs t@(Struct _ decls _) _ = StructureType False (map ((qualifiedTypeToType defs).declarationType) decls)
complexTypeToType defs (TD name) _ = qualifiedTypeToType defs $ lookupType defs name
complexTypeToType defs (E e@(CEnum _ (Just entries) _ a)) _ = IntegerType 32
complexTypeToType defs (Union decls _) _ = StructureType False (arrs ++ [datafield])
	where
		types = map ((qualifiedTypeToType defs).declarationType) decls
		arrs =  map (ArrayType 0) types
		maxSize = maximum $ map sizeof types
		datafield = ArrayType maxSize (IntegerType 8)
complexTypeToType _ t _ = trace ("Unimplemented complex type: " ++ (show t)) undefined

-- TODO Support varargs!
functionTypeToType defs (Nanc.AST.FunctionType rt args) _ = LLVM.FunctionType (qualifiedTypeToType defs rt) (argTypes) False
	where
		argTypes = map ((qualifiedTypeToType defs).fst) args

lookupType :: TypeDefinitions -> String -> QualifiedType
lookupType types name = case find (\ (n, typ) -> n == name) types of
	Just (name, typ) -> typ
	Nothing ->  errorWithStackTrace ("Referenced undeclared type: " ++ name ++ "\n in: " ++ (show types))

sizeof :: Type -> Word64
sizeof (IntegerType i) = ( (fromIntegral i) - 1) `quot` 8 + 1
sizeof VoidType = 0
sizeof (PointerType _ _) = 8 -- or 4?
sizeof (FloatingPointType i _) = ((fromIntegral i) - 1) `quot` 8 + 1
sizeof (LLVM.FunctionType _ _ _) = trace "Functions don't have a size." undefined
sizeof (VectorType n t) = (fromIntegral n) * (sizeof t)
sizeof (StructureType False ts) = sum $ map sizeof ts
sizeof (StructureType True _) = trace "Don't know how to calculate size of packed struct." undefined
sizeof (ArrayType n t) = (fromIntegral n) * (sizeof t)
sizeof t = trace ("Can't determine sizeof unknown type " ++ (show t)) undefined