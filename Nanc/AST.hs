module Nanc.AST where

import Data.Maybe

import Language.C
import Language.C.Data.Ident

import Debug.Trace

data StorageSpec = Auto | Register | Static | Extern | Typedef | Thread | NoStorageSpec
	deriving (Show, Eq)

data SimpleType = 
	Char |
	SignedChar |
	UnsignedChar |
	SignedShortInt |
	UnsignedShortInt |
	SignedInt |
	UnsignedInt |
	SignedLongInt |
	UnsignedLongInt |
	SignedLongLongInt |
	UnsignedLongLongInt |
	Float |
	Double |
	LongDouble |
	Bool |
	Void
	deriving (Show)

data ComplexType = CSU CStructUnion [CAttr] | E CEnum | TD Ident | TOE CExpr | TOT CDecl deriving (Show)

data FunctionType = FunctionType QualifiedType [QualifiedType] deriving (Show)

data TypeSpec = Ptr QualifiedType | CT ComplexType | ST SimpleType | FT FunctionType | Arr Int QualifiedType | NoTypeSpec | TypeType deriving (Show)

data QualifiedType = QualifiedType TypeSpec TypeQualifiers deriving (Show)

isNoTypeSpec :: QualifiedType -> Bool
isNoTypeSpec (QualifiedType NoTypeSpec _) = True
isNoTypeSpec _ = False

isFunctionType :: QualifiedType -> Bool
isFunctionType (QualifiedType (FT _) _) = True
isFunctionType _ = False

qualifiedTypeType :: QualifiedType -> TypeSpec
qualifiedTypeType (QualifiedType t _) = t

data TypeQualifiers = TypeQualifiers {
	typeIsVolatile :: Bool,
	typeIsConst :: Bool,
	typeIsRestrict :: Bool,
	typeIsInline :: Bool
} deriving (Show)

defaultTypeQualifiers :: TypeQualifiers
defaultTypeQualifiers = TypeQualifiers False False False False

data DeclarationSpecs = DeclarationSpecs {
	declStorage :: StorageSpec,
	declType :: QualifiedType,
	declStorageNodes :: [NodeInfo],
	declTypeNodes :: [NodeInfo],
	declQualifierNodes :: [NodeInfo]
} deriving (Show)

data Declaration = Declaration {
	declarationName :: String,
	declarationSpecs :: DeclarationSpecs,
	declarationType :: QualifiedType
} deriving (Show)

