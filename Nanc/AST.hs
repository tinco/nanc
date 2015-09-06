module Nanc.AST where

import Data.Maybe
import Data.Word

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

data ComplexType = Struct !(Maybe String) ![Declaration] ![CAttr] | Union ![Declaration] ![CAttr] | E !CEnum | TD !String | TOE !CExpr | TOT !CDecl deriving (Show)

data FunctionType = FunctionType !QualifiedType ![(QualifiedType, String)] deriving (Show)

data TypeSpec = Ptr !QualifiedType | CT !ComplexType | ST !SimpleType | FT !FunctionType | Arr !Word64 !QualifiedType | NoTypeSpec | TypeType deriving (Show)

data QualifiedType = QualifiedType !TypeSpec !TypeQualifiers deriving (Show)

data Signedness = Signed | Unsigned

returnType :: QualifiedType -> QualifiedType
returnType (QualifiedType (FT (FunctionType t _)) _) = t

isNoTypeSpec :: QualifiedType -> Bool
isNoTypeSpec (QualifiedType NoTypeSpec _) = True
isNoTypeSpec _ = False

isFunctionType :: QualifiedType -> Bool
isFunctionType (QualifiedType (FT _) _) = True
isFunctionType _ = False

isSigned :: QualifiedType -> Bool
isSigned (QualifiedType (ST c) _) = isSigned' c
	where
		isSigned' Char = True
		isSigned' SignedChar = True
		isSigned' SignedShortInt = True
		isSigned' SignedInt = True
		isSigned' SignedLongInt = True
		isSigned' SignedLongLongInt = True
		isSigned' Float = True
		isSigned' Double = True
		isSigned' LongDouble = True
		isSigned' _ = False
isSigned _ = False

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

defaultBooleanType = QualifiedType (ST Bool) defaultTypeQualifiers

constTypeQualifiers :: TypeQualifiers
constTypeQualifiers = defaultTypeQualifiers { typeIsConst = True }

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

