module Nanc.AST where

import Language.C

data StorageSpec = Auto | Register | Static | Extern | Typedef | Thread | NoStorageSpec

data SimpleType = Int |
	LongSignedInt | LongUnsignedInt |
	ShortSignedInt | ShortUnsignedInt |
	LongDouble | Double |
	SignedChar | Char |
	Void | Float

data ComplexType = CSU CStructUnion | E CEnum | TD Ident 

data TypeSpec = CT ComplexType | ST SimpleType

data TypeQualifiers = TypeQualifiers {
	typeIsVolatile :: Bool,
	typeIsConst :: Bool,
	typeIsRestrict :: Bool,
	typeIsInline :: Bool
}

defaultTypeQualifiers :: TypeQualifiers
defaultTypeQualifiers = TypeQualifiers False False False False

data DeclarationSpecs = DeclarationSpecs {
	declStorage :: StorageSpec,
	declType :: TypeSpec,
	declQualifiers :: TypeQualifiers,
	declAttributes :: [CAttr],
	declStorageNodes :: [NodeInfo],
	declTypeNodes :: [NodeInfo],
	declQualifierNodes :: [NodeInfo],
	declAttributeNodes :: [NodeInfo]
}

defaultDeclarationSpec :: DeclarationSpecs
defaultDeclarationSpec = DeclarationSpecs NoStorageSpec (ST Int) defaultTypeQualifiers [] [] [] [] []

buildDeclarationSpecs :: [CDeclSpec] -> DeclarationSpecs
buildDeclarationSpecs specs = build defaultDeclarationSpec specs
	where
		build ds [] = ds
		build ds ((CStorageSpec ss):rest) = build (updateStorageSpec ds ss) rest
		build ds ((CTypeSpec spec):rest) = build (updateTypeSpec ds spec rest) rest
		build ds ((CTypeQual qual):rest) = build (updateTypeQual ds qual) rest

		updateStorageSpec ds ss = ds { 
			declStorage = declStorage',
			declStorageNodes = n : (declStorageNodes ds) 
		}
			where
			(declStorage', n) = parse ss
			parse (CAuto n) = (Auto,n)
			parse (CRegister n) = (Register,n)
			parse (CStatic n) = (Static,n)
			parse (CExtern n) = (Extern,n)
			parse (CTypedef n) = (Typedef,n)
			parse (CThread n) = (Thread,n)

		updateTypeSpec ds spec rest = ds { 
			declType = declType',
			declTypeNodes = n : (declTypeNodes ds) 
		}
			where
				(declType', n) = undefined

		updateTypeQual ds qual = undefined