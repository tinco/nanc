module Nanc.AST where

import Language.C

import Debug.Trace

data StorageSpec = Auto | Register | Static | Extern | Typedef | Thread | NoStorageSpec
	deriving (Show)

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

data ComplexType = CSU CStructUnion | E CEnum | TD Ident | TOE CExpr | TOT CDecl deriving (Show)

data TypeSpec = CT ComplexType | ST SimpleType | NoTypeSpec deriving (Show)

isNoTypeSpec :: TypeSpec -> Bool
isNoTypeSpec NoTypeSpec = True
isNoTypeSpec _ = False

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
	declAttributes :: [CAttrQual],
	declStorageNodes :: [NodeInfo],
	declTypeNodes :: [NodeInfo],
	declQualifierNodes :: [NodeInfo]
}

defaultDeclarationSpec :: DeclarationSpecs
defaultDeclarationSpec = DeclarationSpecs NoStorageSpec NoTypeSpec defaultTypeQualifiers [] [] [] [] []

buildDeclarationSpecs :: [CDeclSpec] -> DeclarationSpecs
buildDeclarationSpecs specs = build defaultDeclarationSpec specs
	where
		build ds [] = ds
		build ds ((CStorageSpec ss):rest) = build (updateStorageSpec ds ss) rest
		build ds ((CTypeSpec spec):rest) = build (updateTypeSpec ds spec rest) rest
		build ds quals@((CTypeQual qual):_) = build (updateTypeQual ds quals)

		updateStorageSpec ds ss = ds { 
			declStorage = declStorage',
			declStorageNodes = n' : (declStorageNodes ds) 
		}
			where
			(declStorage', n') = parse ss
				where
					parse (CAuto n) = (Auto,n)
					parse (CRegister n) = (Register,n)
					parse (CStatic n) = (Static,n)
					parse (CExtern n) = (Extern,n)
					parse (CTypedef n) = (Typedef,n)
					parse (CThread n) = (Thread,n)

		updateTypeSpec :: DeclarationSpecs -> CTypeSpec -> [CDeclSpec] -> DeclarationSpecs
		updateTypeSpec ds spec rest
			| hasSpec = ds
			| otherwise = ds { 
				declType = declType',
				declTypeNodes = n' : (declTypeNodes ds) 
			}
			where
				tspecs = spec : (filterTypeSpecs rest)
				filterTypeSpecs :: [CDeclSpec] -> [CTypeSpec]
				filterTypeSpecs [] = []
				filterTypeSpecs ((CTypeSpec ts):rs) = ts : filterTypeSpecs rs
				filterTypeSpecs _ = []
				hasSpec = isNoTypeSpec $ declType ds
				(declType', n') = parse spec
				-- we willen weten of er een int is in te typespec
				-- als die er is dan willen we weten of er ook een
				-- long, signed, of int is.
				--
				parse :: CTypeSpec -> (TypeSpec, NodeInfo)
				parse (CVoidType n) = (ST Void, n)
				parse (CFloatType n) = (ST Float, n)
				parse (CBoolType n) = (ST Bool, n)
				parse (CComplexType n) = trace ("What the hell is a ComplexType? " ++ (show n)) undefined
				parse (CSUType u n) = (CT (CSU u), n)
				parse (CEnumType e n) = (CT (E e), n)
				parse (CTypeDef i n) = (CT (TD i), n)
				parse (CTypeOfExpr e n) = (CT (TOE e), n)
				parse (CTypeOfType d n) = (CT (TOT d), n)
				parse (CCharType n)
					| hasSigned = (ST SignedChar, n)
					| hasUnsigned = (ST UnsignedChar, n)
					| otherwise = (ST Char, n)
				parse (CDoubleType n)
					| hasLong = (ST LongDouble, n)
					| otherwise = (ST Double, n)
				parse (CIntType n)
					| hasTwoLong && hasUnsigned = (ST UnsignedLongLongInt, n)
					| hasTwoLong = (ST SignedLongLongInt, n)
					| hasLong && hasUnsigned = (ST UnsignedLongInt, n)
					| hasLong = (ST SignedLongLongInt, n)
					| hasShort && hasUnsigned = (ST UnsignedShortInt, n)
					| hasShort = (ST SignedShortInt, n)
					| hasUnsigned = (ST UnsignedInt, n)
					| otherwise = (ST SignedInt, n)

				parse (CShortType n) = parse (CIntType n)
				parse (CLongType n) = parse (CIntType n)
				parse (CSignedType n) = parse (CIntType n)
				parse (CUnsigType n) = parse (CIntType n)


				hasSigned = any isSignedT tspecs
				isSignedT (CSignedType _) = True
				isSignedT _ = False
				hasUnsigned = any isUnsignedT tspecs
				isUnsignedT (CUnsigType _) = True
				isUnsignedT _ = False
				hasLong = any isLongT tspecs
				isLongT (CLongType _) = True
				isLongT _ = False
				hasTwoLong = any isLongT $ drop 1 $ snd $ break isLongT tspecs
				isShortT (CShortType _) = True
				isShortT _ = False
				hasShort = any isShortT tspecs

		-- This function is going to perform horribly, I'm too tired to think
		-- of how to do it smarter:
		updateTypeQual ds quals = ds {
			declQualifiers = TypeQualifiers hasVolatile hasConst hasRestrict hasInline,
			declQualifierNodes = map toNode typeQuals
			declAttributes = attrQuals
		}
		where
			typeQuals = filter isTypeQual quals
			attrQuals = filter (not.isTypeQual) quals

			hasVolatile = any isVolatileQ typeQuals
			hasConst = any isConstQ typeQuals
			hasRestrict = any isRestrictQ typeQuals
			hasInline = any isInlineQ typeQuals

			isTypeQual (CAttrQual _) = False
			isTypeQual _ = True

			isVolatileQ (CVolatQual _) = True
			isVolatileQ _ = False
			isRestrictQ (CRestrQual _) = True
			isRestrictQ _ = False
			isConstQ (CConstQual _) = True
			isConstQ _ = False
			isInlineQ (CInlineQual _) = True
			isInlineQ _ = False

			toNode (CVolatQual n) = n
			toNode (CRestrQual n) = n
			toNode (CConstQual n) = n
			toNode (CInlineQual n) = n
