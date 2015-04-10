module Nanc.AST.Declarations where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import Nanc.AST

{-
 - Helper functions for transforming the Language.C Declarations to Nanc.AST Declaratoins.
 -}

buildDeclaration :: CDecl -> Declaration
buildDeclaration (CDecl specs [(Just (CDeclr (Just (Ident name _ _)) derivedDeclarators _asmName attrs _),_,_)] _) =
	Declaration {
		declarationName = name,
		declarationSpecs = ds,
		declarationType = buildDerivedType (declType ds) derivedDeclarators 
	}
	where
		ds = buildDeclarationSpecs specs

buildDeclaration (CDecl specs [(Just (CDeclr (Nothing) _derivDeclrs _asmName attrs _),_,_)] _) =
	trace "No name declaration" undefined

-- Catch some other weird declarations here
buildDeclaration decl@(CDecl specs declrs _)
	| isStructDefinition dType = Declaration {
		declarationName = structName dType,
		declarationSpecs = declSpecs,
		declarationType = QualifiedType TypeType defaultTypeQualifiers 
	}
	| otherwise = trace ("Weird declaration: " ++ (show $ specs)) undefined
	where
		isStructDefinition (QualifiedType (CT (CSU (CStruct CStructTag (Just (Ident _ _ _)) _ _ _ ) _)) _) = True
		isStructDefinition _ = False
		structName (QualifiedType (CT (CSU (CStruct CStructTag (Just (Ident name _ _)) _ _ _ ) _)) _) = name
		declSpecs = buildDeclarationSpecs specs
		dType = declType declSpecs

buildDerivedType :: QualifiedType -> [CDerivedDeclr] -> QualifiedType
buildDerivedType qt ddrs = buildDerivedType' qt (reverse ddrs)
	where
		buildDerivedType' qt [] = qt
		buildDerivedType' qt ((CPtrDeclr qs _):ddrs) = undefined

-- TODO: The next step here is to extract the typequalifier parser from the declspec builder
-- so we can invoke it here to extract the type qualifiers.

{-
  CPtrDeclr [CTypeQualifier a] a	= Pointer declarator CPtrDeclr tyquals declr
  CArrDeclr [CTypeQualifier a] (CArraySize a) a = Array declarator CArrDeclr declr tyquals size-expr?
  CFunDeclr (Either [Ident] ([CDeclaration a], Bool)) [CAttribute a] a = Function declarator CFunDeclr declr (old-style-params | new-style-params) c-attrs
-}

emptyDeclarationSpec :: DeclarationSpecs
emptyDeclarationSpec = DeclarationSpecs NoStorageSpec (QualifiedType NoTypeSpec defaultTypeQualifiers) [] [] []

globalDeclarationDefaults :: Declaration -> Declaration
globalDeclarationDefaults declaration = declaration {
		declarationSpecs = ds {
			declStorage = declStorage'
		}
	}
	where
		ds = declarationSpecs declaration
		declStorage'
		  -- TODO: are there other storage types for functions? if so this is wrong
		  | isFunctionType $ declarationType declaration = Extern 
		  | declStorage ds == NoStorageSpec = Static
		  | otherwise = declStorage ds

buildDeclarationSpecs :: [CDeclSpec] -> DeclarationSpecs
buildDeclarationSpecs specs = build emptyDeclarationSpec specs
	where
		build ds [] = ds
		build ds ((CStorageSpec ss):rest) = build (updateStorageSpec ds ss) rest
		build ds ((CTypeSpec spec):rest) = build (updateTypeSpec ds spec rest) rest
		build ds quals@((CTypeQual _):_) = updateTypeQual ds typeQuals
			where
				typeQuals = catMaybes $ map extractTypeQual quals
				extractTypeQual (CTypeQual tq) = Just tq
				extractTypeQual _ = Nothing

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
				declType = (QualifiedType declType' defaultTypeQualifiers),
				declTypeNodes = n' : (declTypeNodes ds) 
			}
			where
				tspecs = spec : (filterTypeSpecs rest)
				filterTypeSpecs :: [CDeclSpec] -> [CTypeSpec]
				filterTypeSpecs [] = []
				filterTypeSpecs ((CTypeSpec ts):rs) = ts : filterTypeSpecs rs
				filterTypeSpecs _ = []
				hasSpec = not $ isNoTypeSpec $ declType ds
				(declType', n') = parse spec

				parse :: CTypeSpec -> (TypeSpec, NodeInfo)
				parse (CVoidType n) = (ST Void, n)
				parse (CFloatType n) = (ST Float, n)
				parse (CBoolType n) = (ST Bool, n)
				parse (CComplexType n) = trace ("What the hell is a ComplexType? " ++ (show n)) undefined
				parse (CSUType u n) = (CT (CSU u []), n)
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
		updateTypeQual :: DeclarationSpecs -> [CTypeQual] -> DeclarationSpecs
		updateTypeQual ds quals = ds {
			declType = QualifiedType (justType $ declType ds) (TypeQualifiers hasVolatile hasConst hasRestrict hasInline),
			declQualifierNodes = map toNode typeQuals
			--,declAttributes = map (\ (CAttrQual a) -> a) attrQuals
		}
			where
				justType (QualifiedType t q) = t
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

