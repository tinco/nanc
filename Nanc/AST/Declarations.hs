module Nanc.AST.Declarations where

import Debug.Trace
import Data.Maybe
import Control.Applicative

import Language.C
import Language.C.Data.Ident

import Nanc.AST

{-
 - Helper functions for transforming the Language.C Declarations to Nanc.AST Declaratoins.
 -}

buildDeclaration :: CDecl -> Declaration
buildDeclaration (CDecl specs [(Just (CDeclr maybeName derivedDeclarators _asmName attrs _),_,_)] _) =
	Declaration {
		declarationName = name,
		declarationSpecs = ds,
		declarationType = buildDerivedType (declType ds) derivedDeclarators 
	}
	where
		ds = buildDeclarationSpecs specs
		name = fromMaybe "AnonymousDeclaration" (identToString <$> maybeName) 

{- CDecl [CTypeSpec (CTypeDef (Ident "size_t" 213839698))] [] -}
-- This is a declaration with just a typespecifier and no extra information
buildDeclaration decl@(CDecl specs [] _) =
	Declaration {
		declarationName = name,
		declarationSpecs = ds,
		declarationType = declType ds
	}
	where
		ds = buildDeclarationSpecs specs
		name = head $ catMaybes [structName (declType ds), Just "AnonymousDeclaration"]
		structName (QualifiedType (CT (CSU (CStruct CStructTag (Just (Ident name _ _)) _ _ _) _)) _) = Just name
		structName _ = Nothing

buildDeclaration decl = trace ("Weird declaration: " ++ (show $ decl)) undefined
	
buildDerivedType :: QualifiedType -> [CDerivedDeclr] -> QualifiedType
buildDerivedType qt ddrs = buildDerivedType' qt (reverse ddrs)
	where
		buildDerivedType' qt [] = qt
		buildDerivedType' qt ((CPtrDeclr qs _):ddrs) = buildDerivedType' (QualifiedType (Ptr qt) (fst $ buildTypeQualifiers qs)) ddrs
		buildDerivedType' qt (funDecl@(CFunDeclr _ _ _):ddrs) = buildDerivedType' (buildFunType qt funDecl) ddrs
		buildDerivedType' qt (arrDecl@(CArrDeclr _ _ _):ddrs) = buildDerivedType' (buildArrType qt arrDecl) ddrs

		buildFunType qt (CFunDeclr (Left _) _ _) = trace ("Old style Function declarator: " ++ show ddrs) undefined
		buildFunType qt (CFunDeclr (Right (decls, _mysteriousBool)) _ _) = QualifiedType (FT (FunctionType qt (map (declarationType.buildDeclaration) decls))) defaultTypeQualifiers

		buildArrType qt (CArrDeclr qs _ _) = trace "ArrDeclr not implemented" (QualifiedType (Arr 0 qt) (fst $ buildTypeQualifiers qs))

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
		updateTypeQual ds quals =
			ds {
				declType = QualifiedType (qualifiedTypeType $ declType ds) typeQualifiers,
				declQualifierNodes = qNodes
				--,declAttributes = map (\ (CAttrQual a) -> a) attrQuals
			}
			where
				(typeQualifiers, qNodes) = buildTypeQualifiers quals

buildTypeQualifiers :: [CTypeQual] -> (TypeQualifiers, [NodeInfo])
buildTypeQualifiers quals = 
	(TypeQualifiers hasVolatile hasConst hasRestrict hasInline, map toNode typeQuals)
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

