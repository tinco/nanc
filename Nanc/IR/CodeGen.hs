module Nanc.IR.CodeGen where

import Debug.Trace
import Data.Maybe
import Data.List

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types
import Nanc.IR.Statement
import Nanc.IR.Expression
import Nanc.IR.Declarations
import Nanc.AST.Declarations

{-
 A module consists of a list of external declarations.
-}

data TranslUnit = TranslUnit {
	functionDefs :: [CFunDef],
	asmDefs :: [CStrLit],
	extFunDefs :: [Declaration],
	extVariables :: [Declaration],
	typeDefs :: [Declaration],
	staticVariables :: [Declaration]
}

emptyTranslUnit = TranslUnit [] [] [] [] [] []

generate :: String -> CTranslUnit -> AST.Module
generate name (CTranslUnit decls _) = llvmModuleState $ runModule (emptyModule name) $ do
		mapM generateTypedef (typeDefs tu)
		resolveTypeDefinitions
		mapM generateExternVariable (extVariables tu)
		mapM generateExternFunction (extFunDefs tu)
		mapM generateStaticVariable (staticVariables tu)
		-- mapM generateAsm (asmDefs tu)
		mapM generateFunDef (functionDefs tu)
	where
		tu = sortDeclarations decls


sortDeclarations :: [CExternalDeclaration NodeInfo] -> TranslUnit
sortDeclarations decls = sortDeclarations' emptyTranslUnit decls
	where
		sortDeclarations' tu [] = tu
		sortDeclarations' tu ((CFDefExt decl):rest) = sortDeclarations' (sortFunDef tu decl) rest
		sortDeclarations' tu ((CAsmExt decl _):rest) = sortDeclarations' (sortAsmDef tu decl) rest
		sortDeclarations' tu ((CDeclExt decl):rest) = sortDeclarations' (sortDeclaration tu decl) rest

		sortFunDef tu decl = tu {
			functionDefs = (functionDefs tu) ++ [decl]
		}

		sortAsmDef tu decl = tu {
			asmDefs = (asmDefs tu) ++ [decl]
		}

		-- sortDeclaration tu decl = trace ("Sorting declaration: " ++ (show decl)) $ sortDeclaration' tu decl
		-- 	where
		-- 		declaration = globalDeclarationDefaults $ buildDeclaration decl
		sortDeclaration tu decl 
			| isExtern && isFunction = tu { extFunDefs = (extFunDefs tu ) ++ [declaration] }
			| isExtern = tu { extVariables = (extVariables tu )  ++ [declaration] }
			| isTypedef = sortToTypes
			| isStatic && isStruct && hasVariableName = addStructTypedef $ sortToTypesAndVariables
			| isStatic && isStruct = addStructTypedef $ sortToTypes
			| isStatic = sortToStatics
			| otherwise = trace ("got unknown toplevel decl: " ++ (show declaration)) undefined
			where
--				declName
--					| isStruct = drop 7 (declarationName declaration')
--					| otherwise = declarationName declaration'
				declaration = globalDeclarationDefaults $ buildGlobalDeclaration decl
--				declaration = declaration' { declarationName = declName }
				storage = declStorage $ declarationSpecs declaration
				isExtern = storage == Extern
				isTypedef = storage == Typedef
				isStatic = storage == Static
				isFunction = isFunctionType $ declarationType declaration
				sortToStatics = tu { staticVariables = (staticVariables tu )  ++ [declaration] }
				sortToTypes = tu { typeDefs = (typeDefs tu )  ++ [declaration] }
				isStruct = "struct" `isPrefixOf` (declarationName declaration)

				addStructTypedef = id
				{-}
				-- HACK: This is crazy and might break stuff
				addStructTypedef tu' = tu' {
						typeDefs = (typeDefs tu') ++ [structTypeDef]
					}
					where
						structTypeDef = Declaration {
							declarationName = declarationName declaration',
							declarationType = QualifiedType (TypeAlias $ declarationName declaration) defaultTypeQualifiers,
							declarationSpecs = declarationSpecs declaration
						}
				-- -}

				-- TODO: 
				hasVariableName = False
				sortToTypesAndVariables = undefined

-- At the global declaration scope a single struct with no members is a typedef and not
-- a type alias.
buildGlobalDeclaration decl@(CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident n a b)) Nothing [] c) d)] [] e) =
	buildDeclaration updatedDecl
	where
		-- replace Nothing by (Just [])
		updatedDecl = CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident n a b)) (Just []) [] c) d)] [] e

buildGlobalDeclaration decl = buildDeclaration decl

-- TODO: why compile declarations we might never reference? We should
-- make this lazy. So a declaration only gets compiled when it's referenced
-- from main or is externally visible.
