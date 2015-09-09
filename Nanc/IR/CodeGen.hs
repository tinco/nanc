module Nanc.IR.CodeGen where

import Debug.Trace
import Data.Maybe

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

		sortDeclaration tu decl 
			| isExtern && isFunction = tu { extFunDefs = (extFunDefs tu ) ++ [declaration] }
			| isExtern =  tu { extVariables = (extVariables tu )  ++ [declaration] }
			| isTypedef =  tu { typeDefs = (typeDefs tu )  ++ [declaration] }
			| isStatic =  tu { staticVariables = (staticVariables tu )  ++ [declaration] }
			| otherwise = trace ("got unknown toplevel decl: " ++ (show declaration)) undefined
			where
				declaration = globalDeclarationDefaults $ buildDeclaration decl
				storage = declStorage $ declarationSpecs declaration
				isExtern = storage == Extern
				isTypedef = storage == Typedef
				isStatic = storage == Static
				isFunction = isFunctionType $ declarationType declaration

-- TODO: why compile declarations we might never reference? We should
-- make this lazy. So a declaration only gets compiled when it's referenced
-- from main or is externally visible.
