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

{-
 A module consists of a list of external declarations.
-}
generate :: String -> CTranslUnit -> AST.Module
generate name (CTranslUnit decls _) = runModule (emptyModule name) (mapM_ generateExtDecl decls)

{-
External declarations can either be a toplevel declaration, a function definition or external assember
-}
generateExtDecl :: CExtDecl -> Module ()
generateExtDecl (CDeclExt decl) = generateToplevelDecl decl
generateExtDecl (CFDefExt decl) = generateFunDef decl
generateExtDecl (CAsmExt _decl _) = trace "ASM" $ undefined

{-
C99 requires that there is at least one specifier, though this is merely a syntactic restriction
at most one storage class specifier is allowed per declaration
the elements of the non-empty init-declarator-list are of the form (Just declr, init?, Nothing). The declarator declr has to be present and non-abstract and the initialization expression is optional.
-}
generateToplevelDecl :: CDecl -> Module ()
generateToplevelDecl decl@(CDecl specs _dclrs _)
	| isExtern = generateExtern declSpecs decl
	| isTypedef = generateTypedef declSpecs decl
	| isStatic = generateStaticDecl declSpecs decl
	| otherwise = trace ("got unknown toplevel decl: " ++ (show declSpecs) ++
		"Decl: " ++ (show decl)) undefined
	where
		declSpecs = globalDeclSpecDefaults $ buildDeclarationSpecs specs
		storage = declStorage declSpecs
		
		isExtern = storage == Extern
		isTypedef = storage == Typedef
		isStatic = storage == Static



generateExtern :: DeclarationSpecs -> CDecl -> Module ()
generateExtern _declSpecs _decl@(CDecl _ [(Just declr,Nothing,Nothing)] _) | printf = trace ("ExternDecl: " ++ name ++ " " ++ (show declr)) $ undefined
                                                                        | otherwise = return ()
	where
		name = extractDeclrName declr
		printf = name == "printf"

generateTypedef :: DeclarationSpecs -> CDecl -> Module ()
generateTypedef _declSpecs _decl@(CDecl _ [(Just declr,_,_)] _) = trace ("Typedef: " ++ name) $ return ()
	where
		name = extractDeclrName declr
generateStaticDecl :: DeclarationSpecs -> CDecl -> Module ()
generateStaticDecl _declSpecs _decl@(CDecl _ [(Just declr,_,_)] _) = trace ("StaticDecl: " ++ name) $ return ()
	where
		name = extractDeclrName declr

generateStaticDecl _declSpecs _decl@(CDecl _ _ _) = trace ("StaticDecl: " ++ "no name") $ return ()

generateFunDef :: CFunDef -> Module ()
generateFunDef (CFunDef specs declr _decls stat _) = define tp name fnargs bls
	where
		declSpecs = buildDeclarationSpecs specs
		tp = generateType $ declType declSpecs
		name = extractDeclrName declr
		_args = []
		fnargs = []
		bls = createBlocks $ execCodegen $ do
			entryB <- addBlock entryBlockName
			setBlock entryB
			-- generate argument code here
			generateStatement stat

extractDeclrName :: CDeclr -> String
extractDeclrName (CDeclr ident _ _ _ _)= maybe "anonymous" (\ (Ident n _ _) -> n) ident