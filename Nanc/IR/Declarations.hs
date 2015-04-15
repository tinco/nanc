module Nanc.IR.Declarations where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as Global

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Statement
import Nanc.IR.Expression

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
generateToplevelDecl decl
	| isExtern = generateExtern declaration
	| isTypedef = generateTypedef declaration
	| isStatic = generateStaticDecl declaration
	| otherwise = trace ("got unknown toplevel decl: " ++ (show declaration)) undefined
	where
		declaration = globalDeclarationDefaults $ buildDeclaration decl
		storage = declStorage $ declarationSpecs declaration
		isExtern = storage == Extern
		isTypedef = storage == Typedef
		isStatic = storage == Static

generateExtern :: Declaration -> Module ()
generateExtern declaration =
	trace ("ExternDecl: " ++ name) $ return ()
	where
		name = declarationName declaration

generateTypedef :: Declaration -> Module ()
generateTypedef declaration =
	trace ("Typedef: " ++ name) $ return ()
	where
		name = declarationName declaration

-- we really need to transform this whole declarationspec hell to a neat ast.
generateStaticDecl :: Declaration -> Module ()
generateStaticDecl decl = addDefn def
	where
		def = trace ("Making a global var: " ++ (show $ declarationName decl)) $ AST.GlobalDefinition $ AST.globalVariableDefaults {
			Global.name = AST.Name $ declarationName decl,
			Global.type' = qualifiedTypeToType $ declType $ declarationSpecs decl
		}

generateFunDef :: CFunDef -> Module ()
generateFunDef (CFunDef specs declr _decls stat _) =
	define tp name fnargs bls
	where
		declSpecs = buildDeclarationSpecs specs
		tp = qualifiedTypeToType $ declType declSpecs
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