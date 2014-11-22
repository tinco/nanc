module Nanc.IR.CodeGen where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import LLVM.General.AST hiding (Module)
import qualified LLVM.General.AST as AST

import Nanc.AST
import Nanc.CodeGenState
import Nanc.IR.Instructions

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
generateExtDecl (CAsmExt decl _) = trace "ASM" $ undefined


{-
C99 requires that there is at least one specifier, though this is merely a syntactic restriction
at most one storage class specifier is allowed per declaration
the elements of the non-empty init-declarator-list are of the form (Just declr, init?, Nothing). The declarator declr has to be present and non-abstract and the initialization expression is optional.
-}
generateToplevelDecl :: CDecl -> Module ()
generateToplevelDecl decl@(CDecl specs dclrs _)
	| hasExternSpec = generateExtern decl
	| hasTypedefSpec = generateTypedef decl
	| hasTypeSpec = generateTypeSpec decl
	| otherwise = trace ("got unknown toplevel decl: " ++ (show decl)) undefined
	where
		hasExternSpec = any isExtern specs
		isExtern (CStorageSpec (CExtern _)) = True
		isExtern _ = False

		hasTypedefSpec = any isTypedef specs
		isTypedef (CStorageSpec (CTypedef _)) = True
		isTypedef _ = False

		hasTypeSpec = any isTypeSpec specs
		isTypeSpec (CTypeSpec _) = True
		isTypeSpec _ = False	

generateExtern :: CDecl -> Module ()
generateExtern decl@(CDecl specs [(Just declr,_,_)] _) = external tp name fnargs
	where
		fnargs = []
		tp = extractReturnType specs
		name = extractDeclrName declr

generateTypedef :: CDecl -> Module ()
generateTypedef decl = trace "Don't know how to generate typedefs yet" $ return ()

generateTypeSpec :: CDecl -> Module ()
generateTypeSpec decl = trace "Don't know how to generate typeSpecs yet" $ return ()

generateFunDef :: CFunDef -> Module ()
generateFunDef (CFunDef specifiers declr decls stat _) = define tp name fnargs bls
	where
		tp = extractReturnType specifiers
		name = extractDeclrName declr
		args = []
		fnargs = []
		bls = createBlocks $ execCodegen $ do
			entry <- addBlock entryBlockName
			setBlock entry
			-- generate argument code here
			generateStat stat

generateStat :: CStat -> Codegen ()
generateStat (CExpr expr _) = generateExpr (fromJust expr)
generateStat (CReturn _expr _) = trace "Return" $ undefined
generateStat (CCompound _ident items _) = mapM_ generateBlockItem items
generateStat _d = undefined

generateExpr :: CExpr -> Codegen ()
generateExpr (CCall fn' args' _) = do
	args <- mapM generateExpr args'
	fn <- generateExpr fn'
	-- call fn args
	return ()

generateExpr expr = trace ("encountered expr: " ++ (show expr)) undefined

generateBlockItem :: CBlockItem -> Codegen ()
generateBlockItem (CBlockStmt stat) = generateStat stat
generateBlockItem _ = undefined

extractReturnType :: [CDeclSpec] -> Type
extractReturnType [] = VoidType
extractReturnType (x:[]) = trace ("encountered returntype: " ++ (show x)) VoidType
extractReturnType xs = trace ("encountered returntypes: " ++ (show xs)) VoidType

extractDeclrName :: CDeclr -> String
extractDeclrName (CDeclr ident _ _ _ _)= maybe "anonymous" (\ (Ident n _ _) -> n) ident

