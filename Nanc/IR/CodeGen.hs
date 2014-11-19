module Nanc.IR.CodeGen where

import Debug.Trace
import Data.Maybe

import Language.C
import Language.C.Data.Ident

import LLVM.General.AST hiding (Module)
import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.IR.Instructions

generate :: String -> CTranslUnit -> AST.Module
generate name (CTranslUnit decls _) = runModule (emptyModule name) (mapM_ generateExtDecl decls)

generateExtDecl :: CExtDecl -> Module ()
generateExtDecl (CDeclExt decl) = generateDeclExt decl
generateExtDecl (CFDefExt decl) = generateFunDef decl
generateExtDecl (CAsmExt decl _) = trace "ASM" $ undefined

generateDeclExt :: CDecl -> Module ()
generateDeclExt (CDecl specs ((Just declr, _, _):[]) _) = external tp name fnargs
	where
		fnargs = []
		tp = extractReturnType specs
		name = extractDeclrName declr
generateDeclExt (CDecl specs [] _) = trace ("empty extdecl: " ++ (show $ specs) ++ "\n") $ return () 

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

