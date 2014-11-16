module Main where

import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC

import Nanc.CodeGen

main :: IO ()
main = parseMyFile "test.c" >>= printMyAST

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast -> return ast

printMyAST :: CTranslUnit -> IO ()
printMyAST (CTranslUnit decls _) = mapM_ printExtDecl decls

printExtDecl :: CExtDecl -> IO ()
printExtDecl (CDeclExt decl) = print $ "CDeclExt: " ++ (show $ pretty decl)
printExtDecl (CFDefExt decl) = printFunDef decl
printExtDecl (CAsmExt decl _) = print $ "ASM: " ++ (show $ pretty decl)

printFunDef :: CFunDef -> IO ()
printFunDef (CFunDef declspecs declr decls stat _) = do
	print "Omg a function"
	mapM_ printDeclSpec declspecs
	printDeclR declr
	mapM_ printDecl decls
	printStat stat

printDeclSpec :: CDeclSpec -> IO ()
printDeclSpec _ = print "Omg a declspec"

printDeclR :: CDeclr -> IO ()
printDeclR (CDeclr ident _derdeclrs _strlit _attrs _) = do
	let ident' = maybe "anonymous" (\ (Ident n _ _) -> n) ident
	print $ "Function identity: " ++ ident'

printDecl :: CDecl -> IO ()
printDecl _ = print "Omg a declaration"

printStat :: CStat -> IO ()
printStat (CExpr _expr _) = print "Omg a expression statement"
printStat (CReturn _expr _) = print "Omg a return statement"
printStat (CCompound _ident items _) = mapM_ printBlockItem items
printStat _d = print "Omg a non-expression statement"

printBlockItem :: CBlockItem -> IO ()
printBlockItem (CBlockStmt stat) = printStat stat
printBlockItem _ = print "Omg a non-statement block item"