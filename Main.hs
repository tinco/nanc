module Main where

import Control.Monad.Except

import Language.C
import Language.C.System.GCC

import Nanc.IR.CodeGen

import LLVM.General.Context
import LLVM.General.Module

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

main :: IO ()
main = do
	parsed <- parseMyFile "test.c"
	printMyAST parsed

	let ast = generate "test" parsed

	withContext $ \x -> liftError $ withModuleFromAST x ast $ \m -> do
		ir <- moduleLLVMAssembly m
		putStrLn ir

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast -> return ast

printMyAST :: CTranslUnit -> IO ()
printMyAST = (print.pretty)