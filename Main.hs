module Main where

import Control.Monad
import Control.Monad.Trans.Except

import Language.C
import Language.C.System.Preprocess
import Language.C.System.GCC

import Nanc.IR.CodeGen

import LLVM.General.Context
import LLVM.General.Module

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

main :: IO ()
main = do
	parsed <- parseMyFile "test.c"
	--printMyAST parsed

	let ast = generate "test" parsed

	withContext $ \x -> liftError $ withModuleFromAST x ast $ \m -> do
		ir <- moduleLLVMAssembly m
		putStrLn ir

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	preprocessed_file <- preprocess input_file
	parse_result <- parseCFile (newGCC "gcc") Nothing [] preprocessed_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast -> return ast

preprocess :: FilePath -> IO FilePath
preprocess input_file = do
		runPreprocessor gcc args
		return output_file
	where
		output_file = input_file ++ ".i"
		args = CppArgs [] [] Nothing input_file (Just $ output_file)
		gcc = newGCC "gcc"

printMyAST :: CTranslUnit -> IO ()
printMyAST = (print.pretty)
