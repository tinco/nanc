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

	putStrLn "Going to generate ast"
	let ast = generate "test" parsed
	putStrLn $ "Generated external declarations: " ++ (show ast)

	ir <- withContext $ \x -> liftError $ withModuleFromAST x ast $ \m -> moduleLLVMAssembly m

	writeFile "test.ir" ir

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	--preprocessed_file <- preprocess input_file
	parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file --preprocessed_file
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
