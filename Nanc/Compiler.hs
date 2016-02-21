module Nanc.Compiler where

import System.Environment
import System.IO

import Control.Monad
import Control.Monad.Trans.Except

import Language.C
import Language.C.Parser
import Language.C.System.Preprocess
import Language.C.System.GCC

import Nanc.IR.CodeGen

import qualified LLVM.General.AST as LLA
import qualified LLVM.General.Context as LLC
import qualified LLVM.General.Module as LLM

import System.IO.Temp
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as TIO

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

compile :: FilePath -> FilePath -> IO ()
compile i o = do
	input <- TIO.readFile i
	preprocessed <- preprocess i input
	parsed <- parse' i preprocessed
	let transformed = transformToLLVM parsed
	ir <- generateIR transformed
	TIO.writeFile o ir

transformToLLVM :: CTranslUnit -> LLA.Module
transformToLLVM parsed = generate "UNNAMED_MODULE" parsed

generateIR :: LLA.Module -> IO T.Text
generateIR ast = do
	ir <- LLC.withContext $ \x -> liftError $ LLM.withModuleFromAST x ast $ \m -> LLM.moduleLLVMAssembly m
	return $ T.pack ir

parse :: FilePath -> T.Text -> Either ParseError CTranslUnit
parse f c = parseC (encodeUtf8 c) (initPos f)

parse' :: FilePath -> T.Text -> IO CTranslUnit
parse' f c = case parse f c of
		Left parseErr -> error ("Parse error: "++ (show parseErr))
		Right ast -> return ast

preprocess :: FilePath -> T.Text -> IO T.Text
preprocess f c = withSystemTempFile f preprocess'
	where
		gcc = newGCC "gcc"
		preprocess' tmp h = do
			TIO.hPutStr h c
			hClose h
			let args = CppArgs [] [] Nothing tmp Nothing
			processResult <- runPreprocessor gcc args
			case processResult of
				Left err -> error ("Preprocess error: " ++ (show err))
				Right stream -> return $ (T.replace (T.pack tmp) (T.pack f)) (decodeUtf8 stream)

printMyAST :: CTranslUnit -> IO ()
printMyAST = (print.pretty)
