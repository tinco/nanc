module Nanc.Compiler where

import System.Environment
import System.IO
import System.Process
import System.FilePath
import System.Exit

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
import qualified LLVM.General.Target as LLT

import System.IO.Temp
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as TIO

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

compile :: FilePath -> FilePath -> IO (ExitCode)
compile i o = do
	src <- TIO.readFile i
	preprocessed <- preprocess i src
	parsed <- parse' i preprocessed
	let transformed = transformToLLVM parsed
	object <- generateObject transformed
	let objectName = addExtension o ".o"
	BS.writeFile objectName object
	linkIntoExecutable objectName o

transformToLLVM :: CTranslUnit -> LLA.Module
transformToLLVM parsed = generate "UNNAMED_MODULE" parsed

generateIR :: LLA.Module -> IO T.Text
generateIR ast = do
	ir <- LLC.withContext $ \ctx -> liftError $ LLM.withModuleFromAST ctx ast $ \m -> LLM.moduleLLVMAssembly m
	return $ T.pack ir

generateBC :: LLA.Module -> IO BS.ByteString
generateBC ast = do
	LLC.withContext $ \ctx -> liftError $ LLM.withModuleFromAST ctx ast $ \m -> LLM.moduleBitcode m

generateObject :: LLA.Module -> IO BS.ByteString
generateObject ast = do
	liftError $ LLT.withHostTargetMachine $ \tgm -> do
		LLC.withContext $ \ctx ->
			liftError $ LLM.withModuleFromAST ctx ast $ \m ->
				liftError $ LLM.moduleObject tgm m

parse :: FilePath -> T.Text -> Either ParseError CTranslUnit
parse f c = parseC (encodeUtf8 c) (initPos f)

parse' :: FilePath -> T.Text -> IO CTranslUnit
parse' f c = case parse f c of
		Left parseErr -> error ("Parse error: "++ (show parseErr))
		Right ast -> return ast

linkIntoExecutable :: FilePath -> FilePath -> IO (ExitCode)
linkIntoExecutable obj o = do
	rawSystem "gcc" ["-o", o, obj]

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
