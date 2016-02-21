module Main where

import System.Environment
import System.FilePath

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans.Except

import Language.C
import Language.C.System.Preprocess
import Language.C.System.GCC

import Nanc.IR.CodeGen
import Nanc.Compiler

import LLVM.General.Context
import LLVM.General.Module

main :: IO ()
main = do
	args <- getArgs

	let filename = case (length args) of
		0 -> "test.c"
		_ -> head args

	compile filename (dropExtension filename)
