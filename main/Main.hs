module Main where

import System.Environment
import System.FilePath
import System.Exit

import Nanc.Compiler


main :: IO ()
main = do
	args <- getArgs

	let filename = case (length args) of
		0 -> "test.c"
		_ -> head args

	exitCode <- compile filename (dropExtension filename)
	exitWith exitCode

