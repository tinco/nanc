import Test.Hspec

import Nanc.Compiler
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Either
import Data.List
import System.Directory

main :: IO ()
main = hspec $ do
	describe "Nanc.Compiler.parse" $ do
		it "Parses hello world" $ do
			let fileName = "test.c"
			src <- TIO.readFile fileName
			preprocessed <- preprocess fileName src
			parse fileName preprocessed `shouldSatisfy` isRight

	describe "Nanc.Compiler.transformToLLVM" $ do
		it "Transforms globals to llvm ast" $ do
			pendingWith "This doesn't do anything.."
			src <- testFile "0002-global1"
			let (Right ast) = parse "global" src
			let transformed = transformToLLVM ast
			True `shouldSatisfy` ( /= False )
	
	describe "Nanc.Compiler.generateIR" $ do
		it "Generates IR for sanity" $ do
			src <- testFile "0001-sanity"
			ir <- compileSrcToIR "0001-sanity.c" src
			ir `shouldSatisfy` (/= T.empty)

		it "Generates IR for global" $ do
			--	Assertion failed: (getOperand(0)->getType() 
			--	== cast<PointerType>(getOperand(1)->getType())->getElementType()
			--	  && "Ptr must be a pointer to Val type!")
			--	  , function AssertOK, file Instr
			src <- testFile "0002-global1"
			ir <- compileSrcToIR "0002-global2.c" src
			ir `shouldSatisfy` (/= T.empty)

		describe "Transforms all execute tests to llvm ast" $ do
			let runTest (name, src) = do
				it ("Transforms " ++ name) $ do
					let (Right ast) = parse "global" src
					let transformed = transformToLLVM ast
					ir <- generateIR transformed
					ir `shouldSatisfy` (/= T.empty)

			tests <- runIO $ testFiles "execute"
			mapM_ runTest tests

testFile :: FilePath -> IO T.Text
testFile name = TIO.readFile $ "spec/spec_support/execute/" ++ name ++ ".c"

testFiles :: FilePath -> IO [(FilePath, T.Text)] 
testFiles subdir = do
	let dir = "spec/spec_support/" ++ subdir ++ "/"
	files <- getDirectoryContents dir
	let sourceFiles = filter (isInfixOf ".c") files
	let sourceFilesWithPaths = map ((++) dir) sourceFiles
	sources <- mapM TIO.readFile sourceFilesWithPaths
	let result = zip sourceFiles sources
	return result