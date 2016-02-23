import Test.Hspec

import Nanc.Compiler
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Either

main :: IO ()
main = hspec $ do
	describe "Nanc.Compiler.parse" $ do
		it "Parses hello world" $ do
			let fileName = "test.c"
			src <- TIO.readFile fileName
			preprocessed <- preprocess fileName src
			parse fileName preprocessed `shouldSatisfy` isRight

	describe "Nanc.Compiler.generateIR" $ do
		it "Generates IR for sanity" $ do
			src <- testFile "0001-sanity"
			ir <- compileSrcToIR "0001-sanity.c" src
			ir `shouldSatisfy` (/= T.empty)

		it "Generates IR for global" $ do
			src <- testFile "0002-global1"
			ir <- compileSrcToIR "0002-global2.c" src
			ir `shouldSatisfy` (/= T.empty)

testFile name = TIO.readFile $ "spec/spec_support/execute/" ++ name ++ ".c"
