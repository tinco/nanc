import Test.Hspec

import Nanc.Compiler
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

