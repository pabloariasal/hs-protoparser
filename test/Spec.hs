import Test.Hspec

import HSProtoParser.Parser

main :: IO ()
main = hspec $ do
  describe "syntax definition parser" $ do
    it "parses syntax definition correctly" $
      parse `shouldBe` 8
