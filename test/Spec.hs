{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import HSProtoParser.Ast
import HSProtoParser.Parser

test :: (ProtoFile -> a) -> Text -> Either (ParseErrorBundle Text Void) a
test f t = f <$> parse parseProto "" t

main :: IO ()
main = hspec $ do
  describe "syntax statement parser" $ do
    it "parses double quotes" $
      test syntax "syntax = \"proto3\";" `shouldParse` "proto3"
    it "parses single quotes" $
      test syntax "syntax = 'proto3';" `shouldParse` "proto3"
    it "parses with space inbetween" $
      test syntax "\n  \tsyntax   =  \n  'proto3';" `shouldParse` "proto3"
    it "fails if no syntax specified" $
      parse parseProto ""  `shouldFailOn` ""
    it "fails if syntax is not proto3" $
      parse parseProto ""  `shouldFailOn` "syntax = 'proto2';"
    it "fails if syntax is missing semicolon" $
      parse parseProto ""  `shouldFailOn` "syntax = 'proto3'"
