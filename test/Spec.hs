{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, append, pack)
import Data.Void
import HSProtoParser.Ast
import HSProtoParser.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

addSyntaxStatement :: Text -> Text
addSyntaxStatement s = "syntax='proto3';\n" `append` s

run :: Text -> Either (ParseErrorBundle Text Void) ProtoFile
run = parse protoParser ""

runMap :: (ProtoFile -> a) -> Text -> Either (ParseErrorBundle Text Void) a
runMap f t = f <$> run t

testSyntax :: Text -> Either (ParseErrorBundle Text Void) String
testSyntax = runMap syntax

testPackage :: Text -> Either (ParseErrorBundle Text Void) [PackageDefinition]
testPackage t = runMap package (addSyntaxStatement t)

main :: IO ()
main = hspec $ do
  describe "[Parsing] Syntax Statement" $ do
    it "parses double quotes" $
      testSyntax "syntax = \"proto3\";" `shouldParse` "proto3"
    it "parses single quotes" $
      testSyntax "syntax = 'proto3';" `shouldParse` "proto3"
    it "parses with space inbetween" $
      testSyntax "\n  \tsyntax   =  \n  'proto3';" `shouldParse` "proto3"
    it "fails if no syntax specified" $
      run `shouldFailOn` ""
    it "fails if syntax is not proto3" $
      run `shouldFailOn` "syntax = 'proto2';"
    it "fails if syntax is missing semicolon" $
      run `shouldFailOn` "syntax = 'proto3'"

  describe "[Parsing] Package Specifier" $ do
    it "parses package specifier" $
      testPackage "package  \n F_o__o.b4332ar.RJ7_;" `shouldParse` ["F_o__o.b4332ar.RJ7_"]
    it "fails if package specifier starts with '_'" $
      testPackage "package _foo;" `shouldParse` []
    it "fails if sub package specifier starts with '_'" $
      testPackage "package a._b;" `shouldParse` []
    it "fails if package specifier doesn't end with ';'" $
      testPackage "package a.b;" `shouldParse` []
    it "fails if package specifier has symbol '!'" $
      testPackage "package a!b;" `shouldParse` []
