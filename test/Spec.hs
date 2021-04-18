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

parseSyntax :: Text -> Either (ParseErrorBundle Text Void) SyntaxDefinition
parseSyntax = runMap syntax

parsePackage :: Text -> Either (ParseErrorBundle Text Void) [PackageDefinition]
parsePackage t = runMap package (addSyntaxStatement t)

parseImports :: Text -> Either (ParseErrorBundle Text Void) [ImportStatement]
parseImports t = runMap imports (addSyntaxStatement t)

testSyntax =
  describe "[Parsing] Syntax Definition" $ do
    it "parses double quotes" $
      parseSyntax "syntax = \"proto3\";" `shouldParse` "proto3"
    it "parses single quotes" $
      parseSyntax "syntax = 'proto3';" `shouldParse` "proto3"
    it "parses with space inbetween" $
      parseSyntax "\n  \tsyntax   =  \n  'proto3';" `shouldParse` "proto3"
    it "fails if no syntax specified" $
      run `shouldFailOn` ""
    it "fails if syntax is not proto3" $
      run `shouldFailOn` "syntax = 'proto2';"
    it "fails if syntax is missing semicolon" $
      run `shouldFailOn` "syntax = 'proto3'"

testPackage =
  describe "[Parsing] Package Definition" $ do
    it "parses package specifier" $
      parsePackage "package  \n F_o__o.b4332ar.RJ7_;" `shouldParse` ["F_o__o.b4332ar.RJ7_"]
    it "fails if package specifier starts with '_'" $
      parsePackage "package _foo;" `shouldParse` []
    it "fails if sub package specifier starts with '_'" $
      parsePackage "package a._b;" `shouldParse` []
    it "fails if package specifier doesn't end with ';'" $
      parsePackage "package a.b;" `shouldParse` []
    it "fails if package specifier has symbol '!'" $
      parsePackage "package a!b;" `shouldParse` []

testImports =
  describe "[Parsing] Import Statements" $ do
    it "parses import statements in the right order" $
      parseImports "import   public \"first.proto\";\nimport 'second.proto';\timport weak 'third.proto' "
        `shouldParse` [ ImportStatement Public "first.proto",
                        ImportStatement Default "second.proto",
                        ImportStatement Weak "third.proto"
                      ]

main :: IO ()
main = hspec $ do
  testSyntax
  testPackage
  testImports
