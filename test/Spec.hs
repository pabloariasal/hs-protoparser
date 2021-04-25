{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, append)
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
parseSyntax = runMap syntaxDef

parsePackage :: Text -> Either (ParseErrorBundle Text Void) [PackageSpecification]
parsePackage t = runMap packageSpec (addSyntaxStatement t)

parseImports :: Text -> Either (ParseErrorBundle Text Void) [ImportStatement]
parseImports t = runMap importStmts (addSyntaxStatement t)

testSyntaxDefinition :: SpecWith ()
testSyntaxDefinition =
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

testPackageSpecifier :: SpecWith ()
testPackageSpecifier =
  describe "[Parsing] Package Specifier" $ do
    it "parses package specifier" $
      parsePackage "import '';package  \n F_o__o.b4332ar.RJ7_;" `shouldParse` ["F_o__o.b4332ar.RJ7_"]
    it "fails if package specifier starts with '_'" $
      run `shouldFailOn` addSyntaxStatement "package _foo;"
    it "fails if sub package specifier starts with '_'" $
      run `shouldFailOn` addSyntaxStatement "package a._b;"
    it "fails if package specifier doesn't end with ';'" $
      run `shouldFailOn` addSyntaxStatement "package a.b;"
    it "fails if package specifier has symbol '!'" $
      run `shouldFailOn` addSyntaxStatement "package a!b;"

testImportStatements :: SpecWith ()
testImportStatements =
  describe "[Parsing] Import Statements" $ do
    it "parse simple import statement" $
      parseImports "package foo;import public \"file.proto\";" `shouldParse` [ImportStatement (Just Public) "file.proto"]
    it "parses import statements in the right order" $
      parseImports " import   public \"first\n.proto\";\nimport 'second.proto'   ;\timport weak 'third.proto'; "
        `shouldParse` [ ImportStatement (Just Public) "first\n.proto",
                        ImportStatement Nothing "second.proto",
                        ImportStatement (Just Weak) "third.proto"
                      ]
    it "fails with wrong access qualifier" $
      run `shouldFailOn` addSyntaxStatement "import foo 'bar';"
    it "fails if doesn't end with ';'" $
      run `shouldFailOn` addSyntaxStatement "import 'bar'"

main :: IO ()
main = hspec $ do
  testSyntaxDefinition
  testPackageSpecifier
  testImportStatements
