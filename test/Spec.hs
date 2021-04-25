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

-- runs the proto parser with the provided input
run :: Text -> Either (ParseErrorBundle Text Void) ProtoFile
run = parse protoParser ""

runMap :: (ProtoFile -> a) -> Text -> Either (ParseErrorBundle Text Void) a
runMap f t = f <$> run t

parseSyntax :: Text -> Either (ParseErrorBundle Text Void) SyntaxStatement
parseSyntax = runMap syntaxStmt

parsePackage :: Text -> Either (ParseErrorBundle Text Void) [PackageSpecification]
parsePackage t = runMap packageSpec (addSyntaxStatement t)

parseImports :: Text -> Either (ParseErrorBundle Text Void) [ImportStatement]
parseImports t = runMap importStmts (addSyntaxStatement t)

parseOptions :: Text -> Either (ParseErrorBundle Text Void) [OptionDefinition]
parseOptions t = runMap optionDefs (addSyntaxStatement t)

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
      run `shouldFailOn` addSyntaxStatement "package a.b"
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

testEmpyStatement :: SpecWith ()
testEmpyStatement =
  describe "[Parsing] Empty Statement" $ do
    it "parses empty statement" $
      run "syntax = \"proto3\"; ; \t;;  \t;package foo;\n;;import 'bla.proto';;;"
        `shouldParse` ProtoFile "proto3" ["foo"] [ImportStatement Nothing "bla.proto"] []

testOptionDefinition :: SpecWith ()
testOptionDefinition =
  describe "[Parsing] Option Definitions" $ do
    it "string literals" $
      parseOptions "option java_package = \"com.example.foo\";;"
        `shouldParse` [("java_package", StringLit "com.example.foo")]
    it "identifiers" $
      parseOptions "option java_package =    foo.bar;"
        `shouldParse` [("java_package", Identifier "foo.bar")]
    it "int literals" $
      parseOptions "option num1 = -5;option num2=+42;option num3=666;"
        `shouldParse` [("num1", IntLit (negate 5)), ("num2", IntLit 42), ("num3", IntLit 666)]
    it "float literals" $
      parseOptions "option n1=+4.4;option n2 = -1e5;option n3=+10.0E-1;option n4=666;"
        `shouldParse` [ ("n1", FloatLit 4.4),
                        ("n2", FloatLit (negate 100000)),
                        ("n3", FloatLit 1.0),
                        ("n4", IntLit 666)
                      ]
    it "boolean literals" $
      parseOptions "option b1 = false;option b2=true;;"
        `shouldParse` [("b1", BoolLit False), ("b2", BoolLit True)]
    it "combined" $
      parseOptions "option fl=-4.4;option id = foo;option il=42;option sl=\"666\";option bl=false;"
        `shouldParse` [ ("fl", FloatLit (negate 4.4)),
                        ("id", Identifier "foo"),
                        ("il", IntLit 42),
                        ("sl", StringLit "666"),
                        ("bl", BoolLit False)
                      ]

main :: IO ()
main = hspec $ do
  testSyntaxDefinition
  testPackageSpecifier
  testImportStatements
  testEmpyStatement
  testOptionDefinition
