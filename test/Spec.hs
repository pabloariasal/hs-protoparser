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

parseTopLevelDefs :: Text -> Either (ParseErrorBundle Text Void) [TopLevelDefinition]
parseTopLevelDefs t = runMap topLevelDefs (addSyntaxStatement t)

testSyntaxDefinition :: SpecWith ()
testSyntaxDefinition =
  describe "[Parsing] Syntax Definition" $ do
    it "double quotes" $
      parseSyntax "syntax = \"proto3\";" `shouldParse` "proto3"
    it "single quotes" $
      parseSyntax "syntax = 'proto3';" `shouldParse` "proto3"
    it "with space inbetween" $
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

testEmptyStatement :: SpecWith ()
testEmptyStatement =
  describe "[Parsing] Empty Statement" $ do
    it "parses empty statement" $
      run "syntax = \"proto3\"; ; \t;;  \t;package foo;\n;;import 'bla.proto';;;"
        `shouldParse` ProtoFile "proto3" ["foo"] [ImportStatement Nothing "bla.proto"] [] []

testOptionDefinition :: SpecWith ()
testOptionDefinition =
  describe "[Parsing] Option Definitions" $ do
    it "string literals" $
      parseOptions "option java_package = \"com.example.foo\";;"
        `shouldParse` [("java_package", StringLit "com.example.foo")]
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
    it "parentherized option name" $
      parseOptions "option (custom_option).foo.bar = false;"
        `shouldParse` [("custom_option.foo.bar", BoolLit False)]
    it "fails if closing parenthesis is missing" $
      run `shouldFailOn` addSyntaxStatement "option (a = false;"

testEnumDefinition :: SpecWith ()
testEnumDefinition =
  describe "[Parsing] Enum Definitions" $ do
    it "empty enum" $
      parseTopLevelDefs "enum Enum\n {}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [] []]
    it "enum with just empty statements" $
      parseTopLevelDefs "enum Enum\n {;;}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [] []]
    it "simple enum" $
      parseTopLevelDefs "enum Enum\n {\nUNKNOWN = 0;\n;;; RUNNING = -2;\n}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [] [EnumField "UNKNOWN" 0 [], EnumField "RUNNING" (negate 2) []]]
    it "parse enum with options (1/2)" $
      parseTopLevelDefs "enum Enum\n {option allow_alias = true\t;\n UNKNOWN = 0;}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [("allow_alias", BoolLit True)] [EnumField "UNKNOWN" 0 []]]
    it "parse enum with options (2/2)" $
      parseTopLevelDefs "enum Enum\n {UNKNOWN = 0; option allow_alias=false;}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [("allow_alias", BoolLit False)] [EnumField "UNKNOWN" 0 []]]
    it "parse enum with value options" $
      parseTopLevelDefs "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\", my_int=6];}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" [] [EnumField "RUNNING" 2 [("custom_option", StringLit "foo"), ("my_int", IntLit 6)]]]
    it "original order of enum fields and options is preserved" $
      parseTopLevelDefs "enum Enum\n {\nf1 = 0;\n;option o1 = bla;; f2 = 1; option o2 = true; f3=2; option o3=false;\n}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [("o1", Identifier "bla"), ("o2", BoolLit True), ("o3", BoolLit False)]
                            [EnumField "f1" 0 [], EnumField "f2" 1 [], EnumField "f3" 2 []]
                      ]
    it "fails if closing square bracket is missing" $
      run `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\";}"
    it "fails if no value options specified" $
      run `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [];}"

testEmptyMessage :: SpecWith ()
testEmptyMessage =
  describe "[Parsing] Empty Message Definitions" $ do
    it "empty message" $
      parseTopLevelDefs "message M {}"
        `shouldParse` [MsgDef $ MessageDefinition "M" []]
    it "empty message with empty statements" $
      parseTopLevelDefs "message M {;\t;}"
        `shouldParse` [MsgDef $ MessageDefinition "M" []]
    it "fails if no closing } is found" $
      run `shouldFailOn` addSyntaxStatement "message M {"
    it "fails if no name is provided" $
      run `shouldFailOn` addSyntaxStatement "message {"

testMessageWithOptions :: SpecWith ()
testMessageWithOptions =
  describe "[Parsing] Messages with Options" $ do
    it "message with two options" $
      parseTopLevelDefs "message M { option (my_option).a = 42;;; option b = false;; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Opt ("my_option.a", IntLit 42), Opt ("b", BoolLit False)]
                      ]
    it "fails if option is not correct" $
      run `shouldFailOn` addSyntaxStatement "message M { option my_option = 42 }"

testMessageWithEnums :: SpecWith ()
testMessageWithEnums =
  describe "[Parsing] Messages with Enums" $ do
    it "message with single enums" $
      parseTopLevelDefs "message M { enum Enum\n {\nA = 0;\n; B = 1;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Enum $ EnumDefinition "Enum" [] [EnumField "A" 0 [], EnumField "B" 1 []]]
                      ]
    it "message with two enums and one opt" $
      parseTopLevelDefs "message M { enum E1 {A = 0;};\t; option o = foo;  enum E2 {}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ Enum $ EnumDefinition "E1" [] [EnumField "A" 0 []],
                              Opt ("o", Identifier "foo"),
                              Enum $ EnumDefinition "E2" [] []
                            ]
                      ]
    it "fails if option is not correct" $
      run `shouldFailOn` addSyntaxStatement "message M { enum Enum {}"

testNestedMessages :: SpecWith ()
testNestedMessages =
  describe "[Parsing] Nested Message Definitions" $ do
    it "message inside message" $
      parseTopLevelDefs "message Outer { message Inner {} }"
        `shouldParse` [MsgDef $ MessageDefinition "Outer" [Msg $ MessageDefinition "Inner" []]]
    it "multiple nested messages" $
      parseTopLevelDefs "message O1 { message Inner{} ;} \n; message O2{}"
        `shouldParse` [ MsgDef (MessageDefinition "O1" [Msg $ MessageDefinition "Inner" []]),
                        MsgDef (MessageDefinition "O2" [])
                      ]

main :: IO ()
main = hspec $ do
  testSyntaxDefinition
  testPackageSpecifier
  testImportStatements
  testEmptyStatement
  testOptionDefinition
  testEnumDefinition
  testEmptyMessage
  testMessageWithOptions
  testMessageWithEnums
  testNestedMessages
