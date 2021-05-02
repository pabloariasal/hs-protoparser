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
        `shouldParse` [("num1", IntLit (-5)), ("num2", IntLit 42), ("num3", IntLit 666)]
    it "float literals" $
      parseOptions "option n1=+4.4;option n2 = -1e5;option n3=+10.0E-1;option n4=666;"
        `shouldParse` [ ("n1", FloatLit 4.4),
                        ("n2", FloatLit (-100000)),
                        ("n3", FloatLit 1.0),
                        ("n4", IntLit 666)
                      ]
    it "boolean literals" $
      parseOptions "option b1 = false;option b2=true;;"
        `shouldParse` [("b1", BoolLit False), ("b2", BoolLit True)]
    it "combined" $
      parseOptions "option fl=-4.4;option id = foo;option il=42;option sl=\"666\";option bl=false;"
        `shouldParse` [ ("fl", FloatLit (-4.4)),
                        ("id", Identifier "foo"),
                        ("il", IntLit 42),
                        ("sl", StringLit "666"),
                        ("bl", BoolLit False)
                      ]
    it "parentherized option name" $
      parseOptions "option (  custom_option\n).foo.bar = false;"
        `shouldParse` [("(custom_option).foo.bar", BoolLit False)]
    it "fails if parenthesis is not on first identifier" $
      run `shouldFailOn` addSyntaxStatement "option foo.(a) = false;"
    it "fails if closing parenthesis is missing" $
      run `shouldFailOn` addSyntaxStatement "option (a = false;"

testEnumDefinition :: SpecWith ()
testEnumDefinition =
  describe "[Parsing] Enum Definitions" $ do
    it "empty enum" $
      parseTopLevelDefs "enum Enum\n {}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" []]
    it "enum with just empty statements" $
      parseTopLevelDefs "enum Enum\n {;;}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" []]
    it "simple enum" $
      parseTopLevelDefs "enum Enum\n {\nUNKNOWN = 0;\n;;; RUNNING = -2;\n}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnField (EnumField "UNKNOWN" 0 []), EnField (EnumField "RUNNING" (-2) [])]
                      ]
    it "parse enum with options (1/2)" $
      parseTopLevelDefs "enum Enum\n {option allow_alias = true\t;\n UNKNOWN = 0;}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnOpt ("allow_alias", BoolLit True), EnField (EnumField "UNKNOWN" 0 [])]
                      ]
    it "parse enum with options (2/2)" $
      parseTopLevelDefs "enum Enum\n {option allow_alias=false;UNKNOWN = 0; }"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnOpt ("allow_alias", BoolLit False), EnField (EnumField "UNKNOWN" 0 [])]
                      ]
    it "parse enum with value options" $
      parseTopLevelDefs "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\", my_int=6];}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnField (EnumField "RUNNING" 2 [("(custom_option)", StringLit "foo"), ("my_int", IntLit 6)])]
                      ]
    it "original order of enum fields and options is preserved" $
      parseTopLevelDefs "enum Enum\n {\nf1 = 0;\n;option o1 = bla;; f2 = 1; option o2 = true; f3=2; option o3=false;\n}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [ EnField (EnumField "f1" 0 []),
                              EnOpt ("o1", Identifier "bla"),
                              EnField (EnumField "f2" 1 []),
                              EnOpt ("o2", BoolLit True),
                              EnField (EnumField "f3" 2 []),
                              EnOpt ("o3", BoolLit False)
                            ]
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

testMessageWithNormalFields :: SpecWith ()
testMessageWithNormalFields =
  describe "[Parsing] Messages with normal fields" $ do
    it "message with two normal field" $
      parseTopLevelDefs "message M { foo.Bar nested_message = 2;; repeated int32 samples = 4; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ NorF $ NormalField (FieldDefinition "nested_message" (FTMessageType "foo.Bar") 2 []) False,
                              NorF $ NormalField (FieldDefinition "samples" FTInt32 4 []) True
                            ]
                      ]
    it "message with normal field with options" $
      parseTopLevelDefs "message M { sint32 foo = 4 [o1=true,o2=-5.0];\n; string bar = 1 [o3=-9];; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ NorF $ NormalField (FieldDefinition "foo" FTSInt32 4 [("o1", BoolLit True), ("o2", FloatLit (-5.0))]) False,
                              NorF $ NormalField (FieldDefinition "bar" FTString 1 [("o3", IntLit (-9))]) False
                            ]
                      ]
    it "fails if semicolon missing" $
      run `shouldFailOn` addSyntaxStatement "message M { bool my_option = true }"
    it "fails if repeated is misspelled" $
      run `shouldFailOn` addSyntaxStatement "message M { Repeated fixed64 my_option = 67; }"
    it "fails if message name starts with number" $
      run `shouldFailOn` addSyntaxStatement "message M { fixed32 0my_option = 67; }"
    it "fails if '=' missing" $
      run `shouldFailOn` addSyntaxStatement "message M { double my_option 67; }"
    it "fails if ';' missing" $
      run `shouldFailOn` addSyntaxStatement "message M { sint64 my_option = 67 }"
    it "fails if option number is negative" $
      run `shouldFailOn` addSyntaxStatement "message M { sint64 my_option = -67; }"

testMessageWithOneOfFields :: SpecWith ()
testMessageWithOneOfFields =
  describe "[Parsing] Messages with oneof fields" $ do
    it "message with empty oneof field" $
      parseTopLevelDefs "message M {\toneof foo\n {}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $ OneOfField "foo" []
                            ]
                      ]
    it "message with oneof field with only empty statements" $
      parseTopLevelDefs "message M {\toneof foo\n {;;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $ OneOfField "foo" []
                            ]
                      ]
    it "message with oneof and two simple fields" $
      parseTopLevelDefs "message M {\toneof foo\n {string name = 4;;; bytes b = 5;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $
                                OneOfField
                                  "foo"
                                  [ OFFieldDef (FieldDefinition "name" FTString 4 []),
                                    OFFieldDef (FieldDefinition "b" FTBytes 5 [])
                                  ]
                            ]
                      ]
    it "message with oneof field with and option and a field" $
      parseTopLevelDefs "message M {\toneof foo\n {sfixed32 b = 5; option opt = 'value'; }}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $
                                OneOfField
                                  "foo"
                                  [ OFFieldDef (FieldDefinition "b" FTSfixed32 5 []),
                                    OFOptDef ("opt", StringLit "value")
                                  ]
                            ]
                      ]
    it "message with oneof with field with options" $
      parseTopLevelDefs "message M {\toneof foo\n {string name = 4[o1=true,o2=-5.0];;;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $
                                OneOfField
                                  "foo"
                                  [ OFFieldDef
                                      ( FieldDefinition
                                          "name"
                                          FTString
                                          4
                                          [("o1", BoolLit True), ("o2", FloatLit (-5.0))]
                                      )
                                  ]
                            ]
                      ]
    it "fails if field is repeated" $
      run `shouldFailOn` addSyntaxStatement "message M {oneof foo {repeated string name = 4;}}"

testMessageWithOptions :: SpecWith ()
testMessageWithOptions =
  describe "[Parsing] Messages with Options" $ do
    it "message with two options" $
      parseTopLevelDefs "message M { option (my_option).a = 42;;; option b = false;; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Opt ("(my_option).a", IntLit 42), Opt ("b", BoolLit False)]
                      ]
    it "fails if option is not correct" $
      run `shouldFailOn` addSyntaxStatement "message M { option my_option = 42 }"

testMessageWithMapFields :: SpecWith ()
testMessageWithMapFields =
  describe "[Parsing] Messages with map fields" $ do
    it "message with simple map field" $
      parseTopLevelDefs "message M { map< string , Project > projects\n =   3; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [MapF $ MapField "projects" KTString (FTMessageType "Project") 3 []]
                      ]
    it "message with options" $
      parseTopLevelDefs "message M { map<bool, bool> projects = 3 [o1=42,o2=false]; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [MapF $ MapField "projects" KTBool FTBool 3 [("o1", IntLit 42), ("o2", BoolLit False)]]
                      ]
    it "fails if ',' is missing" $
      run `shouldFailOn` addSyntaxStatement "message M { map< string  Project > projects\n =   3; }"
    it "fails if '<' is missing" $
      run `shouldFailOn` addSyntaxStatement "message M { map string,  Project > projects\n =   3; }"
    it "fails if '>' is missing" $
      run `shouldFailOn` addSyntaxStatement "message M { map <string,  Project  projects\n =   3; }"
    it "fails if ';' is missing" $
      run `shouldFailOn` addSyntaxStatement "message M { map <string,  Project>  projects\n =   3 }"

testMessageWithEnums :: SpecWith ()
testMessageWithEnums =
  describe "[Parsing] Messages with Enums" $ do
    it "message with single enums" $
      parseTopLevelDefs "message M { enum Enum\n {\nA = 0;\n; B = 1;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Enum $ EnumDefinition "Enum" [EnField (EnumField "A" 0 []), EnField (EnumField "B" 1 [])]]
                      ]
    it "message with two enums and one opt" $
      parseTopLevelDefs "message M { enum E1 {A = 0;};\t; option o = foo;  enum E2 {}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ Enum $ EnumDefinition "E1" [EnField (EnumField "A" 0 [])],
                              Opt ("o", Identifier "foo"),
                              Enum $ EnumDefinition "E2" []
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
  testMessageWithNormalFields
  testMessageWithOptions
  testMessageWithEnums
  testNestedMessages
  testMessageWithOneOfFields
  testMessageWithMapFields
