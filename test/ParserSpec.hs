{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Data.Text (Text, append)
import Data.Void
import HSProtoParser.Ast
import HSProtoParser.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec qualified as M

addSyntaxStatement :: Text -> Text
addSyntaxStatement s = "syntax='proto3';\n" `append` s

-- runs the proto parser with the provided input
runParser :: Text -> Either (M.ParseErrorBundle Text Void) ProtoFile
runParser = M.parse protoParser ""

runWithSyntax :: Text -> Either (M.ParseErrorBundle Text Void) ProtoFile
runWithSyntax t = tail <$> (runParser . addSyntaxStatement) t

testSyntaxDefinition :: Spec
testSyntaxDefinition =
  describe "[Parsing] Syntax Definition" $ do
    it "double quotes" $
      runParser "syntax = \"proto3\";" `shouldParse` [SyntaxStmt "proto3"]
    it "single quotes" $
      runParser "syntax = 'proto3';" `shouldParse` [SyntaxStmt "proto3"]
    it "with space inbetween" $
      runParser "\n  \tsyntax   =  \n  'proto3';" `shouldParse` [SyntaxStmt "proto3"]
    it "fails if no syntax specified" $
      runParser `shouldFailOn` ""
    it "fails if syntax is not proto3" $
      runParser `shouldFailOn` "syntax = 'proto2';"
    it "fails if syntax is missing semicolon" $
      runParser `shouldFailOn` "syntax = 'proto3'"

testEmptyStatement :: Spec
testEmptyStatement =
  describe "[Parsing] Empty Statement" $ do
    it "parses empty statement" $
      runParser "syntax = \"proto3\"; ; \t;;  \t;package foo;\n;;import 'bla.proto';;;"
        `shouldParse` [SyntaxStmt "proto3", PackageSpec "foo", ImportStmt (ImportStatement Nothing "bla.proto")]

testPackageSpecification :: Spec
testPackageSpecification =
  describe "[Parsing] Package Specification" $ do
    it "parses package specification" $
      runWithSyntax "import '';package  \n F_o__o.b4332ar.RJ7_;"
        `shouldParse` [ImportStmt (ImportStatement Nothing ""), PackageSpec "F_o__o.b4332ar.RJ7_"]
    it "fails if package specifier starts with '_'" $
      runParser `shouldFailOn` addSyntaxStatement "package _foo;"
    it "fails if sub package specifier starts with '_'" $
      runParser `shouldFailOn` addSyntaxStatement "package a._b;"
    it "fails if package specifier doesn't end with ';'" $
      runParser `shouldFailOn` addSyntaxStatement "package a.b"
    it "fails if package specifier has symbol '!'" $
      runParser `shouldFailOn` addSyntaxStatement "package a!b;"

testImportStatements :: Spec
testImportStatements =
  describe "[Parsing] Import Statements" $ do
    it "parse simple import statement" $
      runWithSyntax "package foo;import public \"file.proto\";"
        `shouldParse` [ PackageSpec "foo",
                        ImportStmt (ImportStatement (Just Public) "file.proto")
                      ]
    it "parses import statements in the right order" $
      runWithSyntax " import   public \"first\n.proto\";\nimport 'second.proto'   ;\timport weak 'third.proto'; "
        `shouldParse` [ ImportStmt $ ImportStatement (Just Public) "first\n.proto",
                        ImportStmt $ ImportStatement Nothing "second.proto",
                        ImportStmt $ ImportStatement (Just Weak) "third.proto"
                      ]
    it "fails with wrong access qualifier" $
      runParser `shouldFailOn` addSyntaxStatement "import foo 'bar';"
    it "fails if doesn't end with ';'" $
      runParser `shouldFailOn` addSyntaxStatement "import 'bar'"

testOptionDefinition :: Spec
testOptionDefinition =
  describe "[Parsing] Option Definitions" $ do
    it "string literals" $
      runWithSyntax "option java_package = \"com.example.foo\"      ;;"
        `shouldParse` [OptionDef ("java_package", StringLit "com.example.foo")]
    it "string literals" $
      runWithSyntax "option java_package = \"com.'example'.foo\";;"
        `shouldParse` [OptionDef ("java_package", StringLit "com.'example'.foo")]
    it "identifiers" $
      runWithSyntax "option java_package =    foo.bar;"
        `shouldParse` [OptionDef ("java_package", Identifier "foo.bar")]
    it "int literals" $
      runWithSyntax "option num1 = -5;option num2=+42;option num3=666;"
        `shouldParse` [OptionDef ("num1", IntLit (-5)), OptionDef ("num2", IntLit 42), OptionDef ("num3", IntLit 666)]
    it "float literals" $
      runWithSyntax "option n1=+4.4;option n2 = -1e5;option n3=+10.0E-1;option n4=666;"
        `shouldParse` [ OptionDef ("n1", FloatLit 4.4),
                        OptionDef ("n2", FloatLit (-100000)),
                        OptionDef ("n3", FloatLit 1.0),
                        OptionDef ("n4", IntLit 666)
                      ]
    it "boolean literals" $
      runWithSyntax "option b1 = false     ;    option b2=true;;"
        `shouldParse` [OptionDef ("b1", BoolLit False), OptionDef ("b2", BoolLit True)]
    it "combined" $
      runWithSyntax "option fl=-4.4;option id = foo;option il=42;option sl=\"666\";option bl=false;"
        `shouldParse` [ OptionDef ("fl", FloatLit (-4.4)),
                        OptionDef ("id", Identifier "foo"),
                        OptionDef ("il", IntLit 42),
                        OptionDef ("sl", StringLit "666"),
                        OptionDef ("bl", BoolLit False)
                      ]
    it "parentherized option name" $
      runWithSyntax "option (  custom_option\n).foo.bar = false;"
        `shouldParse` [OptionDef ("(custom_option).foo.bar", BoolLit False)]
    it "fails if parenthesis is not on first identifier" $
      runParser `shouldFailOn` addSyntaxStatement "option foo.(a) = false;"
    it "fails if closing parenthesis is missing" $
      runParser `shouldFailOn` addSyntaxStatement "option (a = false;"

testEnumDefinition :: Spec
testEnumDefinition =
  describe "[Parsing] Enum Definitions" $ do
    it "empty enum" $
      runWithSyntax "enum Enum\n {}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" []]
    it "enum with just empty statements" $
      runWithSyntax "enum Enum\n {;;}"
        `shouldParse` [EnumDef $ EnumDefinition "Enum" []]
    it "simple enum" $
      runWithSyntax "enum Enum\n {\nUNKNOWN = 0;\n;;; RUNNING = -2;\n}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnField (EnumField "UNKNOWN" 0 []), EnField (EnumField "RUNNING" (-2) [])]
                      ]
    it "parse enum with options (1/2)" $
      runWithSyntax "enum Enum\n {option allow_alias = true\t;\n UNKNOWN = 0;}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnOpt ("allow_alias", BoolLit True), EnField (EnumField "UNKNOWN" 0 [])]
                      ]
    it "parse enum with options (2/2)" $
      runWithSyntax "enum Enum\n {option allow_alias=false;UNKNOWN = 0; }"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnOpt ("allow_alias", BoolLit False), EnField (EnumField "UNKNOWN" 0 [])]
                      ]
    it "parse enum with field options" $
      runWithSyntax "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\"   , my_int=6   ];}"
        `shouldParse` [ EnumDef $
                          EnumDefinition
                            "Enum"
                            [EnField (EnumField "RUNNING" 2 [("(custom_option)", StringLit "foo"), ("my_int", IntLit 6)])]
                      ]
    it "original order of enum fields and options is preserved" $
      runWithSyntax "enum Enum\n {\nf1 = 0;\n;option o1 = bla;; f2 = 1; option o2 = true; f3=2; option o3=false;\n}"
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
      runParser `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\";}"
    it "fails if no value options specified" $
      runParser `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [];}"

testEmptyMessage :: Spec
testEmptyMessage =
  describe "[Parsing] Empty Message Definitions" $ do
    it "empty message" $
      runWithSyntax "message M {}"
        `shouldParse` [MsgDef $ MessageDefinition "M" []]
    it "empty message with empty statements" $
      runWithSyntax "message M {;\t;}"
        `shouldParse` [MsgDef $ MessageDefinition "M" []]
    it "fails if no closing } is found" $
      runParser `shouldFailOn` addSyntaxStatement "message M {"
    it "fails if no name is provided" $
      runParser `shouldFailOn` addSyntaxStatement "message {"

testMessageWithNormalFields :: Spec
testMessageWithNormalFields =
  describe "[Parsing] Messages with normal fields" $ do
    it "message with two normal field" $
      runWithSyntax "message M { foo.Bar nested_message = 2;; repeated int32 samples = 4; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ NorF $ NormalField (FieldDefinition "nested_message" (FTMessageType "foo.Bar") 2 []) False,
                              NorF $ NormalField (FieldDefinition "samples" FTInt32 4 []) True
                            ]
                      ]
    it "message with normal field with options" $
      runWithSyntax "message M { sint32 foo = 4 [o1=true,o2=-5.0];\n; string bar = 1 [o3=-9];; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ NorF $ NormalField (FieldDefinition "foo" FTSInt32 4 [("o1", BoolLit True), ("o2", FloatLit (-5.0))]) False,
                              NorF $ NormalField (FieldDefinition "bar" FTString 1 [("o3", IntLit (-9))]) False
                            ]
                      ]
    it "fails if semicolon missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { bool my_option = true }"
    it "fails if repeated is misspelled" $
      runParser `shouldFailOn` addSyntaxStatement "message M { Repeated fixed64 my_option = 67; }"
    it "fails if message name starts with number" $
      runParser `shouldFailOn` addSyntaxStatement "message M { fixed32 0my_option = 67; }"
    it "fails if '=' missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { double my_option 67; }"
    it "fails if ';' missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { sint64 my_option = 67 }"
    it "fails if option number is negative" $
      runParser `shouldFailOn` addSyntaxStatement "message M { sint64 my_option = -67; }"

testMessageWithOneOfFields :: Spec
testMessageWithOneOfFields =
  describe "[Parsing] Messages with oneof fields" $ do
    it "message with empty oneof field" $
      runWithSyntax "message M {\toneof foo\n {}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $ OneOfField "foo" []
                            ]
                      ]
    it "message with oneof field with only empty statements" $
      runWithSyntax "message M {\toneof foo\n {;;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ OneF $ OneOfField "foo" []
                            ]
                      ]
    it "message with oneof and two simple fields" $
      runWithSyntax "message M {\toneof foo\n {string name = 4;;; bytes b = 5;}}"
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
      runWithSyntax "message M {\toneof foo\n {sfixed32 b = 5; option opt = 'value'; }}"
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
      runWithSyntax "message M {\toneof foo\n {string name = 4[o1=true,o2=-5.0];;;}}"
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
      runParser `shouldFailOn` addSyntaxStatement "message M {oneof foo {repeated string name = 4;}}"

testMessageWithOptions :: Spec
testMessageWithOptions =
  describe "[Parsing] Messages with Options" $ do
    it "message with two options" $
      runWithSyntax "message M { option (my_option).a = 42;;; option b = false;; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Opt ("(my_option).a", IntLit 42), Opt ("b", BoolLit False)]
                      ]
    it "fails if option is not correct" $
      runParser `shouldFailOn` addSyntaxStatement "message M { option my_option = 42 }"

testMessageWithMapFields :: Spec
testMessageWithMapFields =
  describe "[Parsing] Messages with map fields" $ do
    it "message with simple map field" $
      runWithSyntax "message M { map< string , Project > projects\n =   3; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [MapF $ MapField "projects" KTString (FTMessageType "Project") 3 []]
                      ]
    it "message with options" $
      runWithSyntax "message M { map<bool, bool> projects = 3 [o1=42,o2=false]; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [MapF $ MapField "projects" KTBool FTBool 3 [("o1", IntLit 42), ("o2", BoolLit False)]]
                      ]
    it "fails if ',' is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { map< string  Project > projects\n =   3; }"
    it "fails if '<' is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { map string,  Project > projects\n =   3; }"
    it "fails if '>' is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { map <string,  Project  projects\n =   3; }"
    it "fails if ';' is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { map <string,  Project>  projects\n =   3 }"

testMessageWithEnums :: Spec
testMessageWithEnums =
  describe "[Parsing] Messages with Enums" $ do
    it "message with single enums" $
      runWithSyntax "message M { enum Enum\n {\nA = 0;\n; B = 1;}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [Enum $ EnumDefinition "Enum" [EnField (EnumField "A" 0 []), EnField (EnumField "B" 1 [])]]
                      ]
    it "message with two enums and one opt" $
      runWithSyntax "message M { enum E1 {A = 0;};\t; option o = foo;  enum E2 {}}"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [ Enum $ EnumDefinition "E1" [EnField (EnumField "A" 0 [])],
                              Opt ("o", Identifier "foo"),
                              Enum $ EnumDefinition "E2" []
                            ]
                      ]
    it "fails if option is not correct" $
      runParser `shouldFailOn` addSyntaxStatement "message M { enum Enum {}"

testMessageWithReservedFieldNumbers :: Spec
testMessageWithReservedFieldNumbers =
  describe "[Parsing] Messages with reserved field numbers" $ do
    it "message with single reserved field number" $
      runWithSyntax "message M { reserved 5;;; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNums [Single 5]]
                      ]
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 5, 7,   7 ; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNums [Single 5, Single 7, Single 7]]
                      ]
    it "message with reserved field number range" $
      runWithSyntax "message M { reserved 5 to 9; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNums [Range 5 9]]
                      ]
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 5 to 9, 7, 8 to 11, 0; reserved 7 to 4, 42; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNums [Range 5 9, Single 7, Range 8 11, Single 0], RsvFieldNums [Range 7 4, Single 42]]
                      ]
    it "fails if ; is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved 9}"
    it "fails on empty list" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved ;}"

testMessageWithReservedFieldNames :: Spec
testMessageWithReservedFieldNames =
  describe "[Parsing] Messages with reserved field names" $ do
    it "message with single reserved field number" $
      runWithSyntax "message M { reserved \"foo\" ;;  ; reserved 8; reserved 'bla'; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNames ["foo"], RsvFieldNums [Single 8], RsvFieldNames ["bla"]]
                      ]
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 'foo', \"bar\"; ;;; \t\rreserved \"qux\"\t; }"
        `shouldParse` [ MsgDef $
                          MessageDefinition
                            "M"
                            [RsvFieldNames ["foo", "bar"], RsvFieldNames ["qux"]]
                      ]
    it "fails on empty field name list" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved ;}"
    it "fails on invalid field name" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved '8foo' ;}"
    it "fails on invalid field name 2" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved foo ;}"
    it "fails if ; is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved \"foo\"}"

testNestedMessages :: Spec
testNestedMessages =
  describe "[Parsing] Nested Message Definitions" $ do
    it "message inside message" $
      runWithSyntax "message Outer { message Inner {} }"
        `shouldParse` [MsgDef $ MessageDefinition "Outer" [Msg $ MessageDefinition "Inner" []]]
    it "multiple nested messages" $
      runWithSyntax "message O1 { message Inner{} ;} \n; message O2{}"
        `shouldParse` [ MsgDef (MessageDefinition "O1" [Msg $ MessageDefinition "Inner" []]),
                        MsgDef (MessageDefinition "O2" [])
                      ]

spec :: Spec
spec = do
  testSyntaxDefinition
  testPackageSpecification
  testEmptyStatement
  testImportStatements
  testOptionDefinition
  testEnumDefinition
  testEmptyMessage
  testMessageWithNormalFields
  testMessageWithOptions
  testMessageWithEnums
  testMessageWithReservedFieldNumbers
  testMessageWithReservedFieldNames
  testNestedMessages
  testMessageWithOneOfFields
  testMessageWithMapFields
