{-# LANGUAGE DuplicateRecordFields #-}
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

runParser :: Text -> Either (M.ParseErrorBundle Text Void) ProtoFile
runParser = M.parse protoParser ""

runWithSyntax :: Text -> Either (M.ParseErrorBundle Text Void) ProtoFile
runWithSyntax = runParser . addSyntaxStatement

emptyFile :: ProtoFile
emptyFile = ProtoFile "proto3" [] [] [] [] [] []

testSyntaxDefinition :: Spec
testSyntaxDefinition =
  describe "[Parsing] Syntax Definition" $ do
    it "double quotes" $
      runParser "syntax = \"proto3\";" `shouldParse` emptyFile
    it "single quotes" $
      runParser "syntax = 'proto3';" `shouldParse` emptyFile
    it "with space inbetween" $
      runParser "\n  \tsyntax \r  =  \n  'proto3';" `shouldParse` emptyFile
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
        `shouldParse` emptyFile {packages = ["foo"], imports = [ImportStatement Nothing "bla.proto"]}

testPackageSpecification :: Spec
testPackageSpecification =
  describe "[Parsing] Package Specification" $ do
    it "parses package specification" $
      runWithSyntax "import '';package  \n F_o__o.b4332ar.RJ7_;"
        `shouldParse` emptyFile
          { imports = [ImportStatement Nothing ""],
            packages = ["F_o__o.b4332ar.RJ7_"]
          }
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
        `shouldParse` emptyFile
          { packages = ["foo"],
            imports =
              [ImportStatement (Just Public) "file.proto"]
          }
    it "parses import statements in the right order" $
      runWithSyntax " import   public \"first\n.proto\";\nimport 'second.proto'   ;\timport weak 'third.proto'; "
        `shouldParse` emptyFile
          { imports =
              [ ImportStatement (Just Public) "first\n.proto",
                ImportStatement Nothing "second.proto",
                ImportStatement (Just Weak) "third.proto"
              ]
          }
    it "fails with wrong access qualifier" $
      runParser `shouldFailOn` addSyntaxStatement "import foo 'bar';"
    it "fails if doesn't end with ';'" $
      runParser `shouldFailOn` addSyntaxStatement "import 'bar'"

testOptionDefinition :: Spec
testOptionDefinition =
  describe "[Parsing] Option Definitions" $ do
    it "string literals" $
      runWithSyntax "option java_package = \"com.example.foo\"      ;;"
        `shouldParse` (emptyFile {options = [("java_package", CStringLit "com.example.foo")]} :: ProtoFile)
    it "string literals" $
      runWithSyntax "option java_package = \"com.'example'.foo\";;"
        `shouldParse` (emptyFile {options = [("java_package", CStringLit "com.'example'.foo")]} :: ProtoFile)
    it "identifiers" $
      runWithSyntax "option java_package =    foo.bar;"
        `shouldParse` (emptyFile {options = [("java_package", CIdentifier "foo.bar")]} :: ProtoFile)
    it "int literals" $
      runWithSyntax "option num1 = -5;option num2=+42;option num3=666;"
        `shouldParse` (emptyFile {options = [("num1", CIntLit (-5)), ("num2", CIntLit 42), ("num3", CIntLit 666)]} :: ProtoFile)
    it "float literals" $
      runWithSyntax "option n1=+4.4;option n2 = -1e5;option n3=+10.0E-1;option n4=666;"
        `shouldParse` ( emptyFile
                          { options =
                              [ ("n1", CFloatLit 4.4),
                                ("n2", CFloatLit (-100000)),
                                ("n3", CFloatLit 1.0),
                                ("n4", CIntLit 666)
                              ]
                          } ::
                          ProtoFile
                      )
    it "boolean literals" $
      runWithSyntax "option b1 = false     ;    option b2=true;;"
        `shouldParse` (emptyFile {options = [("b1", CBoolLit False), ("b2", CBoolLit True)]} :: ProtoFile)
    it "combined" $
      runWithSyntax "option fl=-4.4;option id = foo;option il=42;option sl=\"666\";option bl=false;"
        `shouldParse` ( emptyFile
                          { options =
                              [ ("fl", CFloatLit (-4.4)),
                                ("id", CIdentifier "foo"),
                                ("il", CIntLit 42),
                                ("sl", CStringLit "666"),
                                ("bl", CBoolLit False)
                              ]
                          } ::
                          ProtoFile
                      )
    it "parentherized option name" $
      runWithSyntax "option (  custom_option\n).foo.bar = false;"
        `shouldParse` (emptyFile {options = [("(custom_option).foo.bar", CBoolLit False)]} :: ProtoFile)
    it "fails if parenthesis is not on first identifier" $
      runParser `shouldFailOn` addSyntaxStatement "option foo.(a) = false;"
    it "fails if closing parenthesis is missing" $
      runParser `shouldFailOn` addSyntaxStatement "option (a = false;"

testEnumDefinition :: Spec
testEnumDefinition =
  describe "[Parsing] Enum Definitions" $ do
    it "empty enum" $
      runWithSyntax "enum Enum\n {}"
        `shouldParse` emptyFile {enums = [EnumDefinition "Enum" [] []]}
    it "enum with just empty statements" $
      runWithSyntax "enum Enum\n {;;}"
        `shouldParse` emptyFile {enums = [EnumDefinition "Enum" [] []]}
    it "simple enum" $
      runWithSyntax "enum Enum\n {\nUNKNOWN = 0;\n;;; RUNNING = -2;\n}"
        `shouldParse` emptyFile {enums = [EnumDefinition "Enum" [] [EnumField "UNKNOWN" 0 [], EnumField "RUNNING" (-2) []]]}
    it "parse enum with options (1/2)" $
      runWithSyntax "enum Enum\n {option allow_alias = true\t;\n UNKNOWN = 0;}"
        `shouldParse` emptyFile {enums = [EnumDefinition "Enum" [("allow_alias", CBoolLit True)] [EnumField "UNKNOWN" 0 []]]}
    it "parse enum with options (2/2)" $
      runWithSyntax "enum Enum\n {option allow_alias=false;UNKNOWN = 0; }"
        `shouldParse` emptyFile {enums = [EnumDefinition "Enum" [("allow_alias", CBoolLit False)] [EnumField "UNKNOWN" 0 []]]}
    it "parse enum with field options" $
      runWithSyntax "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\"   , my_int=6   ];}"
        `shouldParse` emptyFile
          { enums =
              [ EnumDefinition
                  "Enum"
                  []
                  [EnumField "RUNNING" 2 [("(custom_option)", CStringLit "foo"), ("my_int", CIntLit 6)]]
              ]
          }
    it "original order of enum fields and options is preserved" $
      runWithSyntax "enum Enum\n {\nf1 = 0;\n;option o1 = bla;; f2 = 1; option o2 = true; f3=2; option o3=false;\n}"
        `shouldParse` emptyFile
          { enums =
              [ EnumDefinition
                  "Enum"
                  [ ("o1", CIdentifier "bla"),
                    ("o2", CBoolLit True),
                    ("o3", CBoolLit False)
                  ]
                  [ EnumField "f1" 0 [],
                    EnumField "f2" 1 [],
                    EnumField "f3" 2 []
                  ]
              ]
          }
    it "fails if closing square bracket is missing" $
      runParser `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [(custom_option) = \"foo\";}"
    it "fails if no value options specified" $
      runParser `shouldFailOn` addSyntaxStatement "enum Enum\n {RUNNING = 2 [];}"

emptyMessage :: String -> MessageDefinition
emptyMessage n = MessageDefinition n [] [] [] [] [] [] []

testEmptyMessage :: Spec
testEmptyMessage =
  describe "[Parsing] Empty Message Definitions" $ do
    it "empty message" $
      runWithSyntax "message M {}"
        `shouldParse` emptyFile {messages = [emptyMessage "M"]}
    it "empty message with empty statements" $
      runWithSyntax "message M {;\t;}  ;"
        `shouldParse` emptyFile {messages = [emptyMessage "M"]}
    it "fails if no closing } is found" $
      runParser `shouldFailOn` addSyntaxStatement "message M {"
    it "fails if no name is provided" $
      runParser `shouldFailOn` addSyntaxStatement "message {"

testMessageWithNormalFields :: Spec
testMessageWithNormalFields =
  describe "[Parsing] Messages with normal fields" $ do
    it "message with two normal field" $
      runWithSyntax "message M { foo.Bar nested_message = 2 ;; repeated int32 samples = 4\n; }"
        `shouldParse` emptyFile
          { messages =
              [ ( emptyMessage
                    "M"
                )
                  { normalFields =
                      [ NormalField (FieldDefinition "nested_message" (TMessageType "foo.Bar") 2 []) False,
                        NormalField (FieldDefinition "samples" TInt32 4 []) True
                      ]
                  }
              ]
          }
    it "message with normal field with options" $
      runWithSyntax "message M { sint32 foo = 4 [o1=true,o2=-5.0];\n; string bar = 1 [o3=-9];; }"
        `shouldParse` emptyFile
          { messages =
              [ (emptyMessage "M")
                  { normalFields =
                      [ NormalField (FieldDefinition "foo" TSInt32 4 [("o1", CBoolLit True), ("o2", CFloatLit (-5.0))]) False,
                        NormalField (FieldDefinition "bar" TString 1 [("o3", CIntLit (-9))]) False
                      ]
                  }
              ]
          }
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
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {oneOfFields = [OneOfField "foo" [] []]}]}
    it "message with oneof field with only empty statements" $
      runWithSyntax "message M {\toneof foo\n {;;}}"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {oneOfFields = [OneOfField "foo" [] []]}]}
    it "message with oneof and two simple fields" $
      runWithSyntax "message M {\toneof foo\n {string name = 4;;; bytes b = 5;}}"
        `shouldParse` emptyFile
          { messages =
              [ (emptyMessage "M")
                  { oneOfFields =
                      [ OneOfField
                          "foo"
                          [ FieldDefinition "name" TString 4 [],
                            FieldDefinition "b" TBytes 5 []
                          ]
                          []
                      ]
                  }
              ]
          }
    it "message with oneof field with and option and a field" $
      runWithSyntax "message M {\toneof foo\n {sfixed32 b = 5; option opt = 'value'; }}"
        `shouldParse` emptyFile
          { messages =
              [ (emptyMessage "M")
                  { oneOfFields =
                      [ OneOfField
                          "foo"
                          [FieldDefinition "b" TSfixed32 5 []]
                          [("opt", CStringLit "value")]
                      ]
                  }
              ]
          }
    it "message with oneof with field with options" $
      runWithSyntax "message M {\toneof foo\n {string name = 4[o1=true,o2=-5.0];;;}}"
        `shouldParse` emptyFile
          { messages =
              [ (emptyMessage "M")
                  { oneOfFields =
                      [ OneOfField
                          "foo"
                          [ FieldDefinition
                              "name"
                              TString
                              4
                              [("o1", CBoolLit True), ("o2", CFloatLit (-5.0))]
                          ]
                          []
                      ]
                  }
              ]
          }
    it "fails if field is repeated" $
      runParser `shouldFailOn` addSyntaxStatement "message M {oneof foo {repeated string name = 4;}}"

testMessageWithOptions :: Spec
testMessageWithOptions =
  describe "[Parsing] Messages with Options" $ do
    it "message with two options" $
      runWithSyntax "message M { option (my_option).a = 42;;; option b = false;; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {options = [("(my_option).a", CIntLit 42), ("b", CBoolLit False)]}]}
    it "fails if option is not correct" $
      runParser `shouldFailOn` addSyntaxStatement "message M { option my_option = 42 }"

testMessageWithMapFields :: Spec
testMessageWithMapFields =
  describe "[Parsing] Messages with map fields" $ do
    it "message with simple map field" $
      runWithSyntax "message M { map< string , Project > projects\n =   3; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {mapFields = [MapField "projects" KTString (TMessageType "Project") 3 []]}]}
    it "message with options" $
      runWithSyntax "message M { map<bool, bool> projects = 3 [o1=42,o2=false]; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {mapFields = [MapField "projects" KTBool TBool 3 [("o1", CIntLit 42), ("o2", CBoolLit False)]]}]}
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
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {enumDefs = [EnumDefinition "Enum" [] [EnumField "A" 0 [], EnumField "B" 1 []]]}]}
    it "message with two enums and one opt" $
      runWithSyntax "message M { enum E1 {A = 0;};\t; option o = foo;  enum E2 {}}"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {enumDefs = [EnumDefinition "E1" [] [EnumField "A" 0 []], EnumDefinition "E2" [] []], options = [("o", CIdentifier "foo")]}]}
    it "fails if option is not correct" $
      runParser `shouldFailOn` addSyntaxStatement "message M { enum Enum {}"

testMessageWithReservedFieldNumbers :: Spec
testMessageWithReservedFieldNumbers =
  describe "[Parsing] Messages with reserved field numbers" $ do
    it "message with single reserved field number" $
      runWithSyntax "message M { reserved 5;;; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNumbers [FSSingle 5]]}]}
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 5, 7,   7 ; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNumbers [FSSingle 5, FSSingle 7, FSSingle 7]]}]}
    it "message with reserved field number range" $
      runWithSyntax "message M { reserved 5 to 9; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNumbers [FSRange 5 9]]}]}
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 5 to 9, 7, 8 to 11, 0; reserved 7 to 4, 42; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNumbers [FSRange 5 9, FSSingle 7, FSRange 8 11, FSSingle 0], RFNumbers [FSRange 7 4, FSSingle 42]]}]}
    it "fails if ; is missing" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved 9}"
    it "fails on empty list" $
      runParser `shouldFailOn` addSyntaxStatement "message M { reserved ;}"

testMessageWithReservedFieldNames :: Spec
testMessageWithReservedFieldNames =
  describe "[Parsing] Messages with reserved field names" $ do
    it "message with single reserved field number" $
      runWithSyntax "message M { reserved \"foo\" ;;  ; reserved 8; reserved 'bla'; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNames ["foo"], RFNumbers [FSSingle 8], RFNames ["bla"]]}]}
    it "message with multiple reserved field numbers" $
      runWithSyntax "message M { reserved 'foo', \"bar\"; ;;; \t\rreserved \"qux\"\t; }"
        `shouldParse` emptyFile {messages = [(emptyMessage "M") {reservedFields = [RFNames ["foo", "bar"], RFNames ["qux"]]}]}
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
        `shouldParse` emptyFile {messages = [(emptyMessage "Outer") {messageDefs = [emptyMessage "Inner"]}]}
    it "multiple nested messages" $
      runWithSyntax "message O1 { message Inner{} ;} \n; message O2{}"
        `shouldParse` emptyFile {messages = [(emptyMessage "O1") {messageDefs = [emptyMessage "Inner"]}, emptyMessage "O2"]}

spec :: Spec
spec = do
  testSyntaxDefinition
  testEmptyStatement
  testPackageSpecification
  testImportStatements
  testOptionDefinition
  testEnumDefinition
  testEmptyMessage
  testMessageWithNormalFields
  testMessageWithOneOfFields
  testMessageWithOptions
  testMessageWithMapFields
  testMessageWithEnums
  testMessageWithReservedFieldNumbers
  testMessageWithReservedFieldNames
  testNestedMessages
