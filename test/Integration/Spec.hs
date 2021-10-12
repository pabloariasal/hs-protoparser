module Integration.Spec (spec) where

import HSProtoParser.Ast
import HSProtoParser.Parser (parseProto)
import Test.Hspec

expectedSyntax :: SyntaxStatement
expectedSyntax = "proto3"

expectedPackageSpecs :: [PackageSpecification]
expectedPackageSpecs = ["foo.bar", "qux"]

expectedImports :: [ImportStatement]
expectedImports = [ImportStatement (Just Public) "other.proto"]

expectedOptions :: [OptionDefinition]
expectedOptions = [("java_package", CStringLit "com.example.foo")]

expectedEnums :: [EnumDefinition]
expectedEnums = [EnumDefinition "EnumAllowingAlias" [("allow_alias", CBoolLit True)] [EnumField "UNKNOWN" 0 [], EnumField "STARTED" 1 [], EnumField "RUNNING" 2 [("(custom_option)", CStringLit "hello world"), ("my_opt", CIntLit 42)]]]

expectedInnerMessage :: MessageDefinition
expectedInnerMessage =
  MessageDefinition
    "Inner"
    [ OneOfField
        "foo"
        [ FieldDefinition "name" TString 4 [("a", CBoolLit True)],
          FieldDefinition "sub_message" (TMessageType "SubMessage") 9 []
        ]
        [("java", CFloatLit 5.0)]
    ]
    []
    [NormalField (FieldDefinition "ival" TInt64 1 []) False]
    []
    []
    []
    []

expectedMessages :: [MessageDefinition]
expectedMessages =
  [ MessageDefinition
      "Outer"
      []
      [MapField "my_map" KTInt32 TBytes 4 [("i", CIntLit (-90))]]
      [ NormalField (FieldDefinition "inner_message" (TMessageType "Inner") 2 [("r", CIdentifier "foo")]) True,
        NormalField (FieldDefinition "enum_field" (TMessageType "EnumAllowingAlias") 3 []) False
      ]
      [expectedInnerMessage]
      [EnumDefinition "MyEnum" [] [EnumField "BLA" 2 []]]
      [("(my_option).a", CBoolLit True)]
      [RFNumbers [FSSingle 9, FSRange 9 11, FSSingle 42], RFNames ["foo", "bar"]]
  ]

expectedServices :: [ServiceDefinition]
expectedServices = []

expectedProtoFile :: ProtoFile
expectedProtoFile = ProtoFile expectedSyntax expectedPackageSpecs expectedImports expectedOptions expectedMessages expectedEnums expectedServices

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "Integration" $ do
    it "parse succeeds on a valid proto file" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right expectedProtoFile
