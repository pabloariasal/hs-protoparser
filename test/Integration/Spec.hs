module Integration.Spec (spec) where

import HSProtoParser.Ast
import HSProtoParser.Parser (parseProto)
import Test.Hspec

expectedFileElements :: ProtoFile
expectedFileElements =
  [ SyntaxStmt "proto3",
    PackageSpec "foo.bar",
    PackageSpec "qux",
    ImportStmt (Just Public) "other.proto",
    EnumDef $
      EnumDefinition
        "EnumAllowingAlias"
        [ EnOpt ("allow_alias", CBoolLit True),
          EnField $ EnumField "UNKNOWN" 0 [],
          EnField $ EnumField "STARTED" 1 [],
          EnField $
            EnumField
              "RUNNING"
              2
              [("(custom_option)", CStringLit "hello world"), ("my_opt", CIntLit 42)]
        ],
    OptionDef ("java_package", CStringLit "com.example.foo"),
    MsgDef $
      MessageDefinition
        "Outer"
        []
        [MapField "my_map" KTInt32 TBytes 4 [("i", CIntLit (-90))]]
        [ NormalField (FieldDefinition "inner_message" (TMessageType "Inner") 2 [("r", CIdentifier "foo")]) True,
          NormalField (FieldDefinition "enum_field" (TMessageType "EnumAllowingAlias") 3 []) False
        ]
        [ MessageDefinition
            "Inner"
            [ OneOfField
                "foo"
                [ OFFieldDef $ FieldDefinition "name" TString 4 [("a", CBoolLit True)],
                  OFOptDef ("java", CFloatLit 5.0),
                  OFFieldDef $ FieldDefinition "sub_message" (TMessageType "SubMessage") 9 []
                ]
            ]
            []
            [NormalField (FieldDefinition "ival" TInt64 1 []) False]
            []
            []
            []
            []
        ]
        [EnumDefinition "MyEnum" [EnField $ EnumField "BLA" 2 []]]
        [("(my_option).a", CBoolLit True)]
        [RFNumbers [FSSingle 9, FSRange 9 11, FSSingle 42], RFNames ["foo", "bar"]]
  ]

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "Integration" $ do
    it "parse succeeds on a valid proto file" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right expectedFileElements
