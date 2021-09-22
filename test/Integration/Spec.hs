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
        [ EnOpt ("allow_alias", BoolLit True),
          EnField $ EnumField "UNKNOWN" 0 [],
          EnField $ EnumField "STARTED" 1 [],
          EnField $
            EnumField
              "RUNNING"
              2
              [("(custom_option)", StringLit "hello world"), ("my_opt", IntLit 42)]
        ],
    OptionDef ("java_package", StringLit "com.example.foo"),
    MsgDef $
      MessageDefinition
        "Outer"
        [ Opt ("(my_option).a", BoolLit True),
          Msg $
            MessageDefinition
              "Inner"
              [ NorF $ NormalField (FieldDefinition "ival" FTInt64 1 []) False,
                OneF $
                  OneOfField
                    "foo"
                    [ OFFieldDef $ FieldDefinition "name" FTString 4 [("a", BoolLit True)],
                      OFOptDef ("java", FloatLit 5.0),
                      OFFieldDef $ FieldDefinition "sub_message" (FTMessageType "SubMessage") 9 []
                    ]
              ],
          RsvFieldNums [Single 9, Range 9 11, Single 42],
          NorF $
            NormalField
              (FieldDefinition "inner_message" (FTMessageType "Inner") 2 [("r", Identifier "foo")])
              True,
          RsvFieldNames ["foo", "bar"],
          NorF $ NormalField (FieldDefinition "enum_field" (FTMessageType "EnumAllowingAlias") 3 []) False,
          MapF $ MapField "my_map" KTInt32 FTBytes 4 [("i", IntLit (-90))],
          Enum $ EnumDefinition "MyEnum" [EnField $ EnumField "BLA" 2 []]
        ]
  ]

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "Integration" $ do
    it "parse succeeds on a valid proto file" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right expectedFileElements
