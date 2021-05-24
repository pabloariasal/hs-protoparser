module Integration.Spec (spec) where

import HSProtoParser.Ast
import HSProtoParser.Parser (parseProto)
import Test.Hspec

expectedFileElements :: ProtoFile
expectedFileElements =
  [ SyntaxStmt "proto3",
    PackageSpec "foo.bar",
    PackageSpec "qux",
    ImportStmt $
      ImportStatement (Just Public) "other.proto",
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
    OptionDef ("java_package", StringLit "com.example.foo")
  ]

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "Integration" $ do
    it "parse succeeds on a valid proto file" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right expectedFileElements
