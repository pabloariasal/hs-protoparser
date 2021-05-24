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
      ImportStatement (Just Public) "other.proto"
  ]

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "Integration" $ do
    it "parse succeeds on a valid proto file" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right expectedFileElements
