module Integration.Spec (spec)
  where

import Test.Hspec
import HSProtoParser.Parser (parseProto)
import HSProtoParser.Ast

packageSpecifications :: [PackageSpecification]
packageSpecifications = ["foo.bar", "qux"]

importStatements :: [ImportStatement]
importStatements = []

optionDefinitions :: [OptionDefinition]
optionDefinitions = []

topLevelDefinitions :: [TopLevelDefinition]
topLevelDefinitions = []

spec :: Spec
spec = before (readFile "test/Integration/test_file.proto") $ do
  describe "absolute" $ do
    it "returns the original number when given a positive input" $ \proto ->
      do
        parseProto "" proto `shouldBe` Right (ProtoFile "proto3" packageSpecifications importStatements optionDefinitions topLevelDefinitions)
