{-# LANGUAGE ImportQualifiedPost #-}

import Test.Hspec

import ParserSpec qualified
import Integration.Spec qualified

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "Unit Tests" ParserSpec.spec
  describe "Integration Tests" Integration.Spec.spec
