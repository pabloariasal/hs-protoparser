{-# LANGUAGE ImportQualifiedPost #-}

import Integration.Spec qualified
import ParserSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "Unit Tests" ParserSpec.spec
  describe "Integration Tests" Integration.Spec.spec
