module ProgramSpec where

import Test.Hspec
import Program
import Control.Applicative

main :: IO ()
main = hspec $ do
  describe "parent company name" $ do
    context "when the parent company name exists in the copyright" $ do
      it "finds the parent company name" $ do
        readFile "Fixture.txt" >>= (\html -> findParentCompanyName html `shouldBe` Just "Pivotal Software, Inc.")
