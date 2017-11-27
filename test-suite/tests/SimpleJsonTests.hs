module Main where

import SimpleJson
import Test.Hspec

main = hspec $
  describe "Validate haqify function" $
    it "haqify is supposed to prefix Haq! to things" $
      isNull JNull `shouldBe` True