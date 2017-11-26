module Main where
 
import SimpleJson
import Test.Hspec
  
main = hspec $ do
 
  describe "Validate haqify function" $ do
    it "haqify is supposed to prefix Haq! to things" $ do
      isNull JNull `shouldBe` True