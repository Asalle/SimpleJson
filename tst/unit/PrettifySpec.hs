module PrettifySpec where

import Prettify
import Test.Hspec

spec :: Spec
spec =  do
  describe "Pretty printing JSON" $ do
    it "String shoud be converted to Doc with quotes" $
      string "Hello" `shouldBe` char '\"' <> (foldr (<>) empty (map char "Hello")) <> char '\"'

    -- it "String shoud be converted to Doc with no quotes" $
    --   text "Hello" `shouldBe` Doc Text "Hello"

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""
