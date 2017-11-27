module PrettifySpec where

import Prettify
import Test.Hspec

spec :: Spec
spec =  do
  describe "Pretty printing JSON" $ do
    it "String shoud be converted to Doc with quotes" $
      string "Hello" `shouldBe` char '\"' <> (foldr (<>) empty (map char "Hello")) <> char '\"'

    it "a <> Empty should return a" $
      string "Hello" <> empty `shouldBe` string "Hello"

    it "Empty <> a should return a" $
      empty <> string "Hello" `shouldBe` string "Hello"

    it "First and last chars of result of enclose should match initial arguments" $
      -- resultString `shouldStartWith` left:[]
        where
          resultString = show (enclose left right racistStuff)
          left = '¿'
          right = '?'
          racistStuff = text "Amigo"

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""

    -- it "String shoud be converted to Doc with quotes" $
    --   string "Hello" `shouldBe` Doc String "\"Hello\""
