module Data.LanguageCodes.ISO639_2_Spec where

import Test.Hspec
import Data.LanguageCodes.ISO639_2

spec :: Spec
spec =
  describe "Data.LanguageCodes.ISO639_2" $ do
    it "should be instance of Show" $ 
      show AAR `shouldBe` "AAR"
    
    it "should be instance of Read" $
      read "AAR" `shouldBe` AAR
    
    it "should be instance of Eq" $
      AAR == AAR `shouldBe` True
    
    it "should be instance of Enum" $
      succ AAR `shouldBe` ABK
    
    it "should be instance of Ord" $
      AAR < ABK `shouldBe` True
    
    describe "toChars" $
      it "should return language codes for a given ISO639_2" $
        toChars AAR `shouldBe` ('a', 'a', 'r')

    describe "fromChars" $ do
      it "should return the ISO639_2 for a given language code" $
        fromChars ('a', 'a', 'r') `shouldBe` Just AAR
      
      it "should return Nothing if the code given is not an ISO639_2 code" $
        fromChars ('a', 'b', 'c') `shouldBe` (Nothing :: Maybe ISO639_2)

    describe "language" $
      it "should return the first language (in alphabetical) represented by ISO639_2" $
        language ADY `shouldBe` "Adyghe"