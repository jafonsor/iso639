module Data.LanguageCodes.ISO639_1_Spec where

import Test.Hspec
import Data.LanguageCodes.ISO639_1

spec :: Spec
spec =
  describe "Data.LanguageCodes.ISO639_1" $ do
    it "should be instance of Show" $ 
      show AA `shouldBe` "AA"
    
    it "should be instance of Read" $
      read "AA" `shouldBe` AA
    
    it "should be instance of Eq" $
      AA == AA `shouldBe` True
    
    it "should be instance of Enum" $
      succ AA `shouldBe` AB
    
    it "should be instance of Ord" $
      AA < AB `shouldBe` True
    
    describe "toChars" $
      it "should return the language code for a given ISO639_1" $
        toChars AA `shouldBe` ('a', 'a')

    describe "fromChars" $ do
      it "should return the ISO639_1 for a given language code" $
        fromChars 'a' 'a' `shouldBe` Just AA
      
      it "should return Nothing if the code given is not an ISO639_1 code" $
        fromChars 'a' 'c' `shouldBe` (Nothing :: Maybe ISO639_1)

    describe "language" $
      it "should return the language spoken in language represented by an ISO639_1" $
        language AA `shouldBe` "Afar"