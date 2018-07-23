import Test.Hspec
import Data.LanguageCodes
import Prelude hiding (Ordering(..))

main = hspec $
  describe "Data.LanguageCodes" $ do
    describe "ISO639_1" $ do
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
      it "should return country codes for a given ISO639_1" $
        toChars AA `shouldBe` ('a', 'a')
    
    describe "fromChars" $ do
      it "should return the ISO639_1 for a given country code" $
        fromChars 'a' 'a' `shouldBe` Just AA
      
      it "should return Nothing if the code given is not an ISO639_1 code" $
        fromChars 'a' 'c' `shouldBe` (Nothing :: Maybe ISO639_1)
    
    describe "language" $
      it "should return the language spoken in country represented by an ISO639_1" $
        language AA `shouldBe` "Afar"
    
