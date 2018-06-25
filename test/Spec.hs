import Data.JSON.LinkedData
import Protolude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

main :: IO ()
main = hspec $ do
  describe "mkTerm" $ do

    it "cannot create keyword-restricted Terms" $ property $ do
      reserved <- elements keywords
      return $ (mkTerm reserved) `shouldBe` Left (reserved <> " is a JSON-LD keyword")

    it "can create terms from Text" $ property $ do
      t <- arbitrary `suchThat` (\w -> not $ w `elem` keywords)
      return $ (mkTerm t) `shouldBe` Right (Term t)
