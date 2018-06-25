import           Data.JSON.LinkedData
import qualified Data.Text as T
import           Protolude
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text

main :: IO ()
main = hspec $ do
  describe "mkTerm" $ do

    it "cannot create keyword-restricted Terms" $ property $ do
      reserved <- elements keywords
      return $ (mkTerm reserved) `shouldBe` Left (reserved <> " is a JSON-LD keyword")

    it "cannot be the empty Text" $
      (mkTerm "") `shouldBe` Left "A term must contain at least one character"

    it "can create terms from Text" $ property $ do
      t <- arbitrary `suchThat` (\w -> (not $ w `elem` keywords) && w /= T.empty)
      return $ (mkTerm t) `shouldBe` Right (Term t)
