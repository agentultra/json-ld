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
      return $ mkTerm reserved `shouldBe` Left (reserved <> " is a JSON-LD keyword")

    it "cannot be the empty Text" $
      mkTerm "" `shouldBe` Left "A term must contain at least one character"

    it "can create terms from Text" $ property $ do
      t <- arbitrary `suchThat` (\w -> (w `notElem` keywords) && w /= T.empty)
      return $ mkTerm t `shouldBe` Right (Term t)

  describe "mkblankobject" $ do

    it "cannot create BlankObject with an empty key" $
      mkBlankObject "" "foobar" `shouldBe` Left "blankObjectId must start with \"_\""

    it "cannot create BlankObject with an invalid key" $ property $ \objId ->
        mkBlankObject ("a" <> objId) "foobar"
          `shouldBe` Left ("a" <> objId <> " is an invalid BlankObject id")

    it "can create BlankObject with a valid key" $ property $ \objId ->
        mkBlankObject ("_" <> objId) "foobar"
          `shouldBe` (Right $ BlankObject ("_" <> objId) "foobar")
