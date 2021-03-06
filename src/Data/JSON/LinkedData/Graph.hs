module Data.JSON.LinkedData.Graph where

import Data.Aeson
import Data.Text as T
import Iri.Data
import Protolude

data Node =
    IriNode         !Iri
  | BlankNode       !BlankObject
  | JsonLDValueNode !ValueObject
  deriving (Eq, Generic, Show)

data BlankObject =
  BlankObject
  { blankObjectId    :: Text
  , blankObjectValue :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON BlankObject
instance FromJSON BlankObject

data ValueObject =
    JsValue Value
  | TypedValue Text
  | LanguageTaggedString Text
  deriving (Eq, Generic, Show)

mkBlankObject :: Text -> Text -> Either Text BlankObject
mkBlankObject "" _ = Left "blankObjectId must start with \"_\""
mkBlankObject id v =
  if validBlankId id
  then Right $ BlankObject id v
  else Left $ id <> " is an invalid BlankObject id"
  where
    validBlankId x = T.head x == '_'
