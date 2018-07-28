module Data.JSON.LinkedData.Term where

import Protolude

newtype Term = Term Text
  deriving (Eq, Generic, Show)

keywords =
  [ "@context"
  , "@id"
  , "@value"
  , "@language"
  , "@type"
  , "@container"
  , "@list"
  , "@set"
  , "@reverse"
  , "@index"
  , "@base"
  , "@vocab"
  , "@graph"
  , ":"
  ]

mkTerm :: Text -> Either Text Term
mkTerm "" = Left "A term must contain at least one character"
mkTerm word = if elem word keywords
              then Left $ word <> " is a JSON-LD keyword"
              else Right $ Term word
