module Data.JSON.LinkedData.Internal where

import qualified Data.Map as M
import           Iri.Data
import           Protolude

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

newtype ContainerMap =
  ContainerMap
  { unContainerMap :: Map Text Text
  }
  deriving (Eq, Generic, Show)

data ContextError
  = ContextDereferenceError Text
  | ContextInvalidError Text
  deriving (Eq, Generic, Show)

newtype IriMap =
  IriMap
  { _unIriMap :: Map Term Iri
  }
  deriving (Eq, Generic, Show)

newtype LanguageMap =
  LanguageMap
  { unLanguageMap :: Map Text Text
  }
  deriving (Eq, Generic, Show)

newtype Term = Term Text
  deriving (Eq, Generic, Show)

data TermDefinition =
  TermDefinition
  { _termDefinitionIriMap       :: IriMap
  , _termDefinitionReverse      :: Bool
  , _termDefinitionTypeMap      :: Maybe TypeMap
  , _termDefinitionLanguageMap  :: Maybe LanguageMap
  , _termDefinitionContext      :: Maybe Context
  , _termDefinitionNextVal      :: Maybe Text
  , _termDefinitionPrefix       :: Maybe Bool
  , _termDefinitionContainerMap :: Maybe ContainerMap
  }
  deriving (Eq, Generic, Show)

mkTerm :: Text -> Either Text Term
mkTerm "" = Left "A term must contain at least one character"
mkTerm word = if word `elem` keywords
              then Left $ word <> " is a JSON-LD keyword"
              else Right $ Term word

newtype TermDefinitions =
  TermDefinitions
  { unTermDefinitions :: Map Term TermDefinition
  }
  deriving (Eq, Generic, Show)

newtype TypeMap =
  TypeMap
  { _unTypeMap :: Map Text Text
  }
  deriving (Eq, Generic, Show)

newtype Vocabulary =
  Vocabulary
  { unVocabulary :: Map Text Text
  }
  deriving (Eq, Generic, Show)

data Context =
  Context
  { _contextBaseIri     :: Iri
  , _contextDefaultLang :: Text
  , _contextTerms       :: TermDefinitions
  , _contextVocabulary  :: Vocabulary
  }
  deriving (Eq, Generic, Show)
