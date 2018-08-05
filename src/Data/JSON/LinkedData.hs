module Data.JSON.LinkedData
  ( BlankObject (..)
  , Term (..)
  , keywords
  , mkBlankObject
  , mkTerm
  ) where

import Control.Error
import Data.JSON.LinkedData.Graph
import Data.JSON.LinkedData.Internal
import Protolude

processContext :: Context -> [Context] -> [Context] -> Either ContextError Context
processContext activeContext localContext remoteContext =
  Left $ ContextInvalidError "Not Implemented yet"
