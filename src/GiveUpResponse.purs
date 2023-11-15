module GiveUpResponse
  ( GiveUpResponse(..)
  ) where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Maybe (Maybe)

newtype GiveUpResponse
  = GiveUpResponse { secretWord :: Maybe String, eligibleWords :: Array String }

instance EncodeJson GiveUpResponse where
  encodeJson (GiveUpResponse giveUp) = encodeJson giveUp