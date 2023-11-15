module GiveUpRequest
  ( GiveUpRequest(..)
  , RawGiveUpRequest(..)
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Word (Word)

newtype RawGiveUpRequest
  = RawGiveUpRequest { previouslyIncorrectLetters :: Array String, wordSoFar :: String }

newtype GiveUpRequest
  = GiveUpRequest { previouslyIncorrectLetters :: Array Char, wordSoFar :: Word }

instance DecodeJson RawGiveUpRequest where
  decodeJson json = do
    decoded <- decodeJson json
    previouslyIncorrectLetters <- decoded .: "previouslyIncorrectLetters"
    wordSoFar <- decoded .: "wordSoFar"
    pure $ RawGiveUpRequest { previouslyIncorrectLetters, wordSoFar } 