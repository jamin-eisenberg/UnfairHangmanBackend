module GuessRequest
  ( GuessRequest(..)
  , RawGuessRequest(..)
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Combinators ((.:?))
import Data.Maybe (Maybe, isJust)
import Word (Word)

data RawGuessRequest = RawGuessRequest { nice :: Boolean, guessingLetter :: String, previouslyIncorrectLetters :: Array String, wordSoFar :: String } | RawGiveUpRequest { previouslyIncorrectLetters :: Array String, wordSoFar :: String }

data GuessRequest
  = GuessRequest { nice :: Boolean, guessingLetter :: Char, previouslyIncorrectLetters :: Array Char, wordSoFar :: Word } | GiveUpRequest { previouslyIncorrectLetters :: Array Char, wordSoFar :: Word }

instance DecodeJson RawGuessRequest where
  decodeJson json = do
    decoded <- decodeJson json
    previouslyIncorrectLetters <- decoded .: "previouslyIncorrectLetters"
    wordSoFar <- decoded .: "wordSoFar"
    guessingLetterMaybe :: Maybe String <- decoded .:? "guessingLetter"
    if isJust guessingLetterMaybe
    then do
          guessingLetter <- decoded .: "guessingLetter"
          nice <- decoded .: "nice"
          pure $ RawGuessRequest { nice, guessingLetter, previouslyIncorrectLetters, wordSoFar } 
    else do
          pure $ RawGiveUpRequest { previouslyIncorrectLetters, wordSoFar }