module GuessRequest
  ( GameMode(..)
  , GuessRequest(..)
  , RawGuessRequest(..)
  , fromString
  , isUnfair
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Combinators ((.:?))
import Data.Either (Either(..))
import Data.Maybe (Maybe, isJust)
import Data.Newtype (wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import GuessResponse (GuessError(..))
import Word (Word)

data GameMode = Nice | Mean | Normal

fromString :: CaseInsensitiveString -> Either GuessError GameMode
fromString s 
 | s == wrap "nice" = pure Nice
 | s == wrap "mean" = pure Mean
 | s == wrap "normal" = pure Normal
 | otherwise= Left InvalidMode

isUnfair :: GameMode -> Boolean
isUnfair g = case g of
              Nice -> true
              Mean -> true
              Normal -> false

data RawGuessRequest = RawGuessRequest { mode :: String, guessingLetter :: String, previouslyIncorrectLetters :: Array String, wordSoFar :: String } | RawGiveUpRequest { previouslyIncorrectLetters :: Array String, wordSoFar :: String }

data GuessRequest
  = GuessRequest { mode :: GameMode, guessingLetter :: Char, previouslyIncorrectLetters :: Array Char, wordSoFar :: Word } | GiveUpRequest { previouslyIncorrectLetters :: Array Char, wordSoFar :: Word }

instance DecodeJson RawGuessRequest where
  decodeJson json = do
    decoded <- decodeJson json
    previouslyIncorrectLetters <- decoded .: "previouslyIncorrectLetters"
    wordSoFar <- decoded .: "wordSoFar"
    guessingLetterMaybe :: Maybe String <- decoded .:? "guessingLetter"
    if isJust guessingLetterMaybe
    then do
          guessingLetter <- decoded .: "guessingLetter"
          mode <- decoded .: "mode"
          pure $ RawGuessRequest { mode, guessingLetter, previouslyIncorrectLetters, wordSoFar } 
    else do
          pure $ RawGiveUpRequest { previouslyIncorrectLetters, wordSoFar }