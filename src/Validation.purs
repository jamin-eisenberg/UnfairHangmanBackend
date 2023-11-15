module Validation
  ( validateGiveUp
  , validateGuess
  ) where

import Prelude
import Data.Array (any, catMaybes, elem, filter, intersect, length, null)
import Data.Either (Either(..), note)
import Data.Maybe (isNothing)
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.String.CodeUnits (toChar, toCharArray)
import Game (Game(..))
import GiveUpRequest (GiveUpRequest(..), RawGiveUpRequest(..))
import GuessRequest (GuessRequest(..), RawGuessRequest(..), fromString)
import GuessResponse (GuessError(..))
import Utils (isAsciiLetter, leftIf, mapLeft)
import Word (Word, isWon, mkWord)

validateIncorrectLettersAndWordSoFar ::
  forall r.
  Int ->
  { rawPreviouslyIncorrectLetters :: Array String
  , rawWordSoFar :: String
  | r
  } ->
  Either GuessError
    { previouslyIncorrectLetters :: Array Char
    , wordSoFar :: Word
    }
validateIncorrectLettersAndWordSoFar gameWordLength { rawPreviouslyIncorrectLetters
, rawWordSoFar
} = do
  let
    maybePreviouslyIncorrectLetters = toChar <$> rawPreviouslyIncorrectLetters
  previouslyIncorrectLetters <-
    if any isNothing maybePreviouslyIncorrectLetters then
      Left $ StringsShouldBeChars $ filter (\s -> String.length s /= 1) rawPreviouslyIncorrectLetters
    else
      pure $ catMaybes maybePreviouslyIncorrectLetters
  wordSoFar <- mapLeft WordSoFarMalformed $ mkWord $ toCharArray rawWordSoFar
  let
    incorrectLettersInWordSoFar = intersect previouslyIncorrectLetters $ toCharArray rawWordSoFar
  leftIf (not $ null incorrectLettersInWordSoFar) (IncorrectLettersInWordSoFar incorrectLettersInWordSoFar)
  leftIf (isWon wordSoFar) AlreadyWon
  leftIf (gameWordLength /= length (unwrap wordSoFar)) $ WordSoFarWrongLength gameWordLength (length $ unwrap wordSoFar)
  pure { wordSoFar, previouslyIncorrectLetters }

validateGiveUp :: Game -> RawGiveUpRequest -> Either GuessError GiveUpRequest
validateGiveUp (Game { wordLength }) ( RawGiveUpRequest
    { previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
  , wordSoFar: rawWordSoFar
  }
) = do
  { wordSoFar, previouslyIncorrectLetters } <- validateIncorrectLettersAndWordSoFar wordLength { rawPreviouslyIncorrectLetters, rawWordSoFar }
  pure $ GiveUpRequest { previouslyIncorrectLetters, wordSoFar }

validateGuess :: Game -> RawGuessRequest -> Either GuessError GuessRequest
validateGuess (Game { wordLength }) ( RawGuessRequest
    { guessingLetter: rawGuessingLetter
  , previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
  , wordSoFar: rawWordSoFar
  , mode: rawMode
  }
) = do
  guessingLetter <- note (StringsShouldBeChars [ rawGuessingLetter ]) $ toChar rawGuessingLetter
  mode <- fromString (wrap rawMode)
  { wordSoFar, previouslyIncorrectLetters } <- validateIncorrectLettersAndWordSoFar wordLength { rawPreviouslyIncorrectLetters, rawWordSoFar }
  leftIf (not isAsciiLetter guessingLetter) (GuessingLetterNotAlpha guessingLetter)
  leftIf (guessingLetter `elem` previouslyIncorrectLetters || guessingLetter `elem` toCharArray rawWordSoFar)
    GuessingLetterHasBeenGuessedPreviously
  pure $ GuessRequest { guessingLetter, previouslyIncorrectLetters, wordSoFar, mode }
