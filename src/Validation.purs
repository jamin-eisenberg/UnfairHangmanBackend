module Validation
  ( validateGuess
  ) where

import Prelude
import Data.Array (any, catMaybes, elem, filter, intersect, null)
import Data.Either (Either(..), note)
import Data.Maybe (isNothing)
import Data.String as String
import Data.String.CodeUnits (toChar, toCharArray)
import GuessRequest (GuessRequest(..), RawGuessRequest(..))
import GuessResponse (GuessError(..))
import Utils (isAsciiLetter, leftIf, mapLeft)
import Word (Word, isWon, mkWord)

validateIncorrectLettersAndWordSoFar ::
  forall r.
  { rawPreviouslyIncorrectLetters :: Array String
  , rawWordSoFar :: String
  | r
  } ->
  Either GuessError
    { previouslyIncorrectLetters :: Array Char
    , wordSoFar :: Word
    }
validateIncorrectLettersAndWordSoFar { rawPreviouslyIncorrectLetters
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
  pure { wordSoFar, previouslyIncorrectLetters }

validateGuess :: RawGuessRequest -> Either GuessError GuessRequest
validateGuess ( RawGuessRequest
    { guessingLetter: rawGuessingLetter
  , previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
  , wordSoFar: rawWordSoFar
  , nice
  }
) = do
  guessingLetter <- note (StringsShouldBeChars [ rawGuessingLetter ]) $ toChar rawGuessingLetter
  { wordSoFar, previouslyIncorrectLetters } <- validateIncorrectLettersAndWordSoFar { rawPreviouslyIncorrectLetters, rawWordSoFar }
  leftIf (not isAsciiLetter guessingLetter) (GuessingLetterNotAlpha guessingLetter)
  leftIf (guessingLetter `elem` previouslyIncorrectLetters || guessingLetter `elem` toCharArray rawWordSoFar)
    GuessingLetterHasBeenGuessedPreviously
  pure $ GuessRequest { guessingLetter, previouslyIncorrectLetters, wordSoFar, nice }

validateGuess ( RawGiveUpRequest
    { previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
  , wordSoFar: rawWordSoFar
  }
) = validateIncorrectLettersAndWordSoFar { rawPreviouslyIncorrectLetters, rawWordSoFar } >>= pure <<< GiveUpRequest
