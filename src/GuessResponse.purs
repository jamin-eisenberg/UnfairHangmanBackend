module GuessResponse
  ( GuessError(..)
  , GuessResponse(..)
  , GuessResponseMessage(..)
  )
  where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, fromString)
import Data.Array.NonEmpty (fromNonEmpty, init, last)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (joinWith)
import Data.String.CodeUnits (singleton)
import Word (Word)

data GuessError = GuessingLetterNotAlpha Char | GuessingLetterHasBeenGuessedPreviously | IncorrectLettersInWordSoFar (Array Char) | WordSoFarMalformed (Array String) | WordSoFarWrongLength Int Int | AlreadyWon | ImpossibleWordSoFar | StringsShouldBeChars (Array String) | InvalidMode

instance Show GuessError where
  show e = case e of
            GuessingLetterNotAlpha c -> "The guessingLetter must be a letter ([a-z][A-Z]). Found '" <> singleton c <> "'"
            GuessingLetterHasBeenGuessedPreviously -> "The guessingLetter has already been guessed."
            IncorrectLettersInWordSoFar cs -> "Letter(s) that are marked as being previously incorrect exist in the wordSoFar: " <> show cs
            WordSoFarMalformed es -> "The following errors occurred while processing the hangman string: " <> show es
            AlreadyWon -> "The game has already been won."
            ImpossibleWordSoFar -> "There are no words in the (pretty lenient) dictionary that fit the wordSoFar."
            StringsShouldBeChars ss -> "Found a string of length != 1 when this string should represent a char: " <> show ss <> "."
            WordSoFarWrongLength expected actual -> "Expected wordSoFar to be " <> show expected <> " letters long, but it was " <> show actual <> " letters long."
            InvalidMode -> "Expected one of nice|mean|normal"

data GuessResponseMessage = Win String | Incorrect Char | Correct Char (NonEmpty Array Int) | InvalidGuess (Array GuessError)

instance Show GuessResponseMessage where
  show msg = case msg of
                Win winningWord -> "You guessed the word correctly! It was '" <> winningWord <> "'"
                Incorrect guessed -> "The letter " <> singleton guessed <> " is not in the word."
                Correct guessed positions -> "The letter " <> singleton guessed <> " is at " <> positionsToMessage positions
                InvalidGuess errors -> "The following error(s) occurred: " <> show errors

instance EncodeJson GuessResponseMessage where
  encodeJson msg = fromString $ show msg

positionsToMessage :: NonEmpty Array Int -> String
positionsToMessage ps = case ps of 
                          (p :| []) -> "position " <> show p
                          (p1 :| [p2]) -> "positions " <> show p1 <> " and " <> show p2
                          _ -> "positions " <> joinWith ", " (show <$> init nePs) <> ", and " <> show (last $ nePs)
                            where nePs = fromNonEmpty ps


data GuessResponse = GuessResponse { message :: GuessResponseMessage, incorrectLetters :: Array Char, wordSoFar :: Word }

instance EncodeJson GuessResponse where
  encodeJson (GuessResponse guess) = encodeJson guess