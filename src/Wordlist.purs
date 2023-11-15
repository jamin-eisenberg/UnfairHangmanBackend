module Wordlist
  ( getEligibleWords
  , pickRandomUnfairWord
  ) where

import Prelude
import Data.Array (any, filter, groupBy, head, last, length, null, sortWith)
import Data.Array.NonEmpty (fromArray, toNonEmpty)
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.String.CodeUnits (singleton, toCharArray)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import GuessRequest (GuessRequest(..), isUnfair)
import GuessResponse (GuessError(..), GuessResponse(..), GuessResponseMessage(..))
import Partial.Unsafe (unsafePartial)
import Utils (countOccurrences, indexWrapping, occurrenceIndices)
import Word (Word, isWon, revealLetterInWord, wordMatches)

type Wordlist
  = Array String

type WordLength
  = Int

pickRandomUnfairWord :: GuessRequest -> Wordlist -> Aff GuessResponse
pickRandomUnfairWord g wl = do
  randomPick <- liftEffect $ randomInt 0 10000
  pure $ evaluateGuess g wl randomPick

-- TODO split up
getEligibleWords :: Word -> Array Char -> Array String -> Array String
getEligibleWords wordSoFar previouslyIncorrectLetters =
  let
    correctLengthWords word = String.length word == length (unwrap wordSoFar)

    wordContainsAny cs str = any (\c -> contains (Pattern $ singleton c) str) cs

    wordsNotContainingPreviouslyIncorrectLetters = not <<< (wordContainsAny previouslyIncorrectLetters)

    correctlyFilledWords = flip wordMatches wordSoFar
  in
    filter (\word -> correctLengthWords word && wordsNotContainingPreviouslyIncorrectLetters word && correctlyFilledWords word)

evaluateGuess :: GuessRequest -> Wordlist -> Int -> GuessResponse
evaluateGuess (GuessRequest guess) wordlist toPick =
  let
    eligibleWords = getEligibleWords guess.wordSoFar guess.previouslyIncorrectLetters wordlist

    guessingLetterOccurrences = (countOccurrences guess.guessingLetter) <<< toCharArray

    fewestRevealedLetters = sortWith guessingLetterOccurrences eligibleWords

    fewestRevealedLettersGrouped = groupBy (eq `on` guessingLetterOccurrences) fewestRevealedLetters

    response = do
      -- TODO isUnfair is not correct
      goodUnfairWords <- (if isUnfair guess.mode then last else head) fewestRevealedLettersGrouped
      let
        wordToPlay = indexWrapping goodUnfairWords toPick

        revealedLetterPositions = occurrenceIndices guess.guessingLetter (toCharArray wordToPlay)

        isCorrect = not $ null revealedLetterPositions

        newWordSoFar = revealLetterInWord guess.guessingLetter wordToPlay guess.wordSoFar

        message =
          if isCorrect then
            if isWon newWordSoFar then
              Win $ show newWordSoFar
            else
              Correct guess.guessingLetter $ toNonEmpty $ unsafePartial $ fromJust $ fromArray revealedLetterPositions
          else
            Incorrect guess.guessingLetter
      pure
        $ GuessResponse
            { message
            , incorrectLetters: guess.previouslyIncorrectLetters
            , wordSoFar: newWordSoFar
            }
  in
    fromMaybe
      ( GuessResponse
          { message: InvalidGuess [ ImpossibleWordSoFar ]
          , incorrectLetters: guess.previouslyIncorrectLetters
          , wordSoFar: guess.wordSoFar
          }
      )
      response
