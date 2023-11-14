module Data.Guess
  ( GuessRequest
  , RawGuessRequest(..)
  , Wordlist
  , getEligibleWords
  , pickRandomUnfairWord
  , validateGuess
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Combinators ((.:?))
import Data.Array (any, catMaybes, elem, filter, groupBy, head, intersect, last, length, null, sortWith)
import Data.Array.NonEmpty (fromArray, toNonEmpty)
import Data.Either (Either(..), note)
import Data.Function (on)
import Data.GuessResponse (GuessError(..), GuessResponse(..), GuessResponseMessage(..))
import Data.Letter (GuessChar, isAsciiLetter)
import Data.Maybe (Maybe, fromJust, fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.String.CodeUnits (singleton, toChar, toCharArray)
import Data.Word (Word, isWon, mkWord, revealLetterInWord, wordMatches)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Partial.Unsafe (unsafePartial)
import Utils (countOccurrences, indexWrapping, leftIf, mapLeft, occurrenceIndices)

data RawGuessRequest = RawGuessRequest { nice :: Boolean, guessingLetter :: String, previouslyIncorrectLetters :: Array String, wordSoFar :: String } | RawGiveUpRequest { previouslyIncorrectLetters :: Array String, wordSoFar :: String }

data GuessRequest
  = GuessRequest { nice :: Boolean, guessingLetter :: GuessChar, previouslyIncorrectLetters :: Array Char, wordSoFar :: Word } | GiveUpRequest { previouslyIncorrectLetters :: Array Char, wordSoFar :: Word }

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

type Wordlist
  = Array String

pickRandomUnfairWord :: GuessRequest -> Wordlist -> Aff GuessResponse
pickRandomUnfairWord g wl = do
  randomPick <- liftEffect $ randomInt 0 10000
  pure $ evaluateGuess g wl randomPick

validateIncorrectLettersAndWordSoFar :: forall r. { rawPreviouslyIncorrectLetters :: Array String
    , rawWordSoFar :: String
    | r
    }
    -> Either GuessError
         { previouslyIncorrectLetters :: Array Char
         , wordSoFar :: Word
         }
validateIncorrectLettersAndWordSoFar {
    rawPreviouslyIncorrectLetters
  , rawWordSoFar} = do
                  let maybePreviouslyIncorrectLetters = toChar <$> rawPreviouslyIncorrectLetters
                  previouslyIncorrectLetters <- if any isNothing maybePreviouslyIncorrectLetters
                                                then Left $ StringsShouldBeChars $ filter (\s -> String.length s /= 1) rawPreviouslyIncorrectLetters
                                                else pure $ catMaybes maybePreviouslyIncorrectLetters

                  wordSoFar <- mapLeft WordSoFarMalformed $ mkWord $ toCharArray rawWordSoFar

                  let incorrectLettersInWordSoFar = intersect previouslyIncorrectLetters $ toCharArray rawWordSoFar
                  leftIf (not $ null incorrectLettersInWordSoFar) (IncorrectLettersInWordSoFar incorrectLettersInWordSoFar)

                  leftIf (isWon wordSoFar) AlreadyWon

                  pure { wordSoFar, previouslyIncorrectLetters }

validateGuess :: RawGuessRequest -> Either GuessError GuessRequest
validateGuess (RawGuessRequest { 
                  guessingLetter: rawGuessingLetter
                , previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
                , wordSoFar: rawWordSoFar
                , nice }) = 
                do

                  guessingLetter <- note (StringsShouldBeChars [rawGuessingLetter]) $ toChar rawGuessingLetter

                  { wordSoFar, previouslyIncorrectLetters } <- validateIncorrectLettersAndWordSoFar { rawPreviouslyIncorrectLetters, rawWordSoFar }

                  leftIf (not isAsciiLetter guessingLetter) (GuessingLetterNotAlpha guessingLetter)
                  leftIf (guessingLetter `elem` previouslyIncorrectLetters || guessingLetter `elem` toCharArray rawWordSoFar)
                   GuessingLetterHasBeenGuessedPreviously

                  pure $ GuessRequest { guessingLetter, previouslyIncorrectLetters, wordSoFar, nice }
validateGuess ( RawGiveUpRequest {
                previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
              , wordSoFar: rawWordSoFar} ) = 
                  validateIncorrectLettersAndWordSoFar { rawPreviouslyIncorrectLetters, rawWordSoFar } >>= pure <<< GiveUpRequest

getEligibleWords :: Word -> Array Char -> Array String -> Array String
getEligibleWords wordSoFar previouslyIncorrectLetters = let 
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
      goodUnfairWords <- (if guess.nice then last else head) fewestRevealedLettersGrouped
      let
        wordToPlay = indexWrapping goodUnfairWords toPick

        revealedLetterPositions = occurrenceIndices guess.guessingLetter (toCharArray wordToPlay)

        isCorrect = not $ null revealedLetterPositions
        newWordSoFar = revealLetterInWord guess.guessingLetter wordToPlay guess.wordSoFar

        message =
          if isCorrect then
            if isWon newWordSoFar
            then Win $ show newWordSoFar
            else Correct guess.guessingLetter $ toNonEmpty $ unsafePartial $ fromJust $ fromArray revealedLetterPositions
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
evaluateGuess (GiveUpRequest giveUp) wordlist _ = GiveUpResponse { eligibleWords: getEligibleWords giveUp.wordSoFar giveUp.previouslyIncorrectLetters wordlist }

