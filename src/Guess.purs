module Guess where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Array (any, catMaybes, elem, filter, groupBy, head, intersect, last, length, null, sortWith)
import Data.Array.NonEmpty (fromArray, fromNonEmpty, init, toNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.String.CodeUnits (singleton, toChar, toCharArray)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Game (Game(..))
import HTTPurple (Response, ResponseM, badRequest, fromValidatedE, jsonHeaders, notFound, ok', toJson, usingCont)
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Partial.Unsafe (unsafePartial)
import Utils (countOccurrences, indexWrapping, isAsciiLetter, jsonDecodeErrorResponse, jsonDecoder, jsonEncoder, leftIf, mapLeft, occurrenceIndices)
import Word (Word, isWon, mkWord, revealLetterInWord, wordMatches)

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

newtype RawGuessRequest = RawGuessRequest { mode :: String, guessingLetter :: String, previouslyIncorrectLetters :: Array String, wordSoFar :: String }

newtype GuessRequest = GuessRequest { mode :: GameMode, guessingLetter :: Char, previouslyIncorrectLetters :: Array Char, wordSoFar :: Word }

instance DecodeJson RawGuessRequest where
  decodeJson json = do
    decoded <- decodeJson json
    previouslyIncorrectLetters <- decoded .: "previouslyIncorrectLetters"
    wordSoFar <- decoded .: "wordSoFar"
    guessingLetter <- decoded .: "guessingLetter"
    mode <- decoded .: "mode"
    pure $ RawGuessRequest { mode, guessingLetter, previouslyIncorrectLetters, wordSoFar } 

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
  encodeJson msg = Argonaut.fromString $ show msg

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

positionsToMessage :: NonEmpty Array Int -> String
positionsToMessage ps = case ps of 
                          (p :| []) -> "position " <> show p
                          (p1 :| [p2]) -> "positions " <> show p1 <> " and " <> show p2
                          _ -> "positions " <> String.joinWith ", " (show <$> init nePs) <> ", and " <> show (NEA.last $ nePs)
                            where nePs = fromNonEmpty ps

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


data GuessResponse = GuessResponse { message :: GuessResponseMessage, incorrectLetters :: Array Char, wordSoFar :: Word }

instance EncodeJson GuessResponse where
  encodeJson (GuessResponse guess) = encodeJson guess

guessErrorResponse :: forall m. MonadAff m => GuessError -> m Response
guessErrorResponse = badRequest <<< show

pickWordWithWordlist :: Wordlist -> GuessRequest -> Aff GuessResponse
pickWordWithWordlist wl guess = do
  guessResponse <- pickRandomUnfairWord guess wl
  pure $ guessResponse

routeGuess :: Wordlist -> Maybe Game -> RequestBody -> ResponseM
routeGuess wl (Just game) body = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GuessRequest <- fromValidatedE (validateGuess game) guessErrorResponse jsonRequest
    output :: GuessResponse <- lift $ pickWordWithWordlist wl input
    ok' jsonHeaders $ toJson jsonEncoder output
routeGuess _ Nothing _ = notFound

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

    wordContainsAny cs str = any (\c -> String.contains (Pattern $ singleton c) str) cs

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
