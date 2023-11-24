module Guess
  ( GuessError(..)
  , validateIncorrectLettersAndWordSoFar
  , guessErrorResponse
  , getEligibleWords
  , routeGuess
  )
  where

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
import Data.List (List)
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
import Effect.Ref (Ref, modify)
import Game (Game(..), GameId)
import HTTPurple (Response, ResponseM, badRequest, fromValidatedE, jsonHeaders, notFound, ok', toJson, usingCont)
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Partial.Unsafe (unsafePartial)
import Utils (countOccurrences, indexWrapping, isAsciiLetter, jsonDecodeErrorResponse, jsonDecoder, jsonEncoder, leftIf, mapLeft, occurrenceIndices, pickRandom)
import Word (Word, isWon, mkWord, revealLetterInWord, wordMatches)

data Unfair = Nice | Mean
data GameMode = Unfair Unfair | Normal

fromString :: CaseInsensitiveString -> Either GuessError GameMode
fromString s
 | s == wrap "normal" = pure Normal
 | otherwise = Unfair <$> unfairFromString s

unfairFromString :: CaseInsensitiveString -> Either GuessError Unfair
unfairFromString s
 | s == wrap "nice" = pure Nice
 | s == wrap "mean" = pure Mean
 | otherwise = Left InvalidMode

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

data GuessError = GuessingLetterNotAlpha Char | GuessingLetterHasBeenGuessedPreviously | IncorrectLettersInWordSoFar (Array Char) | WordSoFarMalformed (Array String) | WordSoFarWrongLength Int Int | AlreadyWon | ImpossibleWordSoFar | StringsShouldBeChars (Array String) | InvalidMode | WordSoFarDoesNotMatchSecret

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
            WordSoFarDoesNotMatchSecret -> "The wordSoFar's revealed letters do not match the secret word or one of the blanks holds a letter that has already been revealed."

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

pickWordWithWordlist :: Wordlist -> Unfair -> GuessRequest -> Aff GuessResponse
pickWordWithWordlist wl unfairMode guess = do
  guessResponse <- pickRandomUnfairWord guess unfairMode wl
  pure $ guessResponse

unsetSecretWord :: Ref (List Game) -> GameId -> Aff Unit
unsetSecretWord gamesRef gameId = void $ liftEffect $ modify unset gamesRef
  where unset games = map (\game@(Game r@{ id }) -> if id == gameId then Game r { secretWord = Nothing } else game) games

simpleEvaluateGuess :: Char -> Word -> String -> Array Char -> GuessResponse
simpleEvaluateGuess guessingLetter wordSoFar secretWord previouslyIncorrectLetters = 
  let
    revealedLetterPositions = occurrenceIndices guessingLetter (toCharArray secretWord)

    isCorrect = not $ null revealedLetterPositions

    incorrectLetters = previouslyIncorrectLetters <> if isCorrect then [] else [guessingLetter] -- TODO dedup
    newWordSoFar = revealLetterInWord guessingLetter secretWord wordSoFar
    message = if isCorrect then -- TODO dedup
                if isWon newWordSoFar then
                  Win $ show newWordSoFar
                else
                  Correct guessingLetter $ toNonEmpty $ unsafePartial $ fromJust $ fromArray revealedLetterPositions
              else
                Incorrect guessingLetter
  in
  GuessResponse { incorrectLetters, wordSoFar: newWordSoFar, message }

routeGuess :: Ref (List Game) -> Wordlist -> Maybe Game -> RequestBody -> ResponseM
routeGuess games wl (Just game@(Game { secretWord, id })) body = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input@(GuessRequest { guessingLetter, wordSoFar, previouslyIncorrectLetters, mode }) <- fromValidatedE (validateGuess game) guessErrorResponse jsonRequest
    output :: GuessResponse <- lift case mode of
                                  Unfair unfairMode -> do 
                                                        unsetSecretWord games id
                                                        pickWordWithWordlist wl unfairMode input
                                  Normal -> case secretWord of 
                                              Just s -> pure $ simpleEvaluateGuess guessingLetter wordSoFar s previouslyIncorrectLetters
                                              Nothing -> do
                                                            newSecretWord <- (pickRandomEligibleWord input wl :: Aff (Maybe String))
                                                            pure case newSecretWord of
                                                              Just new -> simpleEvaluateGuess guessingLetter wordSoFar new previouslyIncorrectLetters
                                                              Nothing -> GuessResponse { message: InvalidGuess [ ImpossibleWordSoFar ]
                                                                                      , incorrectLetters: previouslyIncorrectLetters 
                                                                                      , wordSoFar: wordSoFar
                                                                                      }
    ok' jsonHeaders $ toJson jsonEncoder output
routeGuess _ _ Nothing _ = notFound

type Wordlist
  = Array String

type WordLength
  = Int

pickRandomUnfairWord :: GuessRequest -> Unfair -> Wordlist -> Aff GuessResponse
pickRandomUnfairWord g unfairMode wl = do
  randomPick <- liftEffect $ randomInt 0 10000  -- TODO get large random num func
  pure $ evaluateGuess g unfairMode wl randomPick

pickRandomEligibleWord :: GuessRequest -> Wordlist -> Aff (Maybe String)
pickRandomEligibleWord (GuessRequest { wordSoFar, previouslyIncorrectLetters }) wl = 
  liftEffect $ pickRandom (getEligibleWords wordSoFar previouslyIncorrectLetters wl)

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

evaluateGuess :: GuessRequest -> Unfair -> Wordlist -> Int -> GuessResponse
evaluateGuess (GuessRequest guess) unfairMode wordlist toPick =
  let
    eligibleWords = getEligibleWords guess.wordSoFar guess.previouslyIncorrectLetters wordlist

    guessingLetterOccurrences = (countOccurrences guess.guessingLetter) <<< toCharArray

    fewestRevealedLetters = sortWith guessingLetterOccurrences eligibleWords

    fewestRevealedLettersGrouped = groupBy (eq `on` guessingLetterOccurrences) fewestRevealedLetters

    response = do
      goodUnfairWords <- (case unfairMode of
                                Nice -> last 
                                Mean -> head) fewestRevealedLettersGrouped
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

        newIncorrectLetters = if isCorrect then
                                guess.previouslyIncorrectLetters
                              else 
                                guess.previouslyIncorrectLetters <> [guess.guessingLetter]
      pure
        $ GuessResponse
            { message
            , incorrectLetters: newIncorrectLetters
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
