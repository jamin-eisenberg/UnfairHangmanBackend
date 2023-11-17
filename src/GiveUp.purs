module GiveUp
  ( routeGiveUp
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Game (Game(..))
import Guess (GuessError, getEligibleWords, guessErrorResponse, validateIncorrectLettersAndWordSoFar)
import HTTPurple (ResponseM, fromValidatedE, jsonHeaders, notFound, ok', toJson, usingCont)
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Utils (jsonDecodeErrorResponse, jsonDecoder, jsonEncoder)
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

newtype GiveUpResponse
  = GiveUpResponse { secretWord :: Maybe String, eligibleWords :: Array String }

instance EncodeJson GiveUpResponse where
  encodeJson (GiveUpResponse giveUp) = encodeJson giveUp

validateGiveUp :: Game -> RawGiveUpRequest -> Either GuessError GiveUpRequest
validateGiveUp (Game { wordLength }) ( RawGiveUpRequest
    { previouslyIncorrectLetters: rawPreviouslyIncorrectLetters
  , wordSoFar: rawWordSoFar
  }
) = do
  { wordSoFar, previouslyIncorrectLetters } <- validateIncorrectLettersAndWordSoFar wordLength { rawPreviouslyIncorrectLetters, rawWordSoFar }
  pure $ GiveUpRequest { previouslyIncorrectLetters, wordSoFar }

type Wordlist = Array String

revealWords :: Wordlist -> Maybe String -> GiveUpRequest -> GiveUpResponse
revealWords wl secretWord (GiveUpRequest { previouslyIncorrectLetters, wordSoFar }) =
  let eligibleWords = getEligibleWords wordSoFar previouslyIncorrectLetters wl
    in
      GiveUpResponse { eligibleWords, secretWord }

routeGiveUp :: Wordlist -> Maybe Game -> RequestBody -> ResponseM
routeGiveUp wl (Just game) body = usingCont do
    jsonRequest :: RawGiveUpRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GiveUpRequest <- fromValidatedE (validateGiveUp game) guessErrorResponse jsonRequest
    let output = revealWords wl (unwrap game).secretWord input
    ok' jsonHeaders $ toJson jsonEncoder output
routeGiveUp _ Nothing _ = notFound