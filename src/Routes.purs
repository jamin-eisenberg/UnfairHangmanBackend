module Routes
  ( GameId(..)
  , Route(..)
  , WordLength(..)
  , route
  , router
  )
  where

import Prelude hiding ((/))

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Either (Either, note)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, parseUUID, toString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import GuessRequest (GuessRequest, RawGuessRequest)
import GuessResponse (GuessError, GuessResponse)
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, ResponseM, RouteDuplex', as, badRequest, fromValidatedE, int, jsonHeaders, mkRoute, notFound, ok, ok', optional, segment, toJson, usingCont, (/), (?))
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Validation (validateGuess)
import Wordlist (pickRandomUnfairWord)

newtype GameId = GameId UUID
derive instance Generic GameId _
derive instance Newtype GameId _

newtype WordLength = WordLength Int
derive instance Generic WordLength _
derive instance Newtype WordLength _
instance Show WordLength where
  show = genericShow

data Route = CreateGame { wordLength :: Maybe WordLength } | Guess GameId
derive instance Generic Route _

wordLengthCapture :: RouteDuplex' String -> RouteDuplex' (Maybe WordLength)
wordLengthCapture = optional <<< _Newtype <<< int

gameIdFromString :: String -> Either String GameId
gameIdFromString = note "The UUID specfied in the URL is invalid." <<< map wrap <<< parseUUID

gameIdCapture :: RouteDuplex' GameId
gameIdCapture = as (toString <<< unwrap) gameIdFromString $ segment

route :: RouteDuplex' Route
route = mkRoute
  { "CreateGame": "game" ? { wordLength: wordLengthCapture }
  , "Guess": gameIdCapture / "guess"
  } 

jsonDecoder ∷ ∀ (a ∷ Type). DecodeJson a ⇒ JsonDecoder JsonDecodeError a
jsonDecoder = JsonDecoder $ parseJson >=> decodeJson
jsonEncoder ∷ ∀ (a ∷ Type). EncodeJson a ⇒ JsonEncoder a
jsonEncoder = JsonEncoder $ encodeJson >>> stringify

type Wordlist = Array String

pickWordWithWordlist :: Wordlist -> GuessRequest -> Aff GuessResponse
pickWordWithWordlist wl guess = do
  guessResponse <- pickRandomUnfairWord guess wl
  pure $ guessResponse

guessErrorResponse :: forall m. MonadAff m => GuessError -> m Response
guessErrorResponse = badRequest <<< show

jsonDecodeErrorResponse :: forall m. MonadAff m => JsonDecodeError -> m Response
jsonDecodeErrorResponse = badRequest <<< printJsonDecodeError

routeGuess :: Wordlist -> GameId -> RequestBody -> ResponseM
routeGuess wl _ body = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GuessRequest <- fromValidatedE validateGuess guessErrorResponse jsonRequest
    output :: GuessResponse <- lift $ pickWordWithWordlist wl input
    ok' jsonHeaders $ toJson jsonEncoder output

routeCreateGame :: Wordlist -> Maybe WordLength -> ResponseM
routeCreateGame _ wordLength = ok $ show wordLength

router :: Wordlist -> Request Route -> ResponseM
router wl request = case request of 
                       { route: Guess gameId, method: Get, body } -> routeGuess wl gameId body
                       { route: CreateGame { wordLength }, method: Post } -> routeCreateGame wl wordLength
                       _ -> notFound
-- TODO validation and routes modules violate verticality?
