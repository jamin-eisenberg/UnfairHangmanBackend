module Routes
  ( GameId(..)
  , Route(..)
  , route
  , router
  )
  where

import Prelude hiding ((/))

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Either (Either, note)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.UUID (UUID, parseUUID, toString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import GuessRequest (GuessRequest, RawGuessRequest)
import GuessResponse (GuessError, GuessResponse)
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, RouteDuplex', ResponseM, as, badRequest, fromValidatedE, jsonHeaders, mkRoute, noArgs, notFound, ok, ok', segment, toJson, usingCont, (/))
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Validation (validateGuess)
import Wordlist (pickRandomUnfairWord)

newtype GameId = GameId UUID
derive instance Generic GameId _
derive instance Newtype GameId _

data Route = CreateGame | Guess GameId
derive instance Generic Route _

gameIdFromString :: String -> Either String GameId
gameIdFromString = note "The UUID specfied in the URL is invalid." <<< map wrap <<< parseUUID

gameIdCapture :: RouteDuplex' String -> RouteDuplex' GameId
gameIdCapture = as ((toString <<< unwrap) :: GameId -> String) gameIdFromString

route :: RouteDuplex' Route
route = mkRoute
  { "CreateGame": noArgs
  , "Guess": gameIdCapture segment / "guess"
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

routeCreateGame :: Wordlist -> ResponseM
routeCreateGame _ = ok "Hi"

router :: Wordlist -> Request Route -> ResponseM
router wl request = case request of 
                       { route: Guess gameId, method: Get, body } -> routeGuess wl gameId body
                       { route: CreateGame, method: Post } -> routeCreateGame wl
                       _ -> notFound