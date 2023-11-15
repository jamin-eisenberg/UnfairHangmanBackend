module Routes
  ( Route(..)
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
import Data.List (List, elem, (:))
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UUID (parseUUID, toString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, modify_, read)
import Game (Game(..), GameId, mkGame)
import GuessRequest (GuessRequest, RawGuessRequest)
import GuessResponse (GuessError, GuessResponse)
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, ResponseM, RouteDuplex', as, badRequest, created', fromValidatedE, header, int, jsonHeaders, mkRoute, notFound, ok', optional, segment, toJson, usingCont, (/), (?))
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)
import Validation (validateGuess)
import Wordlist (pickRandomUnfairWord)

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

jsonDecoder :: forall a. DecodeJson a ⇒ JsonDecoder JsonDecodeError a
jsonDecoder = JsonDecoder $ parseJson >=> decodeJson
jsonEncoder :: forall a. EncodeJson a ⇒ JsonEncoder a
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

routeGuess :: Wordlist -> GameId -> Ref (List Game) -> RequestBody -> ResponseM
routeGuess wl _ games body = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GuessRequest <- fromValidatedE validateGuess guessErrorResponse jsonRequest
    output :: GuessResponse <- lift $ pickWordWithWordlist wl input
    ok' jsonHeaders $ toJson jsonEncoder output

routeCreateGame :: Wordlist -> Ref (List Game) -> Maybe WordLength -> ResponseM
routeCreateGame wl games givenWordLength = do
    randomWordLength <- liftEffect $ wrap <$> randomInt 3 8
    let wordLength = fromMaybe randomWordLength givenWordLength
    g@(Game { id }) <- liftEffect $ mkGame (unwrap wordLength) wl
    let gameLocation = "/" <> show id
    liftEffect $ modify_ (g:_) games
    created' (header "Location" gameLocation)

router :: Wordlist -> Ref (List Game) -> Request Route -> ResponseM
router wl games request = case request of 
                       { route: Guess gameId, method: Get, body } -> do
                                                                        isCreatedGame <- liftEffect $ gamesContainsId gameId
                                                                        if isCreatedGame
                                                                        then routeGuess wl gameId games body
                                                                        else notFound
                       { route: CreateGame { wordLength }, method: Post } -> routeCreateGame wl games wordLength
                       _ -> notFound
          where gamesContainsId id = (\gs -> id `elem` ((_.id <<< unwrap) <$> gs)) <$> read games
-- TODO validation and routes modules violate verticality?
-- TODO add DB for games
