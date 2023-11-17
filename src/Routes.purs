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
import Data.List (List, find, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UUID (parseUUID, toString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Ref (Ref, modify_, read)
import Game (Game(..), GameId, mkGame)
import GiveUp (GiveUpRequest(..), GiveUpResponse(..), RawGiveUpRequest, routeGiveUp, validateGiveUp)
import Guess (GuessError, GuessRequest, GuessResponse, RawGuessRequest, routeGuess)
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, ResponseM, RouteDuplex', as, badRequest, created', fromValidatedE, fullPath, header, int, jsonHeaders, mkRoute, notFound, ok', optional, segment, toJson, usingCont, (/), (?))
import HTTPurple.Body (RequestBody)
import HTTPurple.Json (fromJsonE)

newtype WordLength = WordLength Int
derive instance Generic WordLength _
derive instance Newtype WordLength _
instance Show WordLength where
  show = genericShow

data Route = CreateGame { wordLength :: Maybe WordLength } | Guess GameId | GiveUp GameId
derive instance Generic Route _

loggingMiddleware ::
  forall route.
  (Request route -> ResponseM) ->
  Request route ->
  ResponseM
loggingMiddleware outerRouter request = do
  liftEffect $ log $ "Request starting for " <> show request.method <> "\t" <> path
  response <- outerRouter request
  liftEffect $ log $ "Request ending for " <> path
  pure response
  where
  path = fullPath request

wordLengthCapture :: RouteDuplex' String -> RouteDuplex' (Maybe WordLength)
wordLengthCapture = optional <<< _Newtype <<< int

gameIdFromString :: String -> Either String GameId
gameIdFromString = note "The UUID specfied in the URL is invalid." <<< map wrap <<< parseUUID

gameIdCapture :: RouteDuplex' GameId
gameIdCapture = as (toString <<< unwrap) gameIdFromString $ segment

route :: RouteDuplex' Route
route = mkRoute
  { "CreateGame": "game" ? { wordLength: wordLengthCapture }
  , "GiveUp": gameIdCapture / "giveUp"
  , "Guess": gameIdCapture / "guess"
  } 

type Wordlist = Array String

routeCreateGame :: Ref (List Game) -> Maybe WordLength -> ResponseM
routeCreateGame games givenWordLength = do
    randomWordLength <- liftEffect $ wrap <$> randomInt 3 8
    let wordLength = fromMaybe randomWordLength givenWordLength
    let wordLengthString = show $ unwrap wordLength

    if unwrap wordLength < 1
    then badRequest $ "wordLength should be >= 1. Got " <> wordLengthString
    else do
      g@(Game { id }) <- liftEffect $ mkGame (unwrap wordLength)
      let gameLocation = "/" <> show id
      liftEffect $ modify_ (g:_) games
      created' (header "Location" gameLocation <> header "Word-Length" wordLengthString)

router :: Wordlist -> Ref (List Game) -> Request Route -> ResponseM
router wl games req = loggingMiddleware result req
  where result request = case request of 
                       { route: Guess gameId, method: Patch, body } -> do
                                                                        game <- liftEffect $ gameWithId gameId
                                                                        routeGuess wl game body
                       { route: GiveUp gameId, method: Delete, body } -> do
                                                                          game <- liftEffect $ gameWithId gameId
                                                                          routeGiveUp wl game body
                       { route: CreateGame { wordLength }, method: Post } -> routeCreateGame games wordLength
                       _ -> notFound
          where gameWithId id = do
                  gs <- read games
                  pure $ find (\g -> (unwrap g).id == id) gs
-- TODO validation and routes modules violate verticality?
-- TODO add DB for games
-- TODO get game state
-- TODO get help
-- separate dict filtration into lazy phases