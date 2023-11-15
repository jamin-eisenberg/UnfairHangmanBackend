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

routeGuess :: Wordlist -> Maybe Game -> RequestBody -> ResponseM
routeGuess wl (Just game) body = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GuessRequest <- fromValidatedE (validateGuess game) guessErrorResponse jsonRequest
    output :: GuessResponse <- lift $ pickWordWithWordlist wl input
    ok' jsonHeaders $ toJson jsonEncoder output
routeGuess _ Nothing _ = notFound

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
router wl games request = case request of 
                       { route: Guess gameId, method: Get, body } -> do
                                                                        game <- liftEffect $ gameWithId gameId
                                                                        routeGuess wl game body
                       { route: CreateGame { wordLength }, method: Post } -> routeCreateGame games wordLength
                       _ -> notFound
          where gameWithId id = do
                  gs <- read games
                  pure $ find (\g -> (unwrap g).id == id) gs
-- TODO validation and routes modules violate verticality?
-- TODO add DB for games
