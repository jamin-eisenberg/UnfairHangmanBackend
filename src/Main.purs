module Main
  ( main
  )
  where

import Prelude hiding ((/))

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Either (Either(..), note)
import Data.Game (Game)
import Data.Guess (GuessRequest, RawGuessRequest, Wordlist, pickRandomUnfairWord, validateGuess)
import Data.GuessResponse (GuessError, GuessResponse)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.String.Utils (lines)
import Data.UUID (UUID, parseUUID, toString)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref, new)
import HTTPurple (class GRouteDuplexCtr, class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, Response, ResponseM, RouteDuplex', as, badRequest, fromValidatedE, jsonHeaders, mkRoute, noArgs, notFound, ok', segment, serve, string, toJson, usingCont, (/))
import HTTPurple.Json (fromJsonE)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Routing.Duplex.Generic (gRouteDuplexCtr)

newtype GameId = GameId UUID
derive instance Generic GameId _
derive instance Newtype GameId _

data Route = CreateGame | Guess GameId
derive instance Generic Route _

gameIdFromString :: String -> Either String GameId
gameIdFromString = note "The UUID specfied in the URL is invalid." <<< map wrap <<< parseUUID

gameId :: RouteDuplex' String -> RouteDuplex' GameId
gameId = as ((toString <<< unwrap) :: GameId -> String) gameIdFromString

route :: RouteDuplex' Route
route = mkRoute
  { "CreateGame": noArgs
  , "Guess": gameId / "guess"
  } 

jsonDecoder ∷ ∀ (a ∷ Type). DecodeJson a ⇒ JsonDecoder JsonDecodeError a
jsonDecoder = JsonDecoder $ parseJson >=> decodeJson
jsonEncoder ∷ ∀ (a ∷ Type). EncodeJson a ⇒ JsonEncoder a
jsonEncoder = JsonEncoder $ encodeJson >>> stringify

games :: Effect (Ref (Array Game))
games = new []

-- TODO separate give up to a separate endpoint

loadWordlist :: Aff Wordlist
loadWordlist = do 
  text <- readTextFile UTF8 "res/english_wordlist.txt"
  pure $ lines text

pickWordWithWordlist :: Wordlist -> GuessRequest -> Aff GuessResponse
pickWordWithWordlist wl guess = do
  guessResponse <- pickRandomUnfairWord guess wl
  pure $ guessResponse

guessErrorResponse :: forall m. MonadAff m => GuessError -> m Response
guessErrorResponse = badRequest <<< show

jsonDecodeErrorResponse :: forall m. MonadAff m => JsonDecodeError -> m Response
jsonDecodeErrorResponse = badRequest <<< printJsonDecodeError

router :: Wordlist -> Request Route -> ResponseM
router wl { route: Guess gameId, method: Get, body } = usingCont do
    jsonRequest :: RawGuessRequest <- fromJsonE jsonDecoder jsonDecodeErrorResponse body
    input :: GuessRequest <- fromValidatedE validateGuess guessErrorResponse jsonRequest
    output :: GuessResponse <- lift $ pickWordWithWordlist wl input
    ok' jsonHeaders $ toJson jsonEncoder output
router _ _ = notFound

serveGuessWithWordlist :: Either Error Wordlist -> Effect Unit
serveGuessWithWordlist r = case r of
                             Left e -> logShow e
                             Right wl -> void $ serve { port: 8080 } { route, router: router wl }

main :: Effect Unit
main = runAff_ serveGuessWithWordlist loadWordlist