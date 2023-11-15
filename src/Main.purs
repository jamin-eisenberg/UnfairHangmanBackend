module Main
  ( main
  ) where

import Prelude hiding ((/))
import Data.Either (Either(..))
import Data.List (List(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref, new)
import Game (Game)
import HTTPurple (serve)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Routes (route, router)

type Wordlist
  = Array String

-- TODO move give up to a separate endpoint
-- TODO int repr. word length to create game req
loadWordlist :: Aff Wordlist
loadWordlist = do
  text <- readTextFile UTF8 "res/english_wordlist.txt"
  pure $ lines text

serveWithWordlist :: Either Error Wordlist -> Ref (List Game) -> Effect Unit
serveWithWordlist r games = case r of
  Left e -> logShow e
  Right wl -> void $ serve { port: 8080 } { route, router: router wl games }

main :: Effect Unit
main = do
  games <- new Nil
  runAff_ (flip serveWithWordlist games) loadWordlist
