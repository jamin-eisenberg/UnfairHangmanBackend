module Main
  ( main
  ) where

import Prelude hiding ((/))
import Data.Either (Either(..))
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

games :: Effect (Ref (Array Game))
games = new []

type Wordlist
  = Array String

-- TODO move give up to a separate endpoint
-- TODO int repr. word length to create game req
loadWordlist :: Aff Wordlist
loadWordlist = do
  text <- readTextFile UTF8 "res/english_wordlist.txt"
  pure $ lines text

serveGuessWithWordlist :: Either Error Wordlist -> Effect Unit
serveGuessWithWordlist r = case r of
  Left e -> logShow e
  Right wl -> void $ serve { port: 8080 } { route, router: router wl }

main :: Effect Unit
main = runAff_ serveGuessWithWordlist loadWordlist
