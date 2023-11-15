module Game
  ( Game(..)
  , GameId(..)
  , mkGame
  )
  where

import Prelude

import Data.Array (filter)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.UUID (UUID, genUUID, toString)
import Effect (Effect)
import Effect.Random (randomInt)
import Utils (indexWrapping)

newtype GameId = GameId UUID
derive instance Generic GameId _
derive instance Newtype GameId _
instance Show GameId where
  show = toString <<< unwrap
instance Eq GameId where
  eq = eq `on` unwrap

newtype Game
  = Game
  { id :: GameId
  , secretWord :: String
  }
derive instance Newtype Game _

type WordLength
  = Int

type Wordlist
  = Array String

mkGame :: WordLength -> Wordlist -> Effect Game
mkGame n wl = do
  id <- genUUID
  randomIndex <- randomInt 0 1000000
  pure $ mkGamePure (wrap id) n wl randomIndex

mkGamePure :: GameId -> WordLength -> Wordlist -> Int -> Game
mkGamePure id n wl randomIndex =
  let
    rightLengthWords = filter (\word -> n == String.length word) wl

    secretWord = indexWrapping rightLengthWords randomIndex
  in
    Game { id, secretWord }
