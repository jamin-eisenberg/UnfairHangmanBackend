module Game
  ( Game(..)
  , GameId(..)
  , mkGame
  )
  where

import Prelude

import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.UUID (UUID, genUUID, toString)
import Effect (Effect)

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
  , wordLength :: Int
  , secretWord :: Maybe String
  }
derive instance Newtype Game _

type Wordlist
  = Array String

mkGame :: Int -> Effect Game
mkGame wordLength = do
  id <- genUUID
  pure $ Game { id: wrap id, wordLength, secretWord: Nothing }
