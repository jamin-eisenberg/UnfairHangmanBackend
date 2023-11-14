module Game
  ( Game
  , mkGame
  ) where

import Prelude
import Data.Array (filter)
import Data.String as String
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Random (randomInt)
import Utils (indexWrapping)

newtype Game
  = Game
  { id :: UUID
  , secretWord :: String
  }

type WordLength
  = Int

type Wordlist
  = Array String

mkGame :: WordLength -> Wordlist -> Effect Game
mkGame n wl = do
  id <- genUUID
  randomIndex <- randomInt 0 1000000
  pure $ mkGamePure id n wl randomIndex

mkGamePure :: UUID -> WordLength -> Wordlist -> Int -> Game
mkGamePure id n wl randomIndex =
  let
    rightLengthWords = filter (\word -> n == String.length word) wl

    secretWord = indexWrapping rightLengthWords randomIndex
  in
    Game { id, secretWord }
