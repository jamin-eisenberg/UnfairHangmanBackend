module Data.Game
  ( Game
  , mkGame
  ) where

import Prelude
import Data.UUID (UUID, genUUID)
import Data.Word (Word, mkBlankWord)
import Effect (Effect)

newtype Game
  = Game
  { id :: UUID
  , incorrectlyGuessedLetters :: Array Char
  , wordSoFar :: Word
  }

type WordLength
  = Int

mkGame :: WordLength -> Effect Game
mkGame n = do
  id <- genUUID
  pure $ mkGameWithUUID id n

mkGameWithUUID :: UUID -> WordLength -> Game
mkGameWithUUID id n =
  let
    incorrectlyGuessedLetters = []

    wordSoFar = mkBlankWord n
  in
    Game { id, incorrectlyGuessedLetters, wordSoFar }

-- TODO this is only for a fixed word
