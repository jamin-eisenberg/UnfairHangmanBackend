module Letter
  ( Letter
  , blank
  , letterMatches
  , mkLetter
  , revealLetter
  , toChar
  )
  where

import Prelude

import Data.Either (Either, note)
import Data.String.CodeUnits (singleton)
import Utils (toLower)

data Letter = Letter Char | Blank

derive instance Eq Letter

instance Show Letter where
  show l = case l of
            Letter c -> singleton c
            Blank -> "_"

mkLetter :: Char -> Either String Letter
mkLetter c = if c == '_' 
             then pure Blank
             else note ("Expected to find a letter or an underscore ('_'), but found '" <> singleton c <> "'.") (Letter <$> toLower c)

blank :: Letter
blank = Blank

letterMatches :: Char -> Letter -> Boolean
letterMatches c letter = case letter of
                    Letter lc -> lc == c
                    Blank -> true

toChar :: Letter -> Char
toChar l = case l of
            Letter c -> c
            Blank -> '_'

type GuessChar = Char
type AnswerChar = Char

revealLetter :: GuessChar -> AnswerChar -> Letter -> Letter
revealLetter g a known = case known of
                            Letter _ -> known
                            Blank -> if g == a then Letter g else Blank 