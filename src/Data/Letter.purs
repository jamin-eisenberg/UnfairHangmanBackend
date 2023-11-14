module Data.Letter
  ( AnswerChar
  , GuessChar
  , Letter
  , blank
  , isAsciiLetter
  , letterMatches
  , mkLetter
  , revealLetter
  , toChar
  )
  where

import Prelude

import Data.Either (Either, note)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)

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

type GuessChar = Char
type AnswerChar = Char

toLower :: Char -> Maybe Char
toLower c
 | isAsciiLetter c && isLower c = pure c
 | isAsciiLetter c = (toEnum <<< (_ + 32) <<< fromEnum) c
 | otherwise = Nothing

isLower :: Char -> Boolean
isLower c = let charCode = fromEnum c in charCode >= 97 && charCode <= 122

isAsciiLetter :: Char -> Boolean
isAsciiLetter c = let charCode = fromEnum c in
                    (charCode >= 65 && charCode <= 90) || 
                    isLower c

letterMatches :: Char -> Letter -> Boolean
letterMatches c letter = case letter of
                    Letter lc -> lc == c
                    Blank -> true

toChar :: Letter -> Char
toChar l = case l of
            Letter c -> c
            Blank -> '_'

revealLetter :: GuessChar -> AnswerChar -> Letter -> Letter
revealLetter g a known = case known of
                            Letter _ -> known
                            Blank -> if g == a then Letter g else Blank 