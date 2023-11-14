module Data.Word where

import Prelude

import Data.Argonaut (class EncodeJson, fromString)
import Data.Array (all, any, catMaybes, elem, filter, fold, intersect, mapWithIndex, null, replicate, zip, zipWith)
import Data.Either (Either(..), blush, hush, isRight)
import Data.Generic.Rep (class Generic)
import Data.Letter (GuessChar, Letter, blank, letterMatches, mkLetter, revealLetter, toChar)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (fst, snd)

newtype Word = Word (Array Letter)

derive instance Newtype Word _
derive instance Generic Word _

instance Show Word where
  show (Word ls) = fold (show <$> ls)

instance EncodeJson Word where
  encodeJson = fromString <<< show

type SecretWord = String

mkWord :: Array Char -> Either (Array String) Word
mkWord cs = let letters :: Array (Either String Letter)
                letters = mkLetter <$> cs in
                if all isRight letters
                then (pure <<< wrap <<< catMaybes) $ hush <$> letters
                else let maybeMsgs = blush <$> letters
                         makeMsg i = map (\msg -> "Error occurred at position " <> show i <> ": " <> msg)
                         in
                        (Left <<< catMaybes <<< mapWithIndex makeMsg) maybeMsgs

mkBlankWord :: Int -> Word
mkBlankWord n = wrap $ replicate n blank

wordMatches :: String -> Word -> Boolean
wordMatches s (Word w) = allLettersFit && noBlanksAreAlreadyUsedLetters
  where allLettersFit = all identity $ zipWith letterMatches strArr w

        strArr = toCharArray s
        existingLetters = toChar <$> filter (_ /= blank) w
        lettersInBlanks = map fst $ filter (\e -> snd e == blank) $ zip strArr w
        noBlanksAreAlreadyUsedLetters = null $ intersect existingLetters lettersInBlanks


wordContains :: Word -> Char -> Boolean
wordContains (Word ls) c = elem c $ toChar <$> filter (_ /= blank) ls

revealLetterInWord :: GuessChar -> SecretWord -> Word -> Word
revealLetterInWord c ans word = let ansChars = toCharArray ans
                                    wordLetters = unwrap word
                    in
                      wrap $ zipWith (revealLetter c) ansChars wordLetters

isWon :: Word → Boolean
isWon = not <<< any (_ == blank) <<< unwrap