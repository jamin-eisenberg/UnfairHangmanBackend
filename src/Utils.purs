module Utils where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Array (filter, mapWithIndex)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (class Foldable, indexl, length)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import HTTPurple (JsonDecoder(..), JsonEncoder(..), Response, badRequest)
import Partial.Unsafe (unsafePartial)

pickRandom :: forall a f. Foldable f => f a -> Effect (Maybe a)
pickRandom xs =
  let
    len = length xs
  in
    if len == 0 then
      pure Nothing
    else do
      index <- randomInt 0 $ len - 1
      pure $ (indexl index xs)

indexWrapping :: forall a f. Foldable f => f a -> Int -> a
indexWrapping xs i = unsafePartial $ fromJust (indexl (i `mod` (length xs)) xs)

countOccurrences :: forall a. Eq a => a -> Array a -> Int
countOccurrences x = length <<< filter (_ == x)

occurrenceIndices :: forall a. Eq a => a -> Array a -> Array Int
occurrenceIndices x = map fst <<< filter (\iy -> x == snd iy) <<< mapWithIndex (Tuple)

mapLeft :: forall a b c. (a -> b) -> Either a c -> Either b c
mapLeft f e = case e of
  Left a -> Left $ f a
  Right c -> Right c

leftIf :: forall a. Boolean -> a -> Either a Unit
leftIf cond err = if cond then Left err else pure unit

toLower :: Char -> Maybe Char
toLower c
  | isAsciiLetter c && isLower c = pure c
  | isAsciiLetter c = (toEnum <<< (_ + 32) <<< fromEnum) c
  | otherwise = Nothing

isLower :: Char -> Boolean
isLower c = let charCode = fromEnum c in charCode >= 97 && charCode <= 122

isAsciiLetter :: Char -> Boolean
isAsciiLetter c =
  let
    charCode = fromEnum c
  in
    (charCode >= 65 && charCode <= 90)
      || isLower c

jsonDecoder :: forall a. DecodeJson a ⇒ JsonDecoder JsonDecodeError a
jsonDecoder = JsonDecoder $ parseJson >=> decodeJson

jsonEncoder :: forall a. EncodeJson a ⇒ JsonEncoder a
jsonEncoder = JsonEncoder $ encodeJson >>> stringify

jsonDecodeErrorResponse :: forall m. MonadAff m => JsonDecodeError -> m Response
jsonDecodeErrorResponse = badRequest <<< printJsonDecodeError
