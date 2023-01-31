-- | Module for the book "Finding Success (and Failure) in Haskell"
module Validation where

import Prelude ()

import Relude

import Data.Char qualified as Char
import Data.Ix (inRange)

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

toWord :: String -> Maybe String
toWord [] = Nothing
toWord xs = if all Char.isAlpha xs then Just xs else Nothing

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
  case toWord word1 of
    Nothing -> "The first word is invalid"
    Just _ -> case toWord word2 of
      Nothing -> "The second word is invalid"
      _ ->
        if isAnagram word1 word2
        then "The words are anagrams"
        else "The words are not anagrams"

newtype Password = Password String
  deriving (Show, Eq)

newtype Error = Error String
  deriving (Show, Eq)

newtype Username = Username String
  deriving (Show, Eq)

checkLength :: (Word32, Word32) -> String -> Either Error String
checkLength range str = if inRange range (fromIntegral $ length str)
                        then Right str
                        else Left $ Error ("must be between " <> show range <> " characters long")

checkPasswordLength :: String -> Either Error String
checkPasswordLength = checkLength (10, 20)

checkUsernameLength :: String -> Either Error String
checkUsernameLength = checkLength (3, 12)

requireAlphaNum :: String -> Either Error String
requireAlphaNum str = if all Char.isAlphaNum str
                      then Right str
                      else Left $ Error "must only contain letters and numbers"

cleanWhiteSpace :: String -> Either Error String
cleanWhiteSpace str =
  case dropWhile Char.isSpace str of
    "" -> Left $ Error "empty or only whitespace"
    str' -> Right str'

validatePassword :: String -> Either Error Password
validatePassword p =
  cleanWhiteSpace p
        >>= requireAlphaNum
        >>= checkPasswordLength
        <&> Password

validateUsername :: String -> Either Error Username
validateUsername u =
  cleanWhiteSpace u
        >>= requireAlphaNum
        >>= checkUsernameLength
        <&> Username

-- exercise 11 - bindMaybe

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

-- chapter 7 - constructing a User

data User = User Username Password
  deriving (Show, Eq)

makeUser :: String -> String -> Either Error User
makeUser username password =
  User <$> validateUsername username <*> validatePassword password
