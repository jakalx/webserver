-- | Module for the book "Finding Success (and Failure) in Haskell"
module Validation where

import Prelude ()

import Relude

import Data.Char qualified as Char
import Data.Ix (inRange)

import Data.Validation
-- import Data.List.NonEmpty qualified as NE

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

newtype Error = Error [String]
  deriving (Show, Eq, Semigroup)

newtype Username = Username String
  deriving (Show, Eq)

failWith :: String -> Validation Error a
failWith = Failure . Error . pure

checkLength :: (Word32, Word32) -> String -> Validation Error String
checkLength range str = if inRange range (fromIntegral $ length str)
                        then Success str
                        else failWith ("must be between " <> show range <> " characters long")

checkPasswordLength :: String -> Validation Error String
checkPasswordLength = checkLength (10, 20)

checkUsernameLength :: String -> Validation Error String
checkUsernameLength = checkLength (3, 12)

requireAlphaNum :: String -> Validation Error String
requireAlphaNum str = if all Char.isAlphaNum str
                      then Success str
                      else failWith "must only contain letters and numbers"

cleanWhiteSpace :: String -> Validation Error String
cleanWhiteSpace str =
  case dropWhile Char.isSpace str of
    "" -> failWith "empty or only whitespace"
    str' -> Success str'

validatePassword :: String -> Validation Error Password
validatePassword p = case cleanWhiteSpace p of
  Failure err -> Failure err
  Success p' -> requireAlphaNum p' *> checkPasswordLength p'
        <&> Password

validateUsername :: String -> Validation Error Username
validateUsername u = case cleanWhiteSpace u of
  Failure err -> Failure err
  Success u' -> requireAlphaNum u' *> checkUsernameLength u'
        <&> Username

-- exercise 11 - bindMaybe

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

-- chapter 7 - constructing a User

data User = User Username Password
  deriving (Show, Eq)

makeUser :: String -> String -> Validation Error User
makeUser username password =
  User <$> validateUsername username <*> validatePassword password
