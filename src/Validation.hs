-- | Module for the book "Finding Success (and Failure) in Haskell"

{-# LANGUAGE OverloadedStrings #-}

module Validation where

import Prelude ()

import Relude

import Data.Char qualified as Char
import Data.Ix (inRange)

import Data.Validation
import Data.Text qualified as T

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

newtype Password = Password Text
  deriving (Show, Eq)

newtype Error = Error [Text]
  deriving (Show, Eq, Semigroup)

newtype Username = Username Text
  deriving (Show, Eq)

failWith :: Text -> Validation Error a
failWith = Failure . Error . pure

checkLength :: (Word32, Word32) -> Text -> Validation Error Text
checkLength range str = if inRange range (fromIntegral $ T.length str)
                        then Success str
                        else failWith ("must be between " <> T.pack (show range) <> " characters long")

checkPasswordLength :: Text -> Validation Error Text
checkPasswordLength = checkLength (10, 20)

checkUsernameLength :: Text -> Validation Error Text
checkUsernameLength = checkLength (3, 12)

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum str = if T.all Char.isAlphaNum str
                      then Success str
                      else failWith "must only contain letters and numbers"

cleanWhiteSpace :: Text -> Validation Error Text
cleanWhiteSpace str =
  case T.dropWhile Char.isSpace str of
    "" -> failWith "empty or only whitespace"
    str' -> Success str'

validatePassword :: Text -> Validation Error Password
validatePassword p = case cleanWhiteSpace p of
  Failure err -> Failure err
  Success p' -> requireAlphaNum p' *> checkPasswordLength p'
        <&> Password

validateUsername :: Text -> Validation Error Username
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

makeUser :: Text -> Text -> Validation Error User
makeUser username password =
  User <$> validateUsername username <*> validatePassword password
