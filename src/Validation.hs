-- | Module for the book "Finding Success (and Failure) in Haskell"
module Validation where

import Prelude ()

import Relude

import Data.Char qualified as Char

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

-- chapter 3

-- we use a newtype in order to keep the proof that we had a valid password
  
checkPasswordLength :: String -> Maybe String
checkPasswordLength s = if length s > 20 || length s < 10
                        then Nothing
                        else Just s

requireAlphaNum :: String -> Maybe String
requireAlphaNum str = if all Char.isAlphaNum str
                      then Just str
                      else Nothing  

cleanWhiteSpace :: String -> Maybe String
cleanWhiteSpace str =
  case dropWhile Char.isSpace str of
    "" -> Nothing
    str' -> Just str'

-- chapter 4 - using the Maybe monad

newtype Password = Password String
  deriving (Show, Eq)

validatePassword :: String -> Maybe Password
validatePassword p =
  cleanWhiteSpace p
        >>= requireAlphaNum
        >>= checkPasswordLength
        <&> Password

-- exercise 11 - bindMaybe

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x