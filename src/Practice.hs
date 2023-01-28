-- | Practice module for the book "Finding Success (and Failure) in Haskell"
module Practice where

import Prelude ()

import Relude

import Data.Char qualified as Char

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

toWord :: String -> Maybe String
toWord [] = Nothing
toWord xs = if all Char.IsAlpha xs then Just xs else Nothing
