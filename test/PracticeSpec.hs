{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

module PracticeSpec (spec) where

import Relude
import Prelude ()

import Practice

import Test.Hspec
import Test.Hspec.QuickCheck

-- import Test.Hspec.QuickCheck
-- import Test.QuickCheck

spec :: Spec
spec = do
    describe "isAnagram" do
        prop "the reverse is always an angram" $
            \xs -> isAnagram xs (reverse xs)
