{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec (spec) where

import Relude
import Prelude ()

import Data.Validation
import Validation

import Data.Char qualified as Char

import Test.Hspec
import Test.Hspec.QuickCheck

-- import Test.QuickCheck

spec :: Spec
spec = do
    describe "isAnagram" do
        prop "the reverse is always an angram" $
            \xs -> isAnagram xs (reverse xs)

    describe "checkAnagram" do
        it "checks first and second word" do
            checkAnagram "hello world" "olleh" `shouldBe` "The first word is invalid"
            checkAnagram "olleh" "hello world" `shouldBe` "The second word is invalid"

        it "checks for anagram" do
            checkAnagram "hello" "world" `shouldBe` "The words are not anagrams"
            checkAnagram "olleh" "hello" `shouldBe` "The words are anagrams"

    describe "checkPasswordLength" do
        prop "accepts passwords between 10 and 20 chars" \(n, c) ->
            let
                len = 10 + (abs n `rem` (21 - 10))
                p = replicate len c
             in
                checkPasswordLength p `shouldBe` Success p

        prop "rejects passwords longer than 20 characters" \(str, c) ->
            let
                suffix = replicate 21 c
                p = str <> suffix
             in
                checkPasswordLength p `shouldBe` failWith "must be between (10,20) characters long"

    describe "requireAlphaNum" do
        prop "considers alpha-numerical characters as valid" \str ->
            let p = filter Char.isAlphaNum str
             in requireAlphaNum p `shouldBe` Success p

    describe "cleanWhiteSpace" do
        prop "removes leading whitespace" \(n, str) ->
            let
                str' = 'a' : str
                ws = replicate n ' '
                p = ws <> str'
             in
                cleanWhiteSpace p `shouldBe` Success str'

    describe "validatePassword" do
        it "rejects empty or whitespace only passwords" do
            validatePassword "" `shouldBe` failWith "empty or only whitespace"
            validatePassword (replicate 10 ' ') `shouldBe` failWith "empty or only whitespace"

        it "accepts valid password" do
            validatePassword " abcdefghijkl1234" `shouldBe` Success (Password "abcdefghijkl1234")

    describe "validateUsername" do
        it "rejects empty or whitespace only usernames" do
            validateUsername "" `shouldBe` failWith "empty or only whitespace"
            validateUsername (replicate 10 ' ') `shouldBe` failWith "empty or only whitespace"

        it "accepts valid username" do
            validateUsername "johndoe" `shouldBe` Success (Username "johndoe")
