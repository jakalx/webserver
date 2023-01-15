{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}

module BookSpec (spec) where

import Prelude ()

import Book
import Data.Aeson ((.:))
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.List qualified as L
import Relude
import Test.Hspec

import Test.Hspec.QuickCheck

import Test.QuickCheck

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder qualified as BSB

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start = RequestLine (Method "GET") (RequestTarget "/hello.txt") http_1_1
    host = HeaderField (FieldName "Host") (FieldValue "www.example.com")
    lang = HeaderField (FieldName "Accept-Language") (FieldValue "en, mi")

balanced :: Eq a => [(a, a)] -> [a] -> Bool
balanced pairs list = go [] (filter isRelevant list)
  where
    isRelevant c = isOpening c || isClosing c
    isOpening c = c `elem` (fst <$> pairs)
    isClosing c = c `elem` (snd <$> pairs)

    go [] [] = True
    go _  [] = False
    go stack (x:xs) = case (stack, L.lookup x pairs) of
                        (_, Just c) -> go (c:stack) xs
                        (c:cs, _) -> x == c && go cs xs
                        _ -> False

parseCount :: J.Value -> J.Parser Integer
parseCount = J.withObject "hitCount" $ \o -> o .: "hits" >>= (.: "count")

-- excercise 22 Overflow
mid :: Word8 -> Word8 -> Word8
mid a b = fromIntegral $ (a' + b') `div` 2
  where
    a' = toInteger a
    b' = toInteger b

parens :: [(Char, Char)]
parens = [('(', ')'), ('{','}'), ('[',']')]

spec :: Spec
spec = do
    describe "balanced checks for balanced pairs" do
      it "balanced parens are balanced" do
        balanced parens "{}()[]({[]})" `shouldBe` True
      it "unbalanced parens are unbalanced" do
        balanced parens "(}" `shouldBe` False
      it "intermixed elements are ignoredb" do
        balanced parens "(abced efghi)" `shouldBe` True

    describe "Excercise 22 - Overflow" do
        it "mid 10 30 is 20" do
            mid 10 30 `shouldBe` 20

        it "mid 210 230 is 220" do
            mid 210 230 `shouldBe` 220

        prop "mid x y results in a value inbetween" $
            \x y -> mid x y `shouldSatisfy` (\m -> m >= min x y && m <= max x y)

    describe "Encoding" do
        it "encodes statusLine correctly" do
            BSB.toLazyByteString (encodeStatusLine $ status ok) `shouldBe` "HTTP/1.1 200 OK\r\n"

        it "encodes Content-Type HeaderField correctly" do
            BSB.toLazyByteString (encodeHeaderField $ contentType plainAscii) `shouldBe` "Content-Type: text/plain; charset=us-ascii\r\n"

        it "encodes Content-Length HeaderField correctly" do
            BSB.toLazyByteString (encodeHeaderField $ contentLength (10 :: Word64)) `shouldBe` "Content-Length: 10\r\n"

        prop "contentLength encodes parameter correctly" $
            \len -> contentLength len `shouldBe` HeaderField (FieldName "Content-Length") (FieldValue $ show len)

        it "encodes helloRequest correctly" do
            BSB.toLazyByteString (encodeRequest helloRequest) `shouldBe` "GET /hello.txt HTTP/1.1\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"

        it "encodes helloResponse correctly" do
            BSB.toLazyByteString (encodeResponse helloResponse) `shouldBe` "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=us-ascii\r\nContent-Length: 6\r\n\r\nHello!"

        it "asciiOk 'Hello!' returns helloResponse" do
            asciiOk "Hello!" `shouldBe` helloResponse

        it "countHelloAscii describes page visits correctly" do
            countHelloAscii 0 `shouldBe` "Hello!\r\nThis page has never been viewed."
            countHelloAscii 1 `shouldBe` "Hello!\r\nThis page has been viewed once."
            countHelloAscii 10 `shouldBe` "Hello!\r\nThis page has been viewed 10 times."

        it "countHelloText describes page visits correctly" do
            countHelloText 0 `shouldBe` "Hello! \9835\r\nThis page has never been viewed."
            countHelloText 1 `shouldBe` "Hello! \9835\r\nThis page has been viewed once."
            countHelloText 10 `shouldBe` "Hello! \9835\r\nThis page has been viewed 10 times."

        prop "countHelloJson describes page visits correctly" $
            \(NonNegative @Integer count) ->
              case J.parse parseCount (countHelloJson $ fromIntegral count) of
                J.Success count' -> count == count'
                _ -> False

    describe "Chapter 11 - Chunk" do
      describe "dataChunk" do
        prop "creates Chunk out of a DataChunk" $ \str ->
          let
            bs = LBS.toStrict . BSB.toLazyByteString . BSB.stringUtf8 $ str
            chunkData = ChunkData bs
            chunk = dataChunk chunkData
            size = case chunk of
              Chunk (ChunkSize sz) _ -> sz
          in
            size == fromIntegral (BS.length bs)
