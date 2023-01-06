{-# LANGUAGE OverloadedStrings #-}

module BookSpec (spec) where

import Book
import Test.Hspec

import Data.ByteString.Builder qualified as BSB

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start = RequestLine (Method "GET") (RequestTarget "/hello.txt") http_1_1
    host = HeaderField (FieldName "Host") (FieldValue "www.example.com")
    lang = HeaderField (FieldName "Accept-Language") (FieldValue "en, mi")

spec :: Spec
spec = do
    describe "Request Encoding" $
        it "encodes helloRequest correctly" $ do
            BSB.toLazyByteString (encodeRequest helloRequest) `shouldBe` "GET /hello.txt HTTP/1.1\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"

    describe "Response Encoding" $
        it "encodes helloResponse correctly" $ do
            BSB.toLazyByteString (encodeResponse helloResponse) `shouldBe` "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=us-ascii\r\nContent-Length: 6\r\n\r\nHello!"
