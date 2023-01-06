module Main (main) where

import Book qualified

main :: IO ()
main = Book.writeGreeting
