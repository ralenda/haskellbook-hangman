module Main where

import Hangman
import HangmanIO
import Control.Monad

main :: IO ()
main = forever newGame
