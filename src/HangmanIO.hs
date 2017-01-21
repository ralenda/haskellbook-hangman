module HangmanIO where

import           Data.Maybe
import           Data.List
import           Control.Monad.Free
import           Hangman
import           System.Random (randomRIO)


-- loading word list and random generation

type WordList = [Hangman.Word]

loadWordList :: FilePath -> IO WordList
loadWordList = fmap processWords . readFile
  where processWords = mapMaybe word . lines

select :: [a] -> IO a
select l =
  let maxBound = length l - 1
  in (l !!) <$> randomRIO (0, maxBound)

-- User interaction functions


showRunningState :: InternalState -> String
showRunningState is =
    showCurrentWord is ++ "\n" ++ showTries is
  where
    showCurrentWord = intersperse ' ' . map showCurrentChar . currentWord
    showTries = ("Tries left: " ++) . show . triesLeft
    showCurrentChar (Just c) = c
    showCurrentChar Nothing = '_'

getGuess :: IO Guess
getGuess = do
  putStrLn "Enter your guess:"
  c <- getChar
  let theGuess = guess c
  case theGuess of
    Just c -> return c
    Nothing -> do
      putStrLn "Invalid Guess."
      getGuess

runGame :: Hangman.HangmanGame a -> IO ()
runGame (Free (PlayerTurn is next)) = do
  putStrLn $ "\n" ++ showRunningState is
  theGuess <- getGuess
  runGame $ next theGuess
runGame (Free (GameOver ts)) = putStrLn "The End"

-- main

allWords :: IO Hangman.Word
allWords = loadWordList "data/dict.txt" >>= select

newGame :: IO ()
newGame = allWords >>= runGame . playGame . Hangman.createInitialState sevenTries
