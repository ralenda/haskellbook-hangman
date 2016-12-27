module Hangman where

import           Control.Monad.Free
import           Data.Char
import           Data.Maybe


data InternalState = InternalState {
  wordToGuess :: String,
  usedLetters :: String,
  triesLeft   :: Int,
  currentWord :: [Maybe Char]
} deriving (Show)

newtype RunningState = RunningState InternalState deriving (Show)

internalStateFromRunning :: RunningState -> InternalState
internalStateFromRunning (RunningState is) = is

-- Initial state creation, with safe constructors

newtype Tries = Tries Int
newtype Word = Word String

tries :: Int -> Maybe Tries
tries n = if n > 0 then Just (Tries n) else Nothing

word :: String -> Maybe Hangman.Word
word w = if any isPlayableLetter w then Just (Word w) else Nothing

-- Some popular number of tries, defined for convenience
sevenTries :: Tries
sevenTries = Tries 7

isPlayableLetter :: Char -> Bool
isPlayableLetter c = case generalCategory c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  _               -> False

createInitialState :: Tries -> Hangman.Word -> RunningState
createInitialState (Tries tries) (Word word) = RunningState InternalState {
  wordToGuess = word,
  triesLeft = tries,
  usedLetters = [],
  currentWord = map (\c -> if isPlayableLetter c then Nothing else Just c) word
}


-- Game step: apply a guess

newtype Guess = Guess Char

guess :: Char -> Maybe Guess
guess c = if isPlayableLetter c then Just $ Guess (toLower c) else Nothing

data TerminalState = Win InternalState | Loss InternalState deriving (Show)
data GameState = Terminal TerminalState | Running RunningState deriving (Show)

applyGuess :: RunningState -> Guess -> GameState
applyGuess (RunningState is) g
    | all isJust (currentWord newIs) = Terminal $ Win newIs
    | triesLeft newIs == 0 = Terminal $ Loss newIs
    | otherwise = Running $ RunningState newIs
  where newIs = applyGuess' is g

applyGuess' :: InternalState -> Guess -> InternalState
applyGuess' is (Guess c)
  | c `elem` usedLetters is = is
  | any (eqIgnoreCase c) (wordToGuess is) = is {
    usedLetters = c : usedLetters is,
    currentWord = zipWith (getCharToShow c) (wordToGuess is) (currentWord is)
  }
  | otherwise = is {
    usedLetters = c : usedLetters is,
    triesLeft = triesLeft is - 1
  }
  where
    eqIgnoreCase c c' = c == toLower c'

    getCharToShow :: Char -> Char -> Maybe Char -> Maybe Char
    getCharToShow _ _ (Just x) = Just x
    getCharToShow guess c _
      | guess == toLower c = Just c
      | otherwise = Nothing

-- Free Monad to abstract the game loop

data HangmanGameF a =
    PlayerTurn InternalState (Guess -> a) -- PlayerTurn: gives the current state to show to the player and wait for input
  | GameOver TerminalState                -- GameOver: end of the game; either won or loss

instance Functor HangmanGameF where
  fmap f (PlayerTurn is g) = PlayerTurn is (f . g)
  fmap _ (GameOver ts)     = GameOver ts

type HangmanGame a = Free HangmanGameF a

-- convenience methods
gameOver :: TerminalState -> HangmanGame ()
gameOver ts = liftF $ GameOver ts

playerTurn :: InternalState -> HangmanGame Guess
playerTurn is = liftF $ PlayerTurn is id

-- Game loop
playGame :: RunningState -> HangmanGame ()
playGame rs = do
  c <- playerTurn (internalStateFromRunning rs)
  case applyGuess rs c of
    Running rs' -> playGame rs'
    Terminal ts -> gameOver ts
