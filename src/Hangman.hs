module Hangman where

import           Control.Monad.Free
import           Data.Char
import           Data.Maybe


data InternalState = InternalState {
  wordToGuess          :: String,
  lowerCaseWordToGuess :: String,
  usedLetters          :: String,
  triesLeft            :: Int,
  currentWord          :: [Maybe Char]
} deriving (Show)

data TerminalState = Win InternalState | Loss InternalState deriving (Show)
newtype RunningState = RunningState InternalState deriving (Show)

internalStateFromRunning :: RunningState -> InternalState
internalStateFromRunning (RunningState is) = is

data GameState = Terminal TerminalState | Running RunningState deriving (Show)

data InitialStateCreationErrors = BadWord | InvalidNumberOfTries deriving (Eq, Ord, Show)

createInitialState :: Int -> String -> Either [InitialStateCreationErrors] RunningState
createInitialState tries word = createInitialState' (validateTries tries) (validateWord word)
  where
    validateTries n = if n < 1 then Left [InvalidNumberOfTries] else Right n
    validateWord w = if any isPlayableLetter w then Right w else Left [BadWord]

createInitialState' :: Either [InitialStateCreationErrors] Int -> Either [InitialStateCreationErrors] String -> Either [InitialStateCreationErrors] RunningState
createInitialState' (Left e1) (Left e2) = Left $ e1 ++ e2
createInitialState' (Left e1) _ = Left e1
createInitialState' _ (Left e2) = Left e2
createInitialState' (Right tries) (Right word) = Right $ RunningState InternalState {
  wordToGuess = word,
  lowerCaseWordToGuess = map toLower word,
  triesLeft = tries,
  usedLetters = [],
  currentWord = map (\c -> if isPlayableLetter c then Nothing else Just c) word
}

isPlayableLetter :: Char -> Bool
isPlayableLetter c = case generalCategory c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  _               -> False

applyGuess :: RunningState -> Char -> GameState
applyGuess (RunningState is) c
    | all isJust (currentWord newIs) = Terminal $ Win newIs
    | triesLeft newIs == 0 = Terminal $ Loss newIs
    | otherwise = Running $ RunningState newIs
  where newIs = applyGuess' is c

applyGuess' :: InternalState -> Char -> InternalState
applyGuess' is c
  | c' `elem` usedLetters is = is
  | c' `elem` lowerCaseWordToGuess is = is {
    usedLetters = c' : usedLetters is,
    currentWord = zipWith (getCharToShow c') (wordToGuess is) (currentWord is)
  }
  | otherwise = is {
    usedLetters = c' : usedLetters is,
    triesLeft = triesLeft is - 1
  }
  where
    c' = toLower c

    getCharToShow :: Char -> Char -> Maybe Char -> Maybe Char
    getCharToShow _ _ (Just x) = Just x
    getCharToShow guess c _
      | guess == toLower c = Just c
      | otherwise = Nothing

-- Free Monad to abstract the game loop

data HangmanGameF a =
    PlayerTurn InternalState (Char -> a) -- PlayerTurn: gives the current state to show to the player and wait for input
  | GameOver TerminalState               -- GameOver: end of the game; either won or loss

instance Functor HangmanGameF where
  fmap f (PlayerTurn is g) = PlayerTurn is (f . g)
  fmap _ (GameOver ts)     = GameOver ts

type HangmanGame a = Free HangmanGameF a

-- convenience methods
gameOver :: TerminalState -> HangmanGame ()
gameOver ts = liftF $ GameOver ts

playerTurn :: InternalState -> HangmanGame Char
playerTurn is = liftF $ PlayerTurn is id

-- Game loop
playGame :: RunningState -> HangmanGame ()
playGame rs = do
  c <- playerTurn (internalStateFromRunning rs)
  case applyGuess rs c of
    Running rs' -> playGame rs'
    Terminal ts -> gameOver ts
