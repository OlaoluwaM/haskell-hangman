module Lib where

import Control.Lens
import Control.Monad (forever, when)
import Data.Bool (bool)
import Data.Char (isAlpha, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (($>))
import Data.List (elemIndices, intersperse)
import Data.Maybe (fromMaybe, isJust, isNothing)
import PyF
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)
import Test.QuickCheck

-- ---------------------------------- Types --------------------------------- --
type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Arbitrary Puzzle where
    arbitrary = Puzzle <$> (arbitrary @String) <*> (arbitrary @[Maybe Char]) <*> arbitrary @String

-- ---------------------------------- Types --------------------------------- --

-- -------------------------------- Utilities ------------------------------- --
toLowerCaseStr :: String -> String
toLowerCaseStr = map toLower

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst predicate (x : xs) = if predicate x then [x] else filterFirst predicate xs

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe a = Just a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- -------------------------------- Utilities ------------------------------- --

-- --------------------------------- Helpers -------------------------------- --
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

getRandomWord :: WordList -> IO String
getRandomWord wl = (wl !!) <$> randomRIO (0, length wl - 1)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    let rawWords = lines dict
    let caseInsensitiveWords = map toLowerCaseStr rawWords
    return $ nubOrd caseInsensitiveWords

-- --------------------------------- Helpers -------------------------------- --
instance Show Puzzle where
    show (Puzzle _ discovered guesses) =
        [fmt|\
{discoveredChars}
Guessed so far: {guessChars}\
    |]
      where
        guessChars = intersperse ',' guesses
        discoveredChars = intersperse ' ' $ renderPuzzleChar <$> discovered
        renderPuzzleChar = fromMaybe '_'
gameWords :: IO WordList
gameWords = filter isGameLength <$> allWords
  where
    isGameLength w = let lngth = length w in lngth >= minWordLength && lngth < maxWordLength

randomWord :: IO String
randomWord = gameWords >>= getRandomWord

newPuzzle :: String -> Puzzle
newPuzzle word = let discovered = map (const Nothing) word in Puzzle word discovered []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = (`elem` word)

-- alreadyGuessed :: Puzzle -> Char -> Bool
-- alreadyGuessed (Puzzle _ _ guesses) = (`elem` guesses)

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle ogWord discovered guesses) guessedChar = fromMaybe (Puzzle ogWord discovered newGuessesLst) $ do
    -- Failing here means the guessedChar does no exist in OgWord
    indices' <- listToMaybe $ elemIndices guessedChar ogWord
    -- Failing here means that all occurrences of the char have already been guessed
    firstUndiscoveredIndex <- safeHead $ filterFirst (isNothing . (discovered !!)) indices'
    let newDiscoveredLst = discovered & ix firstUndiscoveredIndex ?~ guessedChar
    return $ Puzzle ogWord newDiscoveredLst newGuessesLst
  where
    newGuessesLst = guessedChar : guesses

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
    if charInWord puzzle guess
        then
            putStrLn correctGuessOutput $> fillInCharacter puzzle guess
        else
            putStrLn wrongGuessOutput $> fillInCharacter puzzle guess
  where
    correctGuessOutput = [fmt|{guess} is in the word! Filling it in|]
    wrongGuessOutput = [fmt|{guess} wasn't in the word. Try again|]

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word discovered guessed) = do
    let maxIncorrectGuessCount = 7
    let correctGuessCount = length $ filter isJust discovered
    let incorrectGuessCount = length guessed - correctGuessCount
    let guessesLeft = maxIncorrectGuessCount - incorrectGuessCount

    -- putStrLn [fmt|Correct guess count: {correctGuessCount}|]
    -- putStrLn [fmt|Incorrect guess count: {incorrectGuessCount}|]

    let pluralGuessLeftOutput = [fmt|You have {guessesLeft} guesses left|]
    let singularGuessLeftOutput = [fmt|You have {guessesLeft} guess left|]

    putStrLn $ bool singularGuessLeftOutput pluralGuessLeftOutput (guessesLeft == 1)

    when (incorrectGuessCount >= maxIncorrectGuessCount) $ do
        putStrLn "You lose!"
        putStrLn [fmt|The word was: {word}|]
        exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) = when (all isJust discovered) $ do
    putStrLn "You win!"
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn [fmt|Current puzzle is {show puzzle}|]
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c]
            | isAlpha c -> handleGuess puzzle c >>= \updatedPuzzle -> putStrLn "" >> runGame updatedPuzzle
            | otherwise -> putStrLn "You guess must be a proper character" >> putStrLn ""
        _ -> putStrLn "Your guess must be a single character" >> putStrLn ""

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord
    let puzzle = newPuzzle word
    runGame puzzle
