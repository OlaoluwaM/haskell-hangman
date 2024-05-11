module TestsSpec where

import Data.Foldable (Foldable (foldl'))
import Data.Maybe (isJust)
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

genDisjointedWordGuessesPair :: Gen (NonEmptyList Char, [Char])
genDisjointedWordGuessesPair = do
  ogWord <- arbitrary @(NonEmptyList Char)
  guessChar <- vectorOf 10 $ go $ getNonEmpty ogWord
  pure (ogWord, guessChar)
 where
  go :: [Char] -> Gen Char
  go word = do
    guessChar' <- arbitrary @Char
    if guessChar' `notElem` word then pure guessChar' else go word

genRelatedWordGuessPair :: Gen (NonEmptyList Char, Char)
genRelatedWordGuessPair = do
  (NonEmpty word) <- arbitrary @(NonEmptyList Char)
  guessChar <- elements word
  pure (NonEmpty word, guessChar)

genDisjointedWordGuessPair :: Gen (NonEmptyList Char, Char)
genDisjointedWordGuessPair = do
  ogWord <- arbitrary @(NonEmptyList Char)
  guess <- go $ getNonEmpty ogWord
  pure (ogWord, guess)
 where
  go :: [Char] -> Gen Char
  go word = do
    guessChar' <- arbitrary @Char
    if guessChar' `notElem` word then pure guessChar' else go word

spec :: Spec
spec = do
  describe "Hangman Test" $ do
    describe "fillInCharacter" $ do
      prop "fillInCharacter with char not in word" $ forAll genDisjointedWordGuessesPair $ \(NonEmpty ogWord, guesses) ->
        let puzzle@(Puzzle _ expectedGuesses _) = newPuzzle ogWord
            (Puzzle _ resultGuesses resultAlreadyGuessed) = foldl' fillInCharacter puzzle guesses
         in conjoin [resultGuesses `shouldMatchList` expectedGuesses, guesses `shouldSatisfy` all (`elem` resultAlreadyGuessed)]

      prop "fillInCharacter with char in word" $ forAll genRelatedWordGuessPair $ \(NonEmpty word, guess) ->
        let guesses = guess : ['c', 'c']
            word' = word ++ "cc"
            puzzle = newPuzzle word'
            (Puzzle _ resultGuesses resultAlreadyGuessed) = foldl' fillInCharacter puzzle guesses
            validGuesses = filter isJust resultGuesses
         in conjoin [length validGuesses `shouldBe` length guesses, guesses `shouldSatisfy` all (`elem` resultAlreadyGuessed)]

    describe "charInWord" $ do
      prop "returns true if a guess is a valid character in the puzzle word" $ forAll genRelatedWordGuessPair $ \(NonEmpty word, guess) ->
        let puzzle = newPuzzle word in charInWord puzzle guess `shouldBe` True

      prop "returns false if a guess is not a valid character in the puzzle word" $ forAll genDisjointedWordGuessPair $ \(NonEmpty word, guess) ->
        let puzzle = newPuzzle word in charInWord puzzle guess `shouldBe` False
