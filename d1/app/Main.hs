{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Control.Monad (foldM)
import Data.Char (isDigit)
import GHC.IO (unsafePerformIO)

part1 = do
  f <- readFile "input"
  ns <- 
    foldM
      (\x y -> pure $ x + (getNumFromDigitList $ filter isDigit y)) 
      0 
      (lines f)
  print ns

-- zero
-- one
-- two
-- three
-- four
-- five
-- six
-- seven
-- eight
-- nine

eatLineForward = \case
  x : xs | isDigit x -> pure x
  'z' : 'e' : 'r' : 'o' : _ -> "0"
  'o' : 'n' : 'e' : _ -> "1"
  't' : xs -> case xs of
    'w' : 'o' : _ -> "2"
    'h' : 'r' : 'e' : 'e' : _ -> "3"
    _ -> eatLineForward xs
  'f' : xs -> case xs of
    'o' : 'u' : 'r' : _ -> "4"
    'i' : 'v' : 'e' : _ -> "5"
    _ -> eatLineForward xs
  's' : xs -> case xs of
    'i' : 'x' : _ -> "6"    
    'e' : 'v' : 'e' : 'n' : _ -> "7"
    _ -> eatLineForward xs
  'e' : 'i' : 'g' : 'h' : 't' : _ -> "8"
  'n' : 'i' : 'n' : 'e' : _ -> "9"
  [] -> ""
  _ : xs -> eatLineForward xs

-- eno   - 1
-- enin  - 9
-- eerht - 3
-- evif  - 5
-- orez  - 0
-- owt   - 2
-- ruof  - 4
-- xis   - 6
-- neves - 7
-- thgie - 8

eatLineBackward = (\case
  x : _ | isDigit x -> pure x
  'e' : xs -> case xs of
    'n' : ys -> case ys of
      'o' : _ -> "1"
      'i' : 'n' : _ -> "9"
      _ -> eatLineBackward ys
    'e' : 'r' : 'h' : 't' : _ -> "3"
    'v' : 'i' : 'f' : _ -> "5"
    _ -> eatLineBackward xs
  'o' : xs -> case xs of
    'r' : 'e' : 'z' : _ -> "0"
    'w' : 't' : _ -> "2"
    _ -> eatLineBackward xs
  'r' : 'u' : 'o' : 'f' : _ -> "4"
  'x' : 'i' : 's' : _ -> "6"
  'n' : 'e': 'v' : 'e': 's' : _ -> "7"
  't' : 'h' : 'g' : 'i' : 'e' : _ -> "8"
  _ : xs -> eatLineBackward xs
  )

getNumFromDigitList = read @Int <$> ((:) <$> head <*> (pure . last))

part2 = do
  f <- readFile "input"
  print $ sum $ map (read @Int . ((<>) <$> eatLineForward <*> (eatLineBackward . reverse))) (lines f)
  

main :: IO ()
main = do
  part1
  putStrLn "==="
  part2
