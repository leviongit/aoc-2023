{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Choice = LeftC | RightC deriving (Eq, Show)

newtype StateTag = StateTag { getStateTag :: Text } deriving (Eq, Show, Ord)

parseToChoices :: Text -> [Choice]
parseToChoices "" = []
parseToChoices s = case T.head s of
  'L' -> LeftC : parseToChoices (T.tail s)
  'R' -> RightC : parseToChoices (T.tail s)
  _ -> undefined

cS f x y = f <$> x <*> y 

choice (left, _) LeftC = left
choice (_, right) RightC = right

parseAlts :: Text -> (StateTag, StateTag)
parseAlts s = 
  T.split (== ',') s &
  map (T.filter (cS (&&) (cS (&&) (/= '(') (/= ')')) (/= ' '))) &
  map StateTag &
  \[x, y] -> (x, y) -- assume correct data

readStateMap :: [Text] -> [(StateTag, (StateTag, StateTag))]
readStateMap ss = 
  map (T.split (== '=')) ss &
  map (map T.strip) &
  map (\[x, y] -> (StateTag x, parseAlts y)) -- assume correct data

find :: StateTag -> [(StateTag, a)] -> Maybe a
find _ [] = Nothing
find s (st : sts) = if s == (fst st) then Just (snd st) else find s sts

main :: IO ()
main = do
  f <- T.readFile "input"
  let css : _ : sts = T.lines f 
      cs = cycle . parseToChoices $ css
      stm = readStateMap sts in
    print $ go cs stm (StateTag "AAA") 0 where
      go (c : cs) stm curs i = 
        let 
          Just found = find curs stm
          next = choice found c in
          if getStateTag next == "ZZZ" then 
            i + 1            else
              go cs stm next (i + 1)
      go _ _ _ _ = undefined


