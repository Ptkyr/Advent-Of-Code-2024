{-# LANGUAGE TemplateHaskell #-}

import Data.HashMap.Strict qualified as HM
import Mayn
import Parsing
import Utils

$(generateMain "11")

type Input = Blinker
type Stone = Int
type Blinker = HM.HashMap Stone Int

aocParse :: Parser Input
aocParse = foldr (`HM.insert` 1) HM.empty <$> some nat <* eof

partOne :: Input -> Int
partOne = solver 25

partTwo :: Input -> Int
partTwo = solver 75

solver :: Int -> Input -> Int
solver x = HM.foldr (+) 0 . (!! x) . iterate blinker

blinker :: Blinker -> Blinker
blinker = foldr eachEntry HM.empty . HM.toList
 where
  eachEntry :: (Stone, Int) -> Blinker -> Blinker
  eachEntry (stone, cnt) hm = foldr (flip (HM.insertWith (+)) cnt) hm $ blink stone

blink :: Stone -> [Stone]
blink 0 = [1]
blink x
  | even $ length strx = [read l, read r]
  | otherwise = [x * 2024]
 where
  strx = show x
  (l, r) = halve strx
