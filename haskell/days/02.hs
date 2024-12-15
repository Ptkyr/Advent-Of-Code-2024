{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "02")

type Level = [Int]

aocParse :: Parser [Level]
aocParse = (some hnat) `endBy` newline <* eof

partOne :: [Level] -> Int
partOne = length . filter safe

partTwo :: [Level] -> Int
partTwo = length . filter (safe2 [])
 where
  safe2 :: Level -> Level -> Bool
  safe2 _ [] = False
  safe2 xs (y : ys) = (safe $ xs ++ ys) || safe2 (xs ++ [y]) ys

safe :: Level -> Bool
safe = cAnd closeBy $ cOr ascending descending
 where
  ascending = and . mapAdjacent (<)
  descending = and . mapAdjacent (>)
  closeBy = and . mapAdjacent (((>=) 3) . abs .: (-))
