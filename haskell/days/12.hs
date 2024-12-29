{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Data.HashSet qualified as HS
import Mayn
import Parsing
import Utils

$(generateMain "12")

type Input = Arr2D Char
type CoordSet = HS.HashSet Coord

aocParse :: Parser Input
aocParse = listArr2D1 <$> some upperChar `endBy` newline <* eof

partOne :: Input -> Int
partOne = regions (+ 1)

partTwo :: Input -> Int
partTwo = regions id

regions :: (Int -> Int) -> Input -> Int
regions f grid = snd $ foldl' score (HS.empty, 0) $ indices grid
 where
  score old@(seen, acc) start
    | HS.member start seen = old
    | otherwise = (HS.union points seen, acc + scorer f points)
   where
    points = floodfill grid start

scorer :: (Int -> Int) -> CoordSet -> Int
scorer f set =
  (*) (HS.size set)
    . foldr folder 0
    . remove (\(p, d) -> HS.member (stepDir d p) set)
    . concatMap (\x -> map (x,) [U, L, D, R])
    $ HS.toList set
 where
  folder (p, d) perim
    | not (HS.member nbr set)
    || HS.member (stepDir d nbr) set = perim + 1
    | otherwise = f perim
   where
    nbr = stepDir (cc90 d) p

floodfill :: Input -> Coord -> CoordSet
floodfill grid start = floodhelp [start] $ HS.singleton start
 where
  cur = grid ! start
  floodhelp :: [Coord] -> CoordSet -> CoordSet
  floodhelp [] set = set
  floodhelp (x : xs) set = floodhelp stack newset
   where
    (stack, newset) = foldr popstack (xs, set) $ cardinals x
    popstack :: Coord -> ([Coord], CoordSet) -> ([Coord], CoordSet)
    popstack nbr old@(stacc, setacc)
      | not (inRange (bounds grid) nbr)
      || HS.member nbr setacc 
      || grid ! nbr /= cur = old
      | otherwise = (nbr : stacc, HS.insert nbr setacc)
