{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Data.HashSet qualified as HS
import Mayn
import Parsing
import Utils

$(generateMain "08")

data Antennae = Antennae
  { freq :: Char,
    spots :: CoordSet
  }
  deriving (Show)

type CoordSet = HS.HashSet Coord
type Bounds = (Coord, Coord)
type Input = (Bounds, [Antennae])

aocParse :: Parser Input
aocParse = do
  squares <- listArr2D1 <$> some regexdot `endBy` newline <* eof
  let ants = filter ((/= '.') . snd) $ assocs squares
  let grouped = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) ants
  let convert = map (\a -> Antennae (snd $ head a) $ HS.fromList $ map fst a)
  pure (bounds squares, convert grouped)

partOne :: Input -> Int
partOne = solver reflect

partTwo :: Input -> Int
partTwo = solver pierce

type NodeFinder = (Coord -> Bool) -> NodeMaker
type NodeMaker = Coord -> Coord -> [Coord]

solver :: NodeFinder -> Input -> Int
solver finder (bnds, ants) = HS.size $ foldr (HS.union . antinodes (finder (inRange bnds))) HS.empty ants
 where
  antinodes :: NodeMaker -> Antennae -> CoordSet
  antinodes nf (Antennae _ nodes) = para func HS.empty $ HS.toList nodes
   where
    func :: Coord -> [Coord] -> CoordSet -> CoordSet
    func x tayl = HS.union (HS.fromList $ concatMap (nf x) tayl)

reflect :: NodeFinder
reflect valid x y = filter valid [project x y, project y x]

pierce :: NodeFinder
pierce valid x y =
  on
    (++)
    (takeWhile valid)
    (iterate (traveller x y) y)
    (iterate (traveller y x) x)

project :: Coord -> Coord -> Coord
project x y = traveller x y y

traveller :: Coord -> Coord -> Coord -> Coord
traveller x y = liftT2 (+) (liftT2 (-) y x)
