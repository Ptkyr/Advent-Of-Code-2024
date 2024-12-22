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
data Square = Empty | Ant deriving (Show, Eq)

aocParse :: Parser Input
aocParse = do
  squares <- some square <* eof
  let ants = filter ((/= '.') . snd) squares
  let grouped = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) ants
  let convert =
        map
          ( \a ->
              Antennae (snd $ head a) $
                HS.fromList $
                  map (sourceToCoord . fst) a
          )
  pure
    ( ((1, 1), sourceToCoord . fst $ last squares),
      convert grouped
    )
 where
  square :: Parser (SourcePos, Char)
  square = (,) <$> getSourcePos <*> lexeme (alphaNumChar <|> char '.')

partOne :: Input -> Int
partOne (bnds, ants) = HS.size $ foldr (HS.union . antinodes reflect bnds) HS.empty ants

partTwo :: Input -> Int
partTwo (bnds, ants) = HS.size $ foldr (HS.union . antinodes pierce bnds) HS.empty ants

type NodeFinder = Bounds -> Coord -> Coord -> [Coord]

antinodes :: NodeFinder -> Bounds -> Antennae -> CoordSet
antinodes nf bnds (Antennae _ nodes) = para func HS.empty $ HS.toList nodes
 where
  func :: Coord -> [Coord] -> CoordSet -> CoordSet
  func x tayl = HS.union (HS.fromList $ concatMap (nf bnds x) tayl)

reflect :: NodeFinder
reflect bnds x y = filter (inRange bnds) [project x y, project y x]

pierce :: NodeFinder
pierce bnds x y =
  on
    (++)
    (takeWhile (inRange bnds))
    (iterate (traveller x y) y)
    (iterate (traveller y x) x)

project :: Coord -> Coord -> Coord
project x y = traveller x y y

traveller :: Coord -> Coord -> Coord -> Coord
traveller x y = liftT2 (+) (liftT2 (-) y x)