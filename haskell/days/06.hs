{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Mayn
import Parsing
import Utils

$(generateMain "06")

data Square = Empty | Block | Start deriving (Show, Eq)
type Start = Coord
type Bounds = (Coord, Coord)
type Walls = HS.HashSet Coord
type Visiteds = HM.HashMap Coord [Direction]
type Input = (Start, Bounds, Walls)

aocParse :: Parser Input
aocParse = do
  squares <-
    listArr2D1
      <$> some (char '#' <|> char '^' <|> char '.')
      `endBy` newline
      <* eof
  let start = find ((== '^') . snd) $ assocs squares
  let walls = map fst $ filter ((== '#') . snd) $ assocs squares
  pure (fst $ fromJust start, bounds squares, HS.fromList walls)

partOne :: Input -> Int
partOne = solver id

partTwo :: Input -> Int
partTwo input@(start, bnds, walls) =
  solver
    ( HM.filterWithKey
        (\coord _ -> HM.null $ patrol bnds (HS.insert coord walls) (HM.singleton start [U]) start U)
    )
    input

solver :: (Visiteds -> Visiteds) -> Input -> Int
solver f (start, bnds, walls) = HM.size . f $ patrol bnds walls (HM.singleton start [U]) start U

patrol :: Bounds -> Walls -> Visiteds -> Coord -> Direction -> Visiteds
patrol bnds walls set cur dir
  | infinite = HM.empty
  | null path = patrol bnds walls set cur (cc90 dir)
  | oob $ stepDir dir newPos = newSet
  | otherwise = patrol bnds walls newSet newPos (cc90 dir)
 where
  infinite = not . null $ mapMaybe (flip HM.lookup set >=> find (== dir)) path
  newSet = foldl' (\acc c -> HM.insertWith (++) c [dir] acc) set path
  newPos = last path
  path = pathUntil (cOr oob $ flip HS.member walls) (unitVec dir) cur
  oob :: Coord -> Bool
  oob = not . inRange bnds
