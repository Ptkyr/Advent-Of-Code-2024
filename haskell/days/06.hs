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

aocParse :: Parser (Start, Bounds, Walls)
aocParse = do
  squares <- some square <* eof
  let start = snd <$> find ((== Start) . fst) squares
  let walls = map snd $ filter ((== Block) . fst) squares
  pure
    ( sourceToCoord $ fromJust start,
      ((1, 1), sourceToCoord . snd $ last squares),
      HS.fromList $ map sourceToCoord walls
    )
 where
  square :: Parser (Square, SourcePos)
  square =
    choice
      [ (Block,) <$> getSourcePos <* lexeme "#",
        (Start,) <$> getSourcePos <* lexeme "^",
        (Empty,) <$> getSourcePos <* lexeme "."
      ]
  sourceToCoord :: SourcePos -> (Int, Int)
  sourceToCoord pos = liftT1 unPos (sourceLine pos, sourceColumn pos)

partOne :: (Start, Bounds, Walls) -> Int
partOne (start, bnds, walls) = HM.size $ patrol bnds walls (HM.singleton start [U]) start U

partTwo :: (Start, Bounds, Walls) -> Int
partTwo (start, bnds, walls) =
  HM.size
    $ HM.filterWithKey
      (\coord _ -> HM.null $ patrol bnds (HS.insert coord walls) (HM.singleton start [U]) start U)
    $ patrol bnds walls (HM.singleton start [U]) start U

patrol :: Bounds -> Walls -> Visiteds -> Coord -> Direction -> Visiteds
patrol bnds walls set cur dir
  | infinite = HM.empty
  | null path = patrol bnds walls set cur (cc90 dir)
  | oob $ stepDir dir newPos = newSet
  | otherwise = patrol bnds walls newSet newPos (cc90 dir)
 where
  infinite = not . null $ mapMaybe (flip HM.lookup set >=> find (== dir)) path
  newSet = foldl' (\acc c -> HM.insertWith (++) c [dir] acc) set $ cur : path
  newPos = last path
  path = pathUntil (cOr oob $ flip HS.member walls) (unitVec dir) cur
  oob :: Coord -> Bool
  oob = not . inRange bnds
