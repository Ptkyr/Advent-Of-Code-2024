{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Graphs
import Mayn
import Parsing
import Utils

$(generateMain "10")

type Input = Gr Int ()
type Trail = [Node]

aocParse :: Parser Input
aocParse = do
  arr <- listArr2D0 <$> some hdigit `endBy` newline <* eof
  pure $ buildGraph ((== (-1)) .: on (-) (arr !)) arr

partOne :: Input -> Int
partOne graph =
  sum
    . map ((length . filter (isSomeAnd (== 9) . lab graph)) . flip bfs graph)
    $ trailheads graph

partTwo :: Input -> Int
partTwo graph = length $ foldl' extensions (map singleton $ trailheads graph) [1 .. 9]
 where
  extensions :: [Trail] -> Int -> [Trail]
  extensions trails height = concatMap extend trails
   where
    extend :: Trail -> [Trail]
    extend tl@(x : _) =
      map (: tl) . filter (isSomeAnd (== height) . lab graph) $ neighbors graph x
    extend _ = error "Unreachable"

trailheads :: Input -> [Node]
trailheads graph = filter (isSomeAnd (== 0) . lab graph) $ nodes graph
