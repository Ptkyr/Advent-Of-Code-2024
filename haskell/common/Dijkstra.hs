module Dijkstra where

import Arrays
import qualified Data.PriorityQueue.FingerTree as PQ
import Utils

-- Dijkstra's
type Graph = Arr2D Node

type Node = Char

type DijkPQ = PQ.PQueue Int Coord

data Dijkstra = Dijkstra
    { _start :: Coord
    , _end :: Coord
    , _graph :: Graph
    , _costs :: Arr2D Int
    }
    deriving (Show)

neighbours :: Graph -> Coord -> (Node -> Node -> Bool) -> [Coord]
neighbours graph v@(vx, vy) fltr =
    filter liftFilter $
        map (clamp2D (bounds graph)) nbrslist
  where
    nbrslist =
        [ (vx - 1, vy)
        , (vx + 1, vy)
        , (vx, vy - 1)
        , (vx, vy + 1)
        ]
    liftFilter :: (Int, Int) -> Bool
    liftFilter nbr = fltr vAt nbrAt && nbr /= v -- fix clamping
      where
        nbrAt = graph ! nbr
        vAt = graph ! v

dijkstra :: (Coord -> Bool) -> (Node -> Node -> Bool) -> Dijkstra -> Int
dijkstra endCond adjCond info@(Dijkstra s e _ c) =
    dijk' info{_costs = c // [(s, 0)]} $
        PQ.singleton 0 s
  where
    dijk' :: Dijkstra -> DijkPQ -> Int
    dijk' d@(Dijkstra _ _ graph costs) pq = case PQ.minViewWithKey pq of
        Nothing -> error "Unreachable"
        Just ((cost, coord), pq')
            | endCond coord -> costs ! coord
            | otherwise -> dijk' d{_costs = recurCosts} recurPQ
          where
            (recurCosts, recurPQ) = updateCosts (neighbours graph coord adjCond) costs pq'
            newCost = cost + 1
            updateCosts :: [Coord] -> Arr2D Int -> DijkPQ -> (Arr2D Int, DijkPQ)
            updateCosts [] oldC oldPQ = (oldC, oldPQ)
            updateCosts (n : nbrs) oldC oldPQ
                | newCost < costs ! n =
                    updateCosts nbrs (oldC // [(n, newCost)]) $
                        PQ.insert newCost n oldPQ
                | otherwise = updateCosts nbrs oldC oldPQ
