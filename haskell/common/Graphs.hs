module Graphs
  ( module Graphs,
    module Data.Graph.Inductive,
  )
where

import Data.Graph.Inductive
import Arrays
import Utils

type Edg = LEdge ()

buildGraph :: (Coord -> Coord -> Bool) -> Arr2D a -> Gr a ()
buildGraph nbr arr = mkGraph nodelist edgelist
 where
  bnds = bounds arr
  len = add1 . fst $ snd bnds
  (nodelist, edgelist) = foldl' convert ([], []) $ assocs arr
   where
    convert :: ([LNode a], [Edg]) -> (Coord, a) -> ([LNode a], [Edg])
    convert (ns, es) (idx, val) =
      ( (nodeid, val) : ns,
        map
          ((nodeid,,()) . to1D len)
          (filter ((&&) <$> inRange bnds <*> nbr idx) $ cardinals idx)
          ++ es
      )
     where
      nodeid = to1D len idx
