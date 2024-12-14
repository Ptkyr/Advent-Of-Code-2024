module Utils (
    module Utils,
    module Data.List,
    module Data.Ord,
    module Data.Function,
    module Data.Composition,
    amap,
    mapAdjacent,
)
where

import Control.Applicative hiding (some)
import Data.Composition -- mostly for .:
import Data.Function
import Data.List
import Data.List.HT (mapAdjacent)
import Data.Ord
import GHC.Arr (amap)

-- Typedefs
type Coord = (Int, Int)

-- Misc util
add1 :: Int -> Int
add1 = flip (+) 1

sub1 :: Int -> Int
sub1 = flip (+) (-1)

countIf :: (a -> Bool) -> [a] -> Int
countIf f lst = length $ filter f lst

clamp2D :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)
clamp2D ((x, y), (x', y')) (a, b) = (clamp (x, x') a, clamp (y, y') b)

manhattan :: Coord -> Coord -> Int
manhattan = phoenix (+) fst snd .: liftT2 (abs .: (-))

-- Will drop the last element if odd
toPairs :: [a] -> [(a, a)]
toPairs (x : y : zs) = (x, y) : toPairs zs
toPairs [] = []
toPairs (z : zs) = []

fromPairs :: [(a, b)] -> ([a], [b])
fromPairs lst = (map fst lst, map snd lst)

-- Generates line segment between two endpoints
fillLine :: Coord -> Coord -> [Coord]
fillLine (x, y) (x', y')
    | x == x' = map (x,) [min y y' .. max y y']
    | y == y' = map (,y) [min x x' .. max x x']
    | otherwise = zip xList yList
  where
    xList =
        if x < x'
            then [x, x + 1 .. x']
            else [x, x - 1 .. x']
    yList =
        if y < y'
            then [y, y + 1 .. y']
            else [y, y - 1 .. y']

nthTri :: Int -> Int
nthTri n = (n * (n + 1)) `div` 2

-- Combinators
phi :: (b -> y -> c) -> (a -> b) -> (x -> y) -> a -> x -> c
phi bin un1 un2 a1 a2 = bin (un1 a1) (un2 a2)

phoenix :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
phoenix bin un1 un2 arg = bin (un1 arg) (un2 arg)

yharon :: (b -> c -> d -> e) -> (a -> b) -> (a -> c) -> a -> d -> e
yharon t u1 u2 a = t (u1 a) (u2 a)

liftT1 :: (a -> b) -> (a, a) -> (b, b)
liftT1 f (x, y) = (f x, f y)

liftT2 :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
liftT2 f a b = (on f fst a b, on f snd a b)
