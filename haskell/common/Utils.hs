module Utils
  ( module Utils,
    module Data.List,
    module Data.Ord,
    module Data.Function,
    module Data.Composition,
    amap,
    mapAdjacent,
    digitToInt,
    module Debug.Trace,
    liftM2,
  )
where

import Control.Applicative hiding (some)
import Debug.Trace

-- mostly for .:
import Control.Monad
import Data.Char (digitToInt)
import Data.Composition
import Data.Function
import Data.List
import Data.List.HT (mapAdjacent)
import Data.Ord
import GHC.Arr (amap)

-- Typedefs
type Coord = (Int, Int)

type Coords = [Coord]

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

takeWhileP1 :: (a -> Bool) -> [a] -> [a]
takeWhileP1 p = foldr (\x ys -> if p x then x : ys else [x]) []

generateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
generateWhile cond f x = takeWhileP1 cond $ iterate f x

-- Generates line segment between two endpoints
fillLine :: Coord -> Coord -> [Coord]
fillLine src dst = generateWhile (dst /=) (liftT2 (+) $ unit) src
  where
    dir = liftT2 (-) dst src
    unit = liftT1 (clamp (-1, 1)) dir

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

cAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
cAnd = liftM2 (&&)

cOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
cOr = liftM2 (||)
