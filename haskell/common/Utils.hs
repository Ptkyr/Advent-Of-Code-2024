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
    numberLength,
    module Data.Maybe,
    module Control.Monad,
  )
where

import Data.Maybe
import Data.NumberLength
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

-- 2D Vector-related things
data Direction = U | R | D | L deriving (Show, Eq, Ord, Enum)
cc90 :: Direction -> Direction
cc90 d = toEnum $ (fromEnum d + 1) `mod` 4

ccw90 :: Direction -> Direction
ccw90 d = toEnum $ (fromEnum d - 1) `mod` 4

unitVec :: Direction -> Coord
unitVec U = (-1, 0)
unitVec R = (0, 1)
unitVec D = (1, 0)
unitVec L = (0, -1)

stepDir :: Direction -> Coord -> Coord
stepDir dir = liftT2 (+) (unitVec dir)

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

nthTri :: Int -> Int
nthTri n = (n * (n + 1)) `div` 2

cardinals :: Coord -> [Coord]
cardinals (vx, vy) =
  [ (vx - 1, vy),
    (vx + 1, vy),
    (vx, vy - 1),
    (vx, vy + 1)
  ]

ordinals :: Coord -> [Coord]
ordinals (vx, vy) =
  [ (vx - 1, vy + 1),
    (vx + 1, vy + 1),
    (vx + 1, vy - 1),
    (vx + 1, vy + 1)
  ]

cordinals :: Coord -> [Coord]
cordinals = phoenix (++) cardinals ordinals

-- Generates line segment between two endpoints
fillLine :: Coord -> Coord -> [Coord]
fillLine src dst = generateWhile (dst /=) (liftT2 (+) $ unit) src
 where
  dir = liftT2 (-) dst src
  unit = liftT1 (clamp (-1, 1)) dir

-- Travels in the direction of vec, stopping right before cond is satisfied
laserUntil :: (Coord -> Bool) -> Coord -> Coord -> Coord
laserUntil cond vec cur = if cond next then cur else laserUntil cond vec next
 where
  next = liftT2 (+) vec cur

-- Travels in the direction of vec, stopping right before cond is satisfied
pathUntil :: (Coord -> Bool) -> Coord -> Coord -> [Coord]
pathUntil cond vec cur = if cond next then [] else next : pathUntil cond vec next
 where
  next = liftT2 (+) vec cur

-- Travels in the direction of vec, returning the coord satisfying cond
laserTo :: (Coord -> Bool) -> Coord -> Coord -> Coord
laserTo cond vec = liftT2 (+) vec . laserUntil cond vec

-- Travels in the direction of vec, stopping right before cond is satisfied
pathTo :: (Coord -> Bool) -> Coord -> Coord -> [Coord]
pathTo cond vec cur = liftT2 (+) vec (head path) : path
 where
  path = pathUntil cond vec cur

-- Computes 2y - x
project :: Coord -> Coord -> Coord
project x y = traveller x y y

-- Computes an adder in the direction and magnitude of y - x
traveller :: Coord -> Coord -> Coord -> Coord
traveller x y = liftT2 (+) $ liftT2 (-) y x

-- Combinators
phi :: (b -> y -> c) -> (a -> b) -> (x -> y) -> a -> x -> c
phi bin un1 un2 a1 a2 = bin (un1 a1) (un2 a2)

phoenix :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
phoenix bin un1 un2 arg = bin (un1 arg) (un2 arg)

yharon :: (b -> c -> d -> e) -> (a -> b) -> (a -> c) -> a -> d -> e
yharon t u1 u2 a = t (u1 a) (u2 a)

liftT1 :: (a -> b) -> (a, a) -> (b, b)
liftT1 f (x, y) = (f x, f y)

liftT2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftT2 f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

cAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
cAnd = liftM2 (&&)

cOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
cOr = liftM2 (||)

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x : xs) = f x xs (para f b xs)

-- Some list util
unalternate :: [a] -> ([a], [a])
unalternate (x : y : zs) = liftT2 (:) (x, y) $ unalternate zs
unalternate (z : _) = ([z], [])
unalternate [] = ([], [])

fromPairs :: [(a, b)] -> ([a], [b])
fromPairs lst = (map fst lst, map snd lst)

takeWhileP1 :: (a -> Bool) -> [a] -> [a]
takeWhileP1 p = foldr (\x ys -> if p x then x : ys else [x]) []

generateWhile :: (a -> Bool) -> (a -> a) -> a -> [a]
generateWhile cond f x = takeWhileP1 cond $ iterate f x

remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

wrapConvert :: ([a] -> [a]) -> (b -> a) -> (a -> b) -> [b] -> [b]
wrapConvert apply to from = map from . apply . map to

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ transpose [xs, ys]

halve :: [a] -> ([a], [a])
halve = splitAt <$> flip div 2 . (+ 1) . length <*> id

-- Maybe util
isSomeAnd :: (a -> Bool) -> Maybe a -> Bool
isSomeAnd _ Nothing = False
isSomeAnd f (Just a) = f a

-- Statistics
welford :: [Float] -> (Float, Float)
welford l = (m, v / (c - 1))
 where
  (c, m, v) = foldr update (0.0, 0.0, 0.0) l
  update :: Float -> (Float, Float, Float) -> (Float, Float, Float)
  update x (cnt, mn, var) = (new_c, new_mn, new_var)
   where
    new_c = cnt + 1
    delta = x - mn
    new_mn = delta / new_c
    delta2 = x - new_mn
    new_var = var + delta * delta2
