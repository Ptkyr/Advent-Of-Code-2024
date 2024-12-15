module Arrays
  ( module Arrays,
    module Data.Array,
  )
where

import Data.Array
import Data.PriorityQueue.FingerTree qualified as PQ
import Utils

type Arr a = Array Int a

type Arr2D a = Array (Int, Int) a

-- Hopefully bounds a1 == bounds a2
zipWithArr2D :: (a -> b -> c) -> Arr2D a -> Arr2D b -> Arr2D c
zipWithArr2D f a1 a2 = listArray (bounds a1) $ zipWith f (elems a1) (elems a2)

-- Construct a 1-indexed array
listArr1 :: [a] -> Arr a
listArr1 arr = listArray (1, length arr) arr

-- Construct a 0-indexed array
listArr0 :: [a] -> Arr a
listArr0 arr = listArray (0, length arr - 1) arr

-- Construct a (1, 1)-indexed 2D array
listArr2D1 :: [[a]] -> Arr2D a
listArr2D1 arr = listArray ((1, 1), (x, y)) $ concat arr
 where
  x = length arr
  y = length $ head arr

-- Construct a (0, 0)-indexed 2D array
listArr2D0 :: [[a]] -> Arr2D a
listArr2D0 arr = listArray ((0, 0), (x - 1, y - 1)) $ concat arr
 where
  x = length arr
  y = length $ head arr

-- WARNING: partial function
indexByValue :: (Ix i, Eq e) => e -> Array i e -> i
indexByValue val =
  fst
    . head
    . dropWhile (\a -> snd a /= val)
    . assocs

ayMax :: Arr2D e -> Int
ayMax = snd . snd . bounds

axMax :: Arr2D e -> Int
axMax = fst . snd . bounds

at :: Arr2D a -> Coord -> Maybe a
at arr c =
  if inRange (bounds arr) c
    then Just (arr ! c)
    else Nothing

midpoint :: Arr a -> a
midpoint arr = (!) arr $ snd (bounds arr) `div` 2
