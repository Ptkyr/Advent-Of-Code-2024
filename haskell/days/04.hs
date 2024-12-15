{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Mayn
import Parsing
import Utils

$(generateMain "04")

aocParse :: Parser (Arr2D Char)
aocParse = listArr2D1 <$> some upperChar `endBy` newline <* eof

partOne :: Arr2D Char -> Int
partOne arr =
  sum
    . map
      ( \(i, e) ->
          if e /= 'X'
            then 0
            else countIf xmas $ lineSearches i
      )
    $ assocs arr
 where
  xmas :: Coords -> Bool
  xmas = (== map Just "XMAS") . map (at arr)
  lineSearches :: Coord -> [Coords]
  lineSearches c = map (fillLine c) $ fanEnds c
  fanEnds :: Coord -> Coords
  fanEnds c =
    map
      (liftT2 (+) c)
      [(-3, -3), (-3, 0), (-3, 3), (0, -3), (0, 3), (3, -3), (3, 0), (3, 3)]

partTwo :: Arr2D Char -> Int
partTwo arr =
  length
    . filter (\(i, e) -> e == 'A' && xmas i)
    $ assocs arr
 where
  xmas :: Coord -> Bool
  xmas (x, y) =
    on (&&) sammas (fillLine (x - 1, y - 1) (x + 1, y + 1)) $
      fillLine (x - 1, y + 1) (x + 1, y - 1)
   where
    sammas = cOr (== map Just "SAM") (== map Just "MAS") . map (at arr)
