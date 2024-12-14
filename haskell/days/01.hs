{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "01")

partOne :: ([Int], [Int]) -> Int
partOne (a, b) = sum $ zipWith (abs .: (-)) as bs
 where
  as = sort a
  bs = sort b

partTwo :: ([Int], [Int]) -> Int
partTwo (a, b) = sum $ zipWith (*) a $ (map (\x -> countIf ((==) x) b)) a

aocParse :: Parser ([Int], [Int])
aocParse = fromPairs <$> some ((,) <$> nat <*> nat) <* eof
