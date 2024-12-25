{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Data.HashSet qualified as HS
import Mayn
import Parsing
import Utils

$(generateMain "05")

type Update = Arr Int
type Updates = [Update]
type Rule = (Int, Int)
type Rules = HS.HashSet Rule

aocParse :: Parser (Rules, Updates)
aocParse = do
  rules <- ((,) <$> nat <* lexeme "|" <*> hnat) `endBy` newline
  void newline
  updates <- (listArr0 <$> hnat `sepBy1` ",") `endBy` newline <* eof
  pure (HS.fromList rules, updates)

partOne :: (Rules, Updates) -> Int
partOne (rules, updates) = sum . map midpoint $ filter (ordered rules) updates

partTwo :: (Rules, Updates) -> Int
partTwo (rules, updates) =
  sum . map (midpoint . listArr0 . map toInt . sort . map (Innit rules) . elems) $
    remove (ordered rules) updates

data Innit = Innit Rules Int

instance Eq Innit where
  (Innit _ x) == (Innit _ y) = x == y

instance Ord Innit where
  compare (Innit r x) (Innit _ y)
    | x == y = EQ
    | HS.member (x, y) r = LT
    | otherwise = GT

toInt :: Innit -> Int
toInt (Innit _ x) = x

ordered :: Rules -> Update -> Bool
ordered rules update = para scanRules True $ elems update
 where
  scanRules :: Int -> [Int] -> Bool -> Bool
  scanRules pg = (&&) . all (goodPage pg)
  goodPage :: Int -> Int -> Bool
  goodPage a b = not $ HS.member (b, a) rules
