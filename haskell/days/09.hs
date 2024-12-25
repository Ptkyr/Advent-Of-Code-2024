{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence qualified as S
import Mayn
import Parsing
import Utils

$(generateMain "09")

data File = File
  { uuid :: Int,
    copies :: Int
  }
  deriving (Show)
type Files = S.Seq File
type Spaces = S.Seq Int
type Input = (Files, Spaces)

repl :: File -> [Int]
repl (File u c) = replicate c u

decr :: File -> File
decr (File u c) = File u $ c - 1

toFile :: (Int, Int) -> File
toFile (a, b) = File a b

aocParse :: Parser Input
aocParse = do
  (files, spaces) <- unalternate <$> some digit <* eof
  pure (S.fromList . map toFile . assocs $ listArr0 files, S.fromList spaces)

partOne :: Input -> Int
partOne (files, spaces) = sum $ zipWith (*) [0 ..] decompressed
 where
  decompressed = oneStep Fwd files spaces 0

partTwo :: Input -> Int
partTwo a = 56

data Selector = Fwd | Bck
oneStep :: Selector -> Files -> Spaces -> Int -> [Int]
oneStep Fwd (f :<| _) Empty _ = repl f
oneStep _ _ Empty _ = []
oneStep Fwd (f :<| fs) spaces idx =
  repl f ++ oneStep Bck fs spaces (idx + copies f)
oneStep Bck files@(rs :|> r) (s :<| ss) idx
  | s == 0 = oneStep Fwd files ss idx
  | copies r == 0 = oneStep Bck rs (s <| S.take (S.length ss - 1) ss) idx
  | otherwise = uuid r : oneStep Bck (rs |> decr r) (s - 1 <| ss) (idx + 1)
oneStep _ _ _ _ = error "Unreachable"
