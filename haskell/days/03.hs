{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "03")

data Instruction = Do | Dont | Mul (Int, Int)
type Input = [Instruction]

aocParse :: Parser Input
aocParse = catMaybes <$> some maybeInst <* eof
 where
  maybeInst =
    try (Just . Mul <$> oneMul)
      <|> Just Do <$ lexeme "do()"
      <|> Just Dont <$ lexeme "don't()"
      <|> Nothing <$ asciiChar
  oneMul :: Parser Coord
  oneMul =
    (,) <$> (lexeme "mul(" *> nat) <*> (lexeme "," *> nat <* lexeme ")")

partOne :: Input -> Int
partOne = sum . map mapper
 where
  mapper (Mul (x, y)) = x * y
  mapper _ = 0

partTwo :: Input -> Int
partTwo = snd . foldl' mapper (1, 0)
 where
  mapper (_, acc) Do = (1, acc)
  mapper (_, acc) Dont = (0, acc)
  mapper (mult, acc) (Mul (x, y)) = (mult, mult * x * y + acc)
