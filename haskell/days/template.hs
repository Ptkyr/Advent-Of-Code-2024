{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "")

type Input = 

aocParse :: Parser Input
aocParse = fromPairs <$> some ((,) <$> nat <*> nat) <* eof

partOne :: Input -> Int
partOne a = 56

partTwo :: Input -> Int
partTwo a = 56
