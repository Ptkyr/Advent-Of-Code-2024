module Parsing
  ( module Parsing,
    module Data.Void,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Debug,
    Text,
    readMaybe,
    void,
  )
where

import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)
import Utils

type Parser = Parsec Void Text

eatSome :: Parser ()
eatSome = L.space space1 empty empty

sipSome :: Parser ()
sipSome = L.space hspace1 empty empty

eatMany :: Parser ()
eatMany = L.space space empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme eatSome

hlexeme :: Parser a -> Parser a
hlexeme = L.lexeme sipSome

symbol :: Text -> Parser Text
symbol = L.symbol eatMany

nat :: Parser Int
nat = lexeme L.decimal

num :: Parser Int
num = L.decimal

hnat :: Parser Int
hnat = hlexeme L.decimal

int :: Parser Int
int = L.signed eatSome nat

digit :: Parser Int
digit = digitToInt <$> lexeme digitChar

regexdot :: Parser Char
regexdot = anySingleBut '\n'

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

word :: Parser String
word = some letterChar

lexword :: Parser String
lexword = lexeme $ some letterChar

parseInput :: Parser a -> String -> IO (Either (ParseErrorBundle Text Void) a)
parseInput parser file = runParser parser file . pack <$> readFile file

sourceToCoord :: SourcePos -> (Int, Int)
sourceToCoord pos = liftT1 unPos (sourceLine pos, sourceColumn pos)
