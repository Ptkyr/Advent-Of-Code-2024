module Parsing (
    module Parsing,
    module Data.Void,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Debug,
    Text,
    readMaybe,
)
where

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Read (readMaybe)

type Parser = Parsec Void Text

eatSome :: Parser ()
eatSome = L.space space1 empty empty

eatMany :: Parser ()
eatMany = L.space space empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme eatSome

symbol :: Text -> Parser Text
symbol = L.symbol eatSome

nat :: Parser Int
nat = lexeme L.decimal

int :: Parser Int
int = L.signed eatSome nat

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
