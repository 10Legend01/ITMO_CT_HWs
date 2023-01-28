module HW3.Parser
  ( parse,
  )
where

import Control.Applicative (many, optional, (<|>))
import Data.Void (Void)
import HW3.Base (HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, empty, eof, runParser)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer (scientific, signed)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpr <* eof) ""

sc :: Parser ()
sc =
  L.space
    space1
    empty
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExpr :: Parser HiExpr
parseExpr = lexeme $ do
  c <- parseHiValue
  parseApply c

parseHiValue :: Parser HiExpr
parseHiValue = lexeme (parseValue <|> parens parseExpr)

parseApply :: HiExpr -> Parser HiExpr
parseApply expr = do
  brackets <- optional $ parens parseArguments
  case brackets of
    Just args -> do
      parseApply (HiExprApply expr args)
    Nothing -> return expr

parseArguments :: Parser [HiExpr]
parseArguments = do
  x <- parseExpr
  xs <- many $ lexeme (char ',') *> parseExpr
  return (x : xs)

parseValue :: Parser HiExpr
parseValue = lexeme $ HiExprValue <$> (parseNumber <|> parseFunction)

parseNumber :: Parser HiValue
parseNumber = lexeme $ do
  res <- toRational <$> signed sc scientific
  return (HiValueNumber res)

parseFunction :: Parser HiValue
parseFunction =
  lexeme $
    HiValueFunction
      <$> choice
        [ pStr "mul" HiFunMul,
          pStr "div" HiFunDiv,
          pStr "add" HiFunAdd,
          pStr "sub" HiFunSub
        ]
  where
    pStr s r = string s >> return r
