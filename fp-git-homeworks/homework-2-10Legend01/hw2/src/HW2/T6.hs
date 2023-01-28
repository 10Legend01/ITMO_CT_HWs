{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..),
    Parser (..),
    runP,
    pChar,
    parseError,
    pEof,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T5

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = case runES es (0, str) of
  Error e -> Error e
  Success (a :# _) -> Success a

-- 1) Если s оказалось пустым, то выбрасывает Error с указанием позиции
-- 2) Иначе возвращается Success со новой строкой без головного символа и прибавлением единицы к позиции
pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (P p) <|> (P q) = P $
    ES $ \s -> case runES p s of
      Error _ -> runES q s
      success -> success

instance MonadPlus Parser -- No methods.

pEof :: Parser ()
pEof = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Success (() :# (pos, []))
      _ -> Error (ErrorAtPos pos)

--parseExpr :: String -> Except ParseError Expr
