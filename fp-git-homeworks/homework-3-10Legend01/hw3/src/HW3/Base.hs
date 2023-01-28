module HW3.Base
  ( HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..),
  )
where

data HiFun -- function names (e.g. div, sort, length, ...)
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub

instance Show HiFun where
  show HiFunDiv = "div"
  show HiFunMul = "mul"
  show HiFunAdd = "add"
  show HiFunSub = "sub"

data HiValue -- values (numbers, booleans, strings, ...)
  = HiValueNumber Rational
  | HiValueFunction HiFun
  deriving Show

data HiExpr -- expressions (literals, function calls, ...)
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError -- evaluation errors (invalid arguments, ...)
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show
