{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator
  ( eval,
  )
where

import HW3.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))

eval :: HiExpr -> Either HiError HiValue
eval = \case
  (HiExprValue a) -> Right a
  (HiExprApply a b) ->
    case eval a of
      (Right (HiValueFunction f)) -> evalFunc f (map eval b)
      _ -> Left HiErrorInvalidFunction

evalFunc :: HiFun -> [Either HiError HiValue] -> Either HiError HiValue
evalFunc HiFunAdd [Right (HiValueNumber a), Right (HiValueNumber b)] = Right $ HiValueNumber (a + b)
evalFunc HiFunAdd [_, _] = Left HiErrorInvalidArgument
evalFunc HiFunSub [Right (HiValueNumber a), Right (HiValueNumber b)] = Right $ HiValueNumber (a - b)
evalFunc HiFunSub [_, _] = Left HiErrorInvalidArgument
evalFunc HiFunMul [Right (HiValueNumber a), Right (HiValueNumber b)] = Right $ HiValueNumber (a * b)
evalFunc HiFunMul [_, _] = Left HiErrorInvalidArgument
evalFunc HiFunDiv [Right (HiValueNumber a), Right (HiValueNumber b)] =
  if b == 0
    then Left HiErrorDivideByZero
    else Right $ HiValueNumber (a / b)
evalFunc HiFunDiv [_, _] = Left HiErrorInvalidArgument
evalFunc _ _ = Left HiErrorArityMismatch
