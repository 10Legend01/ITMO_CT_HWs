{-# LANGUAGE LambdaCase #-}

module HW3.Pretty
  ( prettyValue,
  )
where

import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), floatingOrInteger, formatScientific, fromFloatDigits, fromRationalRepetendUnlimited)
import HW3.Base (HiValue (..))
import Prettyprinter (Doc, pretty, slash)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  (HiValueFunction f) -> pretty $ show f
  (HiValueNumber n) ->
    case mbR of
      Nothing -> prettyFiniteNumber $ floatingOrInteger s
      Just _ -> prettyInfiniteNumber n
    where
      (s, mbR) = fromRationalRepetendUnlimited n

prettyFiniteNumber :: Either Double Integer -> Doc AnsiStyle
prettyFiniteNumber = \case
  (Left d) -> pretty $ formatScientific Fixed Nothing (fromFloatDigits d)
  (Right i) -> pretty i

prettyInfiniteNumber :: Rational -> Doc AnsiStyle
prettyInfiniteNumber s
  | z == 0
    =
    pretty (if qNeg then "-" else "")
      <> prettyRPos
  | otherwise
    =
    pretty z
      <> pretty (if qNeg then " - " else " + ")
      <> prettyRPos
  where
    den = denominator s
    (z, q) = quotRem (numerator s) den

    qNeg = q < 0
    prettyRPos = pretty (if qNeg then - q else q) <> slash <> pretty den
