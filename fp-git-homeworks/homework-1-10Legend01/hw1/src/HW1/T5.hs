module HW1.T5
  ( splitOn,
    joinWith,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn char inp
  | null inp = [] :| []
  | otherwise = split ([] :| []) (reverse inp)
  where
    split (curr :| mind) ma = case ma of
      [] -> curr :| mind
      first : rest
        | first == char -> split ([] :| (curr : mind)) rest
        | otherwise -> split ((first : curr) :| mind) rest

joinWith :: a -> NonEmpty [a] -> [a]
joinWith char (first :| rest) = first ++ concatMap add rest
  where
    add curr = char : curr
