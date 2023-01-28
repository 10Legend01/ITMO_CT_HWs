module HW0.T4
  ( repeat',
    map',
    fib,
    fac,
  )
where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' =
  fix
    ( \rec f ma -> case ma of
        [] -> []
        first : rest -> f first : rec f rest
    )

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib = fix rec 1 0
  where
    rec f past current n = if n == 0 then current else f current (past + current) (n - 1)

fac :: Natural -> Natural -- computes the factorial
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n -1))
