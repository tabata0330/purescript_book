module Main where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

length :: forall a. Array a -> Int
length arr = 
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

-- 演習4.4-1
even :: Int -> Boolean
even 2 = true
even 1 = false
even digit = even (digit - 2)

-- 演習4.4-2
numContainEven :: Array Int -> Int
numContainEven arr =
  if null arr
    then 0
    else if even (unsafePartial head arr)
      then 1 + numContainEven (unsafePartial tail arr)
      else numContainEven (unsafePartial tail arr)

