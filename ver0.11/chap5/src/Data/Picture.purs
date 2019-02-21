module Picture where

import Prelude

import Data.Array (cons)
import Global as Global
import Math as Math

gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m = gcd' (n - m) m
        |otherwise = gcd' n (m - n)

-- 演習5.5-1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n - 1))

-- 末尾再帰のfactorial
factorial_tail :: Int -> Int
factorial_tail n = factorial_tail' n 1
  where
    factorial_tail' 0 m = m
    factorial_tail' n m = factorial_tail' (n - 1) (m * n)

-- 演習5.5-2
-- nを引数として渡した時、二項係数をリストで返す関数
-- パスカルの公式とは…
bicoefficient :: Int -> Array Int
bicoefficient n = bicoefficient' n n
  where
    bicoefficient' _ 0 = [1]
    bicoefficient' n m = cons ((factorial_tail n) / ((factorial_tail m) * (factorial_tail (n - m)))) (bicoefficient' n (m - 1))

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

