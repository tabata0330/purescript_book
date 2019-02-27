module Main where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, filter, null, (..), concatMap, cons)
import Data.Array.Partial (head, tail)
import Data.Foldable (product, foldl)
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
contaionEvenNum :: Array Int -> Int
contaionEvenNum arr =
  if null arr
    then 0
    else if even (unsafePartial head arr)
      then 1 + contaionEvenNum (unsafePartial tail arr)
      else contaionEvenNum (unsafePartial tail arr)

-- 演習4.7-1
sqrtArray :: Array Int -> Array Int
sqrtArray arr = map (\n -> n * n) arr

-- 演習4.7-2
-- removeNegativeNum :: Array Int -> Array Int
-- removeNegativeNum arr = filter (\n -> n >= 0) arr

-- 演習4.7-3
infixl 4 filter as <$?>
removeNegativeNum :: Array Int -> Array Int
removeNegativeNum arr = (\n -> n >= 0) <$?> arr

-- 与えられた配列の重複なし組み合わせ作成
combination :: Int -> Array (Array Int)
combination n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

-- 因数分解してみる
factors :: Int -> (Array (Array Int))
factors n = filter (\pair -> product pair == n) (combination n)

-- do記法を使った因数分解の別実装
factorsDo :: Int -> (Array (Array Int))
factorsDo n = filter (\pair -> product pair == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

-- さらにguard関数を用いた因数分解の別実装
factorsDoGuard :: Int -> (Array (Array Int))
factorsDoGuard n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- 演習4.11-1
isPrime :: Int -> Boolean
isPrime n =
  if n == 1
    then false
    else if length (factors n) == 1
      then true
      else false

-- 演習4.11-2 
findDirectProductSet :: forall a. Array a -> Array a -> (Array (Array a))
findDirectProductSet first second = concatMap (\i -> map (\j -> [i, j]) second) first

-- 演習4.11-3
findPythagorasSet :: Int -> (Array (Array Int))
findPythagorasSet n = do
  a <- 1 .. (n - 1)
  b <- a .. (n - 1)
  c <- 1 .. (n - 1)
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- 演習4.11-4
-- わからんから後回し
containsOne :: Array Int -> Boolean
containsOne arr = 
  if (length  (filter (\i -> i == 1) arr)) == 0
    then false
    else true

getLast :: forall a. Array a -> a
getLast arr =
  if (length arr) == 1
    then unsafePartial head arr
    else getLast (unsafePartial tail arr)

-- factorization :: Int -> (Array (Array Int))
-- factorization 2 = [[1, 2]]
-- factorization n = do
--   m <- factors n
--   guard $ containsOne m
--   guard $ ((isPrime (unsafePartial head m)) && (isPrime (getLast m)))
--   do
--     o <- factorization (getLast m)
--     guard $ ((length (filter (\i -> (i < unsafePartial head m)) o)) > 0)
--     cons (unsafePartial head m) o

-- 一応これが正解っぽい…。納得いかないけど。
factorizations :: Int -> Array Int
factorizations num = concat $ factors num

-- reverse関数
-- reverse :: forall a. Array a -> Array a
-- reverse [] = []
-- reverse xs = snoc (reverse (unsafePartial tail xs)) (unsafePartial head xs)

-- 累積器を導入して末尾再帰にしたreverse関数
reverse :: forall a. Array a -> Array a
reverse arg = reverse' [] arg
  where
    reverse' acc [] = acc
    reverse' acc xs = reverse' (cons (unsafePartial head xs) acc) (unsafePartial tail xs)

-- 演習4.15-1
containsOnlyTrue :: Array Boolean -> Boolean
containsOnlyTrue arr = foldl (\i j -> i && j) true arr

-- 演習4.15-2
-- falseが奇数個ある配列?

-- 演習4.15-3
count :: forall a. (a -> Boolean) -> Array a -> Int
count func arr = 
  if func (unsafePartial head arr)
    then count' func (unsafePartial tail arr) 1
    else count' func (unsafePartial tail arr) 0
  where
    count' _ [] num = num
    count' func arr num = count func arr + num
-- 間違ってる

-- 演習4.15-4
reverse_fr :: forall a. Array a -> Array a
reverse_fr arr = foldl (\acc x -> cons x acc) [] arr
