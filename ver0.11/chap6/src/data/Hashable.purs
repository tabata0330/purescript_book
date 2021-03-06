module Data.Hashable where

import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple
import Math
import Prelude

import Data.Array (cons, nubBy)
import Data.Array.Partial (head, tail)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Monoid (class Monoid, mempty)
import Data.String (toCharArray)
import Partial.Unsafe (unsafePartial)
import Stream (class Stream, uncons)

-- 6.3
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number}

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point {x: 0.0, y: 0.0}

    p2 :: Point
    p2 = Point {x: 100.0, y: 50.0}


exampleCircle :: Shape
exampleCircle = Circle p r
  where
    p :: Point
    p = origin

    r :: Number
    r = 10.0

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

-- 演習6.3-1
instance showPoint :: Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r) = "O = " <> show c <> ", r = " <> show r
  show (Rectangle c w h) = "O = " <> show c <> ", width = " <> show w <> ", height = " <> show h
  show (Line start end) = "start = " <> show start <> ", end = " <> show end
  show (Text p text) = "O = " <> show p <> ", text = " <> text

-- Eq型クラスインスタンス作ってみた
instance eqPoint :: Eq Point where
  eq (Point {x: x_1, y: y_1}) (Point {x: x_2, y: y_2})
    | (x_1 == x_2) && (y_1 == y_2) = true
    | otherwise = false

instance eqShape :: Eq Shape where
  eq (Circle c1 r1) (Circle c2 r2)
    | (c1 == c2) && (r1 == r2) = true
    | otherwise = false
  eq (Rectangle c1 w1 h1) (Rectangle c2 w2 h2)
    | (c1 == c2) && (w1 == w2) && (h1 == h2) = true
    | otherwise = false
  eq (Line start1 end1) (Line start2 end2)
    | (start1 == start2) && (end1 == end2) = true
    | otherwise = false
  eq (Text p1 text1) (Text p2 text2) 
    | (p1 == p2) && (text1 == text2) = true
    | otherwise = false
  eq _ _ = false

-- Ord型クラスインスタンス作ってみた
-- 座標の大小定義はy軸のみを考える
instance ordPoint :: Ord Point where
  compare (Point {x: x_1, y: y_1}) (Point {x: x_2, y: y_2})
    | y_1 > y_2 = GT
    | y_1 < y_2 = LT
    | otherwise = EQ

-- 図形の大小はサイズで決める
area :: Shape -> Number
area (Circle c r) = pi * r * r
area (Rectangle c w h) = w * h
area (Line (Point start) (Point end)) = sqrt (xdiff * xdiff + ydiff * ydiff)
  where
    xdiff = start.x - end.x
    ydiff = start.y - end.y 
area (Text c text) = 0.0

instance ordArea :: Ord Shape where
  compare s1 s2
    | (area s1) > (area s2) = GT
    | (area s1) < (area s2) = LT
    | otherwise = EQ

-- 演習6.4-1
newtype Complex = Complex 
    { real :: Number
    , imaginary :: Number 
    }

instance showComplex :: Show Complex where
  show (Complex {real: r, imaginary: i}) =  show r <> "+" <> show i <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2})
    | (r1 == r2) && (i1 == i2) = true
    | otherwise = false

-- 6.5
threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3

showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2 | a1 < a2 =
  show a1 <> " is less than " <> show a2
showCompare a1 a2 | a1 > a2 =
  show a1 <> " is greater than " <> show a2
showCompare a1 a2 =
  show a1 <> " is equal to " <> show a2

-- 演習6.7-1
data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a1 arr1) = show a1 <> " " <> show arr1

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a1 arr1) (NonEmpty a2 arr2)
    | (a1 == a2) && (arr1 == arr2) = true
    | otherwise = false

-- 演習6.7-2
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a1 arr1) (NonEmpty a2 arr2) = (NonEmpty a1 (append arr1 arr2))

-- 演習6.7-3
instance functorNonEmpty :: Functor NonEmpty where
  map func (NonEmpty a1 arr1) = NonEmpty (func a1) (map func arr1)

-- 演習6.7-4
-- 正直題意がよくわからない
data Extended a = Finite a | Infinite

instance eqExtendedA :: Eq a => Eq (Extended a) where
  eq a1 a2 = a1 == a2

instance ordExtendedA :: Ord a => Ord (Extended a) where
  compare a1 a2
    | a1 == a2 = EQ
    | a1 > a2 = GT
    | otherwise = LT

-- 演習6.7-5
instance foldableNonEmpty :: Foldable NonEmpty where
  foldl func m (NonEmpty a arr) = foldl func m (cons a arr)
  foldr func m (NonEmpty a arr) = foldr func m (cons a arr)
  foldMap func (NonEmpty a arr) = foldMap func (cons a arr)

-- 演習6.7-6
-- 正直題意がよくわからない
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore _ b) = foldr f z b
  foldl f z (OneMore _ b) = foldl f z b
  foldMap f (OneMore _ b) = foldMap f b

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail

secondElement :: forall a. Partial => Array a -> a
secondElement xs = head (tail xs)

-- 演習6.11-1
maxValue :: Partial => Array Int -> Int
maxValue arr = 
  case maximum arr of
    Nothing -> 0
    Just int -> int

-- 演習6.11-2
newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance showMultiply :: Show Multiply where
  show (Multiply i) = show i

class Monoid m <= Action m a where
  act :: m -> a -> a

instance repeatAction :: Action Multiply String where
  act (Multiply i) str = fuga (Multiply i) str "" where
    fuga (Multiply i) str result
      | i == 0 = result
      | otherwise = fuga (Multiply (i - 1)) str (result <> str)

-- 演習6.11-3
instance actionArray :: Action m a => Action m (Array a) where
  act m arr = map (\a -> act m a) arr

-- 演習6.11-4
-- 題意が謎
newtype Self m = Self m

instance semigroupSelf :: Semigroup n => Semigroup (Self n) where
  append (Self n) (Self m) = Self (n <> m) 

instance actionSelf :: Monoid m => Action m (Self m) where
  act m a = a <> a

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

instance showHashCode :: Show HashCode where
  show (HashCode i) = show i

class Eq a <= Hashable a where
  hash :: a -> HashCode

instance eqHashCode :: Eq HashCode where
  eq (HashCode i) (HashCode j) = i == j

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash i = hashCode i

instance hashBoolean :: Hashable Boolean where
  hash true = hashCode 1
  hash false = hashCode 0

instance hashChar :: Hashable Char where
  hash char = hash $ (toCharCode char)

instance hashArray :: Hashable a => Hashable (Array a) where
  hash arr = foldl combineHashes (hashCode 0) (map hash arr)

instance hashString :: Hashable String where
  hash arr = hash $ (toCharArray arr)

-- 演習6.12-2
nubByHash :: forall a. Hashable a => Array a -> Array a
nubByHash arr = nubBy hashEqual arr

-- 演習6.12-3
newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hash (mod n 12) 

instance hashMaybe :: (Hashable a) => Hashable (Maybe a) where
  hash Nothing = (hash 0)
  hash (Just a) = combineHashes (hash 1) (hash a)

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = combineHashes (hash a) (hash b)

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = combineHashes (hash 0) (hash a)
  hash (Right b) = combineHashes (hash 1) (hash b)
  