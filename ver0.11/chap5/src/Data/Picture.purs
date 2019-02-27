module Picture where

import Prelude

import Data.Array (cons)
import Data.Array.Partial (tail)
import Data.Foldable (sum, foldl)
import Data.Maybe (Maybe(..))
import Global as Global
import Math as Math
import Partial.Unsafe (unsafePartial)

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

-- 正解
-- どうやら3C2とかを計算できれば良さそう。パスカルの三角形を辿って行くイメージ。
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k | k > n = 0
binomialCoefficient n 0 = 1
binomialCoefficient n k = binomialCoefficient (n-1) (k-1) + binomialCoefficient (n-1) k

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

-- 演習5.9-1
type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

tom :: Person
tom = { name : "Tom", address : { street : "16", city : "LA" } }
john :: Person
john = { name : "John", address : { street : "18", city : "LA" } }
hunk :: Person
hunk = { name : "Hunk", address : { street : "16", city : "NY" } }
curl :: Person
curl = { name : "Curl", address : { street : "16", city : "NY" } }


sameCity :: Person -> Person -> Boolean
sameCity p1 p2 
  | p1.address.city == p2.address.city = true
  | otherwise = false

-- 演習5.9-2
sameCity' {address :{city:x}} {address :{city:y}}
  | x == y = true
  | otherwise = false

-- 演習5.9-3
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _ = x

-- 5.10
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
  0 -> xs
  _ -> lzs (unsafePartial tail xs)

-- 5.12
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number}

-- 5.13
exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point {x: 0.0, y: 0.0}

    p2 :: Point
    p2 = Point {x: 100.0, y: 50.0}

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

showPoint :: Point -> String
showPoint (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r) = "O = " <> showPoint c <> ", r = " <> show r
showShape (Rectangle c w h) = "O = " <> showPoint c <> ", width = " <> show w <> ", height = " <> show h
showShape (Line start end) = "start = " <> showPoint start <> ", end = " <> showPoint end
showShape (Text p text) = "O = " <> showPoint p <> ", text = " <> text

-- 演習5.14-1
exampleCircle :: Shape
exampleCircle = Circle p r
  where
    p :: Point
    p = origin

    r :: Number
    r = 10.0

-- 演習5.14-2
doubleShape :: Shape -> Shape
doubleShape (Circle c r) = (Circle origin (r * 2.0))
doubleShape (Rectangle c w h) = (Rectangle origin (w * 2.0) (h * 2.0))
doubleShape (Line (Point start) (Point end)) = (Line newStart newEnd)
  where
    xdiff = start.x - end.x
    ydiff = start.y - end.y
    newStart :: Point
    newStart = Point {x: -xdiff, y: -ydiff}
    newEnd :: Point
    newEnd = Point {x: xdiff, y:ydiff}
doubleShape (Text c text) = (Text origin text)

-- 演習5.14-3
findText :: Shape -> Maybe String
findText (Text _ text) = Just text
findText _ = Nothing

-- 5.16
type Picture = Array Shape
showPicture :: Picture -> Array String
showPicture pict = map showShape pict

-- 5.17
data Bounds = Bounds
  { top :: Number
  , left :: Number
  , bottom :: Number
  , right :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x : x, y : y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x : x, y : y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x : x, y : y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersection :: Bounds -> Bounds -> Bounds
intersection (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

-- 演習5.17-1
area :: Shape -> Number
area (Circle c r) = Math.pi * r * r
area (Rectangle c w h) = w * h
area (Line (Point start) (Point end)) = Math.sqrt (xdiff * xdiff + ydiff * ydiff)
  where
    xdiff = start.x - end.x
    ydiff = start.y - end.y 
area (Text c text) = 0.0