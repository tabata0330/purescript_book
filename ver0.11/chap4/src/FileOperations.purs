module FileOperations where

import Prelude

import Data.Array ((:), concatMap, filter)
import Data.Array.Partial (head)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Control.MonadZero (guard)
import Data.Path (Path, filename, isDirectory, isFile, ls, root, size)
import Main (length)
import Math (max)
import Partial.Unsafe (unsafePartial)


allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  f <- ls file
  allFiles' f

-- 演習4.17-1
onlyFile :: Path -> Array Path
onlyFile file = filter (\i -> isFile i) (allFiles file)

-- 演習4.17-2
getBiggestFileSize :: Path -> Number
getBiggestFileSize file = foldl (\acc xs -> max acc xs) 0.0 ((map (\i -> toNumber (maybe 0 (\x -> x) (size i)))) (onlyFile file))

getBiggestFile :: Path -> Array Path
getBiggestFile file = filter (\x -> toNumber (maybe 0 (\x -> x) (size x)) == getBiggestFileSize file) (onlyFile file)

-- 演習4.17-3 要求通りではない
whereIs' :: String -> Maybe Path
whereIs' name = 
  if (length (filter (\x -> (filename x) == name) (onlyFile root)) == 1)
    then Just (unsafePartial head (filter (\x -> (filename x) == name) (onlyFile root)))
    else Nothing

-- whereIs :: String -> Maybe Path
-- whereIs name = do
--   child <- (allFiles root)
--   guard $ isDirectory child
--   if (length (filter (\x -> (filename x) == name) (ls child)) == 1)
--     then pure [Just child]
--     else pure [Nothing]