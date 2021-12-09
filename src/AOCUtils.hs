module AOCUtils where
import           Data.List.Split                ( splitOn )
import           Data.List                      ( sortOn
                                                , groupBy
                                                , sort
                                                , group
                                                , genericLength
                                                )
import           Data.Maybe                     ( fromMaybe )
-- Helpers --
-- DATA PARSING --
readInt :: String -> Int
readInt = read

parseNums :: String -> [Int]
parseNums = fmap readInt . splitOn ","

-- STATS --
mean :: (Real a, Fractional b) => [a] -> b
mean [] = error "mean imposible"
mean xs = realToFrac (sum xs) / genericLength xs

median :: (Real a, Fractional b) => [a] -> b
median [] = error "median impossible"
median xs
  | length xs == 1 = realToFrac $ head xs
  | odd split = realToFrac $ (!!) sorted $ (split - 1) `div` 2
  | otherwise = realToFrac
  $ mean [(!!) sorted $ split `div` 2 - 1, (!!) sorted $ split `div` 2]
 where
  sorted = sort xs
  split  = genericLength sorted


-- LISTS --
toIndexedList :: [a] -> [(a, Int)]
toIndexedList = flip zip [0 ..]

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i | (i > -1) && (length xs > i) = Just (xs !! i)
               | otherwise                   = Nothing

safeIndex1 :: [a] -> Int -> a -> a
safeIndex1 xs i d = fromMaybe d (safeIndex xs i)

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs | n > 0     = take n xs : groupByN n (drop n xs)
              | otherwise = error "invalid group arguments"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs) | n == 0    = newVal : xs
                             | otherwise = x : replaceNth (n - 1) newVal xs


-- BINARY -- 
bin2num :: [Int] -> Int
bin2num list = parse (reverse list) 0
 where
  parse []       _ = 0
  parse (x : xs) n = x * (2 ^ n) + parse xs (n + 1)

-- TUPLES --
uniqueTuples :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
uniqueTuples = minUniqueTuples 1

minUniqueTuples :: (Ord a, Ord b) => Int -> [(a, b)] -> [(a, b)]
minUniqueTuples minRep =
  fmap head
    <$> filter ((>= minRep) . length)
    .   concatMap (groupBy (\a b -> snd a == snd b) . sortOn snd)
    .   groupBy (\a b -> fst a == fst b)
    .   sortOn fst

