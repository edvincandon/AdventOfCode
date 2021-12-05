module AOCUtils where
import           Data.List.Split                ( splitOn )
import           Data.List                      ( sortOn
                                                , groupBy
                                                , sort
                                                , group
                                                )
-- Helpers --
readInt :: String -> Int
readInt = read

parseNums :: String -> [Int]
parseNums = fmap readInt . splitOn ","

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs | n > 0     = take n xs : groupByN n (drop n xs)
              | otherwise = error "invalid group arguments"

bin2num :: [Int] -> Int
bin2num list = parse (reverse list) 0
 where
  parse []       _ = 0
  parse (x : xs) n = x * (2 ^ n) + parse xs (n + 1)

uniqueTuples :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
uniqueTuples = minUniqueTuples 1

minUniqueTuples :: (Ord a, Ord b) => Int -> [(a, b)] -> [(a, b)]
minUniqueTuples minRep =
  fmap head
    <$> filter ((>= minRep) . length)
    .   concatMap (groupBy (\a b -> snd a == snd b) . sortOn snd)
    .   groupBy (\a b -> fst a == fst b)
    .   sortOn fst
