module AOCUtils where
import           Data.List.Split                ( splitOn )
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
