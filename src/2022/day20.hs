import           AOCUtils   (readInt, toIndexedList)
import           Data.List  (elemIndex, find)
import           Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- lines <$> readFile "./src/2022/data/day20.txt"
  let encrypted = readInt <$> input
  putStrLn "---Part 1 -------------"
  print $ solve encrypted 1 1
  putStrLn "---Part 2 -------------"
  print $ solve encrypted 811589153 10

-- 🐢 viva immutability 🐢
mix :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mix from to = foldl mix' to from
  where
    mix' :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    mix' mixed value@(delta, _) =
      if delta == 0
        then mixed
        else mixed'
      where
        idx = fromJust $ elemIndex value mixed
        buff = take idx mixed ++ drop (idx + 1) mixed
        idx' = (idx + delta) `mod` length buff
        mixed'
          | idx' == 0 = buff ++ [value]
          | otherwise = take idx' buff ++ [value] ++ drop idx' buff

solve :: [Int] -> Int -> Int -> Int
solve xs key n = sum res
  where
    l = length mixed
    indexed = toIndexedList $ (* key) <$> xs
    mixed = fst <$> (iterate (mix indexed) indexed !! n)
    zero = fromJust $ elemIndex 0 mixed
    res = (\i -> mixed !! ((zero + i) `mod` l)) <$> [1000, 2000, 3000]
