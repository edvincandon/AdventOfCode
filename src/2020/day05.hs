
import           Data.List                      ( sort )
main = do
  seats <- map parseSeat . lines <$> readFile "./src/2020/data/day05.txt"
  print . maximum $ seats
  print . findSeat $ seats

-- F : lower half
-- B : upper half
parseBinaryPartition :: (Int, Int) -> (Int, Int) -> String -> Int
parseBinaryPartition (start, end) (min, max) xs = fst $ foldl
  (\a b -> if [b] `elem` ["F", "L"]
    then (fst a, fst a + (snd a - fst a) `div` 2)
    else (fst a + (snd a - fst a) `div` 2 + 1, snd a)
  )
  (min, max)
  (take end $ drop start xs)

parseRow :: String -> Int
parseRow = parseBinaryPartition (0, 7) (0, 127)

parseCol :: String -> Int
parseCol = parseBinaryPartition (7, 3) (0, 7)

parseSeat :: String -> Int
parseSeat xs = parseRow xs * 8 + parseCol xs

findSeat :: [Int] -> Int
findSeat xs = sum [head xs' .. xs' !! (length xs' - 1)] - sum xs
  where xs' = sort xs

