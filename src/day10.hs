import           Data.List                      ( sort )

main :: IO ()
main = do
  contents <- sort . map readInt . lines <$> readFile "./data/day10.txt"
  let deltas = findDeltas contents
  let d1     = lengthWithPredicate (== 1) deltas
  let d3     = lengthWithPredicate (== 3) deltas
  print $ d1 * d3

readInt :: String -> Int
readInt = read

findDeltas :: [Int] -> [Int]
findDeltas xs = zipWith (-) x (0 : x) where x = xs ++ [last xs + 3]

lengthWithPredicate :: (a -> Bool) -> [a] -> Int
lengthWithPredicate p = length . filter p
