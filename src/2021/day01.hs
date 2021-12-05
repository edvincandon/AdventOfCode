import           AOCUtils                       ( readInt )
main :: IO ()
main = do
  depths <- lines <$> readFile "./src/2021/data/day01.txt"
  putStrLn $ "Part 1 = " ++ show (countIncreases . map readInt $ depths)
  putStrLn $ "Part 2 = " ++ show
    (countIncreases . computeWindow . map readInt $ depths)


countPos :: [Int] -> Int
countPos = length . filter (> 0)

countIncreases :: [Int] -> Int
countIncreases xs = countPos (zipWith (-) (tail xs) xs)

computeWindow :: [Int] -> [Int]
computeWindow xs = zipWith (+) (zipWith (+) (tail $ tail xs) (tail xs)) xs
