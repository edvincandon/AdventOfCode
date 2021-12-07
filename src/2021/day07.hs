import           AOCUtils                       ( readInt
                                                , parseNums
                                                , median
                                                )
import           Data.List.Split                ( splitOn )


main :: IO ()
main = do
  pos <- map readInt . splitOn "," <$> readFile "./src/2021/data/day07.txt"
  putStrLn "---Part 1 -------------"
  print $ computeNaiveTotalFuel (mostEfficientPos pos) pos
  putStrLn "---Part 2 -------------"
  print $ computeTotalFuel
    (mostEfficientPosRec pos (minimum pos, maximum pos))
    pos

-- Helpers --
computeNaiveTotalFuel :: Int -> [Int] -> Int
computeNaiveTotalFuel n = sum . map (abs . (-) n)

computeTotalFuel :: Int -> [Int] -> Int
computeTotalFuel n = sum . map (\x -> sum [0 .. abs $ n - x])

-- Part 1 --
mostEfficientPos :: [Int] -> Int
mostEfficientPos = round . median

-- Part 2 --
-- binary search --
mostEfficientPosRec :: [Int] -> (Int, Int) -> Int
mostEfficientPosRec xs (min_, max_)
  | fuel_min == fuel_max = min_
  | fuel_min < fuel_max  = mostEfficientPosRec xs (min_, max_ - split)
  | otherwise            = mostEfficientPosRec xs (min_ + split, max_)
 where
  delta    = max_ - min_
  split    = (max_ - min_) `div` 2 + (if odd delta then 1 else 0)
  fuel_min = computeTotalFuel min_ xs
  fuel_max = computeTotalFuel max_ xs

