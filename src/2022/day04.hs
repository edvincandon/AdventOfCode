import           AOCUtils        (readInt)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  pairs <-
    map
      ((\(x:y:_) -> (x, y)) .
       map ((\(a:b:_) -> (readInt a, readInt b)) <$> splitOn "-") . splitOn ",") .
    lines <$>
    readFile "./src/2022/data/day04.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  print $ length $ filter (uncurry fullOverlap) pairs
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  print $ length $ filter (uncurry partialOverlap) pairs

fullOverlap :: (Int, Int) -> (Int, Int) -> Bool
fullOverlap (a, b) (c, d) = (a <= c && b >= d) || (c <= a && d >= b)

partialOverlap :: (Int, Int) -> (Int, Int) -> Bool
partialOverlap (a, b) (c, d) =
  (a <= c && (b >= c || b >= d)) || (c <= a && (d >= a || d >= b))
