import           AOCUtils  (readInt)
import           Data.List (sortBy)

main :: IO ()
main = do
  cals <- lines <$> readFile "./src/2022/data/day01.txt"
  let totals =
        foldl
          (\(xs:tail) line ->
             if null line
               then 0 : xs : tail
               else (xs + readInt line) : tail)
          [0]
          cals
  putStrLn "---Part 1 -------------"
  print $ maximum totals
  putStrLn "---Part 2 -------------"
  print $ sum <$> take 3 $ sortBy (flip compare) totals
