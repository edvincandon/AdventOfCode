import AOCUtils  (readInt)
import Data.List (inits, tails)

main :: IO ()
main = do
  input <- readFile "./src/2024/data/day02.txt"
  putStrLn "---Part 1 -------------"
  print . solve1 $ input
  putStrLn "---Part 2 -------------"
  print . solve2 $ input
  where
    parse = map (map readInt . words) . lines
    solve1 = length . filter id . map isSafe . parse
    solve2 = length . filter id . map (any isSafe . dampen) . parse

    isSafe :: [Int] -> Bool
    isSafe (x : y : xs) =
      let pairs = zip (x : y : xs) (y : xs)
          dir (a, b) = if x < y then a < b else a > b
          delta (a, b) = let d = abs $ a - b in d > 0 && d <= 3
          check = (&&) . dir <*> delta
       in all check pairs
    isSafe _ = False

    dampen :: [Int] -> [[Int]]
    dampen xs = [x ++ y | (x, _ : y) <- zip (inits xs) (tails xs)]
