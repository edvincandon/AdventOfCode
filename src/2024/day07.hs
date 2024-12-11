import           Data.List       (find, inits, tails)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)

type Op = Int -> Int -> Int

makeCombinations :: [Op] -> Int -> [Int] -> [Int]
makeCombinations _ _ [x] = [x]
makeCombinations ops max ints@(x:xs)
  | x > max = []
  | x == max = [x]
  | otherwise =
    let nexts = makeCombinations ops max xs
     in foldl (\res op -> res ++ map (`op` x) nexts) [] ops

cat :: Op
cat a b = read (show a ++ show b)

solve :: [Op] -> [(Int, [Int])] -> Int
solve ops =
  let combinations = makeCombinations ops
   in sum . mapMaybe (\(val, ints) -> find (val ==) (combinations val ints))

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "./src/2024/data/day07.txt"
  putStrLn "---Part 1 -------------"
  print $ solve [(+), (*)] input
  putStrLn "---Part 2 -------------"
  print $ solve [cat, (+), (*)] input
  where
    parse = (\[t, n] -> (read t, map read $ reverse (words n))) . splitOn ":"
