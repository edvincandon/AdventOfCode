import           AOCUtils       (frequencies)
import           Data.Bifunctor (bimap)
import           Data.List      (sort)
import qualified Data.Map       as Map

main :: IO ()
main = do
  input <- readFile "./src/2024/data/day01.txt"
  putStrLn "---Part 1 -------------"
  print . solve1 $ input
  putStrLn "---Part 2 -------------"
  print . solve2 $ input
  where
    parse = map ((\(x : y : _) -> (x, y)) . map read . words) . lines
    distance x y = abs (x - y)
    similarity xs freq = sum $ map (\x -> x * Map.findWithDefault 0 x freq) xs
    solve1 = sum . uncurry (zipWith distance) . bimap sort sort . unzip . parse
    solve2 = uncurry similarity . bimap sort frequencies . unzip . parse
