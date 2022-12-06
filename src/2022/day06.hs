import           Data.Set      (Set)

import qualified Data.Foldable as Set
import qualified Data.Set      as Set

main :: IO ()
main = do
  buffer <- lines <$> readFile "./src/2022/data/day06.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  print $ findMarker 0 4 <$> head $ buffer
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  print $ findMarker 0 14 <$> head $ buffer

findMarker :: Int -> Int -> String -> Int
findMarker n distinct [] = n
findMarker n distinct xs =
  if valid
    then (if Set.length (Set.fromList buffer) /= distinct
            then findMarker (n + 1) distinct (tail xs)
            else n + distinct)
    else error "no markers found"
  where
    buffer = take distinct xs
    valid = distinct == length buffer
