{-# LANGUAGE TupleSections #-}
import           Data.List

main :: IO ()
main = do
  contents <- map readInt . lines <$> readFile "./data/day09.txt"
  print $ head $ findInvalidValues 25 contents

readInt :: String -> Int
readInt = read

combinations :: [a] -> [(a, a)]
combinations []       = []
combinations (x : xs) = map (x, ) xs ++ combinations xs

isValid :: [Int] -> Int -> Bool
isValid xs n = not . null $ [ x | x <- combinations xs, uncurry (+) x == n ]

findInvalidValues :: Int -> [Int] -> [Int]
findInvalidValues n xs =
  foldr
      (\val invalid ->
        if snd val >= 0 && not (isValid (take n $ drop (snd val) xs) (fst val))
          then invalid ++ [fst val]
          else invalid
      )
      []
    $ reverse
    $ zip xs [(-n) ..]

