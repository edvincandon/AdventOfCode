{-# LANGUAGE TupleSections #-}
import           Data.List                      ( minimum
                                                , maximum
                                                )

main :: IO ()
main = do
  contents <- map readInt . lines <$> readFile "./data/day09.txt"
  let invalid = head $ findInvalidValues 25 contents
  let subset  = findContiguousSet contents invalid 0
  print invalid
  print subset
  print $ minimum subset + maximum subset


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

findContiguousSet :: [Int] -> Int -> Int -> [Int]
findContiguousSet xs n idx
  | null xs             = []
  | idx > length xs - 1 = findContiguousSet (drop 1 xs) n 0
  | match               = subset
  | otherwise           = findContiguousSet xs n (idx + 1)
 where
  subset = take idx xs
  match  = sum subset == n
