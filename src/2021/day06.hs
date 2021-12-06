{-# LANGUAGE TupleSections #-}
import           AOCUtils                       ( readInt
                                                , parseNums
                                                , minUniqueTuples
                                                , replaceNth
                                                )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( group
                                                , groupBy
                                                , sort
                                                , sortOn
                                                )
import qualified Data.Map                      as M


main :: IO ()
main = do
  fishAges <- map readInt . splitOn "," <$> readFile "./src/2021/data/day06.txt"
  putStrLn "---Part 1 -------------"
  let init = createInitState 8 fishAges
  print $ countFishAfterNDays 80 init
  putStrLn "---Part 2 -------------"
  print $ countFishAfterNDays 256 init

-- Types --

-- Helpers --
simulate :: Int -> [(Int, Int)] -> [(Int, Int)]
simulate n | n <= 0    = id
           | otherwise = simulate (n - 1) . update

update :: [(Int, Int)] -> [(Int, Int)]
update [] = []
update ((_, t1) : xs) =
  map (\(d, t2) -> (d - 1, t2 + (if d - 1 == 6 then t1 else 0))) xs ++ [(8, t1)]


createInitState :: Int -> [Int] -> [(Int, Int)]
createInitState n initState = foldl
  (\_days (age, len) -> replaceNth age (age, len) _days)
  days
  init
 where
  days = map (, 0) [0 .. n]
  init = map (\x -> (head x, length x)) . group $ sort initState


-- Part 1/2 --
countFishAfterNDays :: Int -> [(Int, Int)] -> Int
countFishAfterNDays n = sum . map snd . simulate n
