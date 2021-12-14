{-# LANGUAGE TupleSections #-}
import           Data.List.Split                ( splitOn )
import           Data.Char                      ( isSpace )
import           Data.Bifunctor                 ( bimap )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( sort
                                                , group
                                                , sortBy
                                                , groupBy
                                                )

main :: IO ()
main = do
  rawData <- lines <$> readFile "./src/2021/data/day14.txt"
  putStrLn "---Part 1-------------"
  print $ puzzleAnswer 40 rawData

-- Types --
type CharCount = M.Map Char Int
type PairCount = M.Map String Int
type PolyDic = M.Map String Char

-- Helpers --
parseData :: [String] -> (String, M.Map String Char)
parseData =
  bimap
      head
      ( M.fromList
      . ( map ((\(a : b : _) -> (a, head b)) . splitOn " -> ")
        . filter (not . all isSpace)
        )
      )
    . span (/= "")

getInitialCount :: String -> (CharCount, PairCount)
getInitialCount initial =
  ( M.fromList $ map (\x -> (head x, length x)) . group . sort $ initial
  , M.fromList
    $ map (\x -> ((\(a, b) -> a : [b]) $ head x, length x))
    . group
    $ sort
    $ zip initial (tail initial)
  )

update :: Int -> Maybe Int -> Maybe Int
update count x = case x of
  Just val -> Just (val + count)
  Nothing  -> Just count

-- Part 1/2 --
step :: Int -> CharCount -> PairCount -> PolyDic -> CharCount
step n charCount pairCount polys
  | n == 0 = charCount
  | otherwise = M.unionWith (+)
                            charCount
                            (step (n - 1) nextCharCount nextPairCount polys)
 where
  nextPairCount = foldl
    (\acc (ch1 : ch2 : _, count) ->
      M.alter (update count) [fromJust $ M.lookup [ch1, ch2] polys, ch2]
        $ M.alter (update count) [ch1, fromJust $ M.lookup [ch1, ch2] polys] acc
    )
    M.empty
    (M.toList pairCount)
  -- 
  nextCharCount =
    M.fromList
      $ map (\x -> (fst $ head x, sum $ map snd x))
      . groupBy (\a b -> fst a == fst b)
      . sort
      . map (\(pair, count) -> (, count) $ fromJust $ M.lookup pair polys)
      $ M.toList pairCount

puzzleAnswer :: Int -> [String] -> Int
puzzleAnswer n xs = maximum res - minimum res
 where
  (initial  , dic      ) = parseData xs
  (charCount, pairCount) = getInitialCount initial
  finalCount             = step n charCount pairCount dic
  res                    = map snd $ M.toList finalCount




