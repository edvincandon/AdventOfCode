import           AOCUtils        (splitHalf, tupleToList)
import           Data.Bifunctor  (bimap)
import           Data.Set        (Set)

import qualified Data.Set        as Set

import           Data.List       (elemIndex)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (fromJust)

main :: IO ()
main = do
  items <- lines <$> readFile "./src/2022/data/day03.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  let compartments = map (bimap Set.fromList Set.fromList . splitHalf) items
  print $ sum (fromJust (mapM (charToInt . common . tupleToList) compartments))
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  let groups = chunksOf 3 $ map Set.fromList items
  print $ fromJust $ fmap sum <$> sequence $ charToInt . common <$> groups

common :: [Set Char] -> Char
common (xs:tail) = head $ Set.elems $ foldl Set.intersection xs tail
common []        = error "Empty input"

charToInt :: Char -> Maybe Int
charToInt = fmap (+ 1) . flip elemIndex (['a' .. 'z'] ++ ['A' .. 'Z'])
