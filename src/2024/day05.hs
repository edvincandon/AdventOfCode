import           AOCUtils        (midPoint, readInt, toTuple)
import           Data.Bifunctor  (bimap)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import           Data.Maybe      (mapMaybe)

type PageOrdering = M.Map Int [Int]

comparePages :: PageOrdering -> Int -> Int -> Ordering
comparePages ordering curr next =
  case M.lookup curr ordering of
    Just order ->
      if next `elem` order
        then LT
        else GT
    Nothing -> GT

solve :: PageOrdering -> [[Int]] -> Int
solve ordering =
  sum . map midPoint . filter ((==) <*> sortBy (comparePages ordering))

solve' :: PageOrdering -> [[Int]] -> Int
solve' ordering =
  sum
    . mapMaybe
        (\xs ->
           let sorted = sortBy (comparePages ordering) xs
            in if sorted /= xs
                 then Just (midPoint sorted)
                 else Nothing)

main :: IO ()
main = do
  input <- readFile "./src/2024/data/day05.txt"
  putStrLn "---Part 1 -------------"
  print . uncurry solve . parse $ input
  putStrLn "---Part 2 -------------"
  print . uncurry solve' . parse $ input
  where
    parseInputs = toTuple . map lines . splitOn "\n\n"
    parse = bimap parseOrders parseUpdates . parseInputs
    parseUpdates = map $ map readInt . splitOn ","
    parseOrders =
      M.fromListWith (++)
        . map (bimap readInt ((: []) . readInt) . toTuple . splitOn "|")
