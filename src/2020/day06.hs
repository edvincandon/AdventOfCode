import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import           Data.List                      ( intercalate )

main = do
  contents <- parseGroups <$> readFile "./src/2020/data/day06.txt"
  print . findUniqueYes $ contents
  print . findCommonYes $ contents

parseGroups :: String -> [[String]]
parseGroups xs = map (filter (/= "") . splitOn "\n") $ splitOn "\n\n" xs

findUniqueYes :: [[String]] -> Int
findUniqueYes =
  sum . map (Set.size . Set.fromList . chunksOf 1 . intercalate "")

findCommonYes :: [[String]] -> Int
findCommonYes = sum . map countCommonChars

countCommonChars :: [String] -> Int
countCommonChars xs = foldr
  (\chr count ->
    if foldr (\answers match -> match && chr `elem` answers) True rest
      then count + 1
      else count
  )
  0
  first
 where
  first = head xs
  rest  = drop 1 xs
