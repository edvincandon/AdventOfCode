{-# LANGUAGE MultiWayIf #-}
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Data.Bifunctor                 ( second )
import           Data.Maybe                     ( fromJust )
import           Data.Char                      ( isLower )

main :: IO ()
main = do
  caves <- parseCavePaths . lines <$> readFile "./src/2021/data/day12.txt"
  putStrLn "---Part 1-------------"
  let nodes = buildCaveNodes caves
  putStrLn $ "Total paths : " ++ show (countPaths nodes)
  putStrLn "---Part 2-------------"
  let nodes = buildCaveNodes caves
  putStrLn $ "Total paths : " ++ show (countPaths nodes)


-- Types --
type CaveGraph = M.Map String [String]

-- Helpers --
parseCavePaths :: [String] -> [(String, String)]
parseCavePaths xs = map (\(a : b : _) -> (a, b)) $ fmap (splitOn "-") xs

buildCaveNodes :: [(String, String)] -> CaveGraph
buildCaveNodes caves = M.fromListWith (++) nodes
  where nodes = map (second (: [])) $ caves ++ map (\x -> (snd x, fst x)) caves

findPaths :: CaveGraph -> [String] -> [String] -> [[String]]
findPaths graph subPath visited = concatMap
  (\_sub -> if
    | last _sub `elem` visited -> []
    | last _sub == "end"       -> [_sub]
    | all isLower $ last _sub  -> findPaths graph _sub (last _sub : visited)
    | otherwise                -> findPaths graph _sub visited
  )
  subPaths
 where
  subPaths =
    map ((++) subPath . (: []))
      $ filter (not . flip elem visited)
      $ fromJust
      $ M.lookup (last subPath) graph

-- Part 1 --
countPaths :: CaveGraph -> Int
countPaths graph = length $ findPaths graph ["start"] ["start"]

-- Part 2 --
