{-# LANGUAGE MultiWayIf #-}
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Data.Bifunctor                 ( second )
import           Data.Maybe                     ( fromJust )
import           Data.Char                      ( isLower )
import           Data.List                      ( group
                                                , sort
                                                )

main :: IO ()
main = do
  caves <- buildCaveNodes . lines <$> readFile "./src/2021/data/day12.txt"
  putStrLn "---Part 1-------------"
  putStrLn $ "Total paths : " ++ show (countPathsSingle caves)
  putStrLn "---Part 2-------------"
  putStrLn $ "Total paths : " ++ show (countPathsTwice caves)


-- Types --
type CaveGraph = M.Map String [String]
type VisitedCount = M.Map String Int
type CheckFunc = (String -> VisitedCount -> Bool)

-- Helpers --
buildCaveNodes :: [String] -> CaveGraph
buildCaveNodes xs = M.fromListWith (++) nodes
 where
  caves = map (\(a : b : _) -> (a, b)) $ fmap (splitOn "-") xs
  nodes = map (second (: [])) $ caves ++ map (\x -> (snd x, fst x)) caves


findPaths :: CaveGraph -> [String] -> VisitedCount -> CheckFunc -> [[String]]
findPaths graph subPath visited check = concatMap
  (\_sub -> if
    | last _sub == "start" || check (last _sub) visited -> []
    | last _sub == "end" -> [_sub]
    | all isLower (last _sub) -> findPaths
      graph
      _sub
      (M.adjust (+ 1) (last _sub) visited)
      check
    | otherwise -> findPaths graph _sub visited check
  )
  subPaths
 where
  subPaths =
    map ((++) subPath . (: []))
      $ filter (not . flip check visited)
      $ fromJust
      $ M.lookup (last subPath) graph

-- Part 1 --
countPathsSingle :: CaveGraph -> Int
countPathsSingle graph = length $ findPaths
  graph
  ["start"]
  (M.map (const 0) graph)
  (\curr visited -> (== 1) $ M.findWithDefault 0 curr visited)

-- Part 2 --
-- slow --
countPathsTwice :: CaveGraph -> Int
countPathsTwice graph = length $ findPaths
  graph
  ["start"]
  (M.map (const 0) graph)
  (\curr visited -> (M.size (M.filter (> 1) visited) > 1)
    || (== 2) (M.findWithDefault 0 curr visited)
  )
