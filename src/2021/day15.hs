import           AOCUtils        (readInt, safeIndex)
import           Data.Char       (isSpace)
import qualified Data.Heap       as H
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust, fromMaybe, isJust)
import qualified Data.Set        as S

main :: IO ()
main = do
  rawData <- readFile "./src/2021/data/day15.txt"
  putStrLn "---Part 1-------------"
  -- let nodeList = parseNodes rawData
  -- let lastIdx = uncurry (*) (snd nodeList) - 1
  -- print $ getNodeAtIndexExpanded nodeList 21
  -- let s@(Dijkstra _ _ _ costs) =
  --       dijkstra $ dijkstraInit nodeList (getNodeAtIndex nodeList 0)
  -- print "t"
  -- print $ fromJust $ M.lookup lastIdx costs
  -- let s@(Dijkstra _ _ _ costs2) =
  --       dijkstra $ dijkstraInit nodeList (getNodeAtIndexExpanded nodeList 0)
  -- print $ fromJust $ M.lookup (uncurry (*) (snd nodeList) * 25 - 1) costs2

-- Helpers --
type Visited = S.Set Int -- Node index

type NodeCosts = M.Map Int Int -- Node index : Distance

type NodeList = ([Int], (Int, Int)) -- Node List, bounds

data Node =
  Node Int Int [Int] -- Node Index, Weight, Edges (neighbour indexes) --
  deriving (Show)
-- -- Node Min Heap --
-- newtype NodeDist = NodeDist (Int, Node)
--   deriving Show
-- instance Eq NodeDist where
--   (NodeDist (a, _)) == (NodeDist (b, _)) = a == b
-- instance Ord NodeDist where
--   compare (NodeDist (a, _)) (NodeDist (b, _)) = compare a b
-- type NodeMinHeap = H.Heap NodeDist
-- -- Dijkstra state --
-- data Dijkstra = Dijkstra
--   { nodes   :: NodeList
--   , heap    :: NodeMinHeap
--   , visited :: Visited
--   , costs   :: NodeCosts
--   }
--   deriving Show
-- -- Helpers --
-- infinity :: Int
-- infinity = 999999999999999999
-- parseNodes :: String -> ([Int], (Int, Int))
-- parseNodes xs = (concat nodes, bounds)
--  where
--   nodes =
--     map (map readInt . filter (not . all isSpace) . splitOn "") $ lines xs
--   bounds = (length (head nodes), length nodes)
-- getNodeAtIndex :: NodeList -> Int -> Node
-- getNodeAtIndex (xs, (maxX, maxY)) idx = Node
--   idx
--   (xs !! idx)
--   (getNeighbours (x, y) (maxX, maxY))
--   where (x, y) = (idx `mod` maxX, idx `div` maxY)
-- getNodeAtIndexExpanded :: NodeList -> Int -> Node
-- getNodeAtIndexExpanded (xs, (_maxX, _maxY)) idx = Node
--   idx
--   (getExpandedValue idx)
--   (getNeighbours (x, y) (maxX, maxY))
--  where
--   maxX   = _maxX * 5
--   maxY   = _maxY * 5
--   (x, y) = (idx `mod` maxX, idx `div` maxY)
--   getExpandedValue :: Int -> Int -- index -> value
--   getExpandedValue expandedIdx = _value
--    where
--     (xx     , yy         ) = (expandedIdx `mod` maxX, expandedIdx `div` maxY)
--     (targetX, multiplierX) = (xx `div` maxX, xx `mod` 5)
--     (targetY, multiplierY) = (yy `div` maxY, yy `mod` 5)
--     _idx = maxY * (targetY + multiplierY) + (targetX + multiplierX)
--     _value                 = (xs !! _idx + multiplierX + multiplierY) `mod` 10
-- getNeighbours :: (Int, Int) -> (Int, Int) -> [Int]
-- getNeighbours (x, y) (maxX, maxY) = map (\(xx, yy) -> maxY * yy + xx) $ filter
--   (\(xx, yy) -> xx >= 0 && xx < maxX && yy >= 0 && yy < maxY)
--   [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
-- dijkstraInit :: NodeList -> Node -> Dijkstra
-- dijkstraInit nodes first = Dijkstra { visited = visited
--                                     , nodes   = nodes
--                                     , costs   = M.fromList [(0, 0)]
--                                     , heap    = heap
--                                     }
--  where
--   visited = S.empty
--   heap    = H.insert (NodeDist (0, first)) H.empty
-- dijkstra :: Dijkstra -> Dijkstra
-- dijkstra state@(Dijkstra nodeList heap visited costs)
--   | H.null heap = state
--   | otherwise   = dijkstra $ Dijkstra nodeList nextHeap nextVisited nextCosts
--  where
--   (NodeDist (_, Node idx weight _edges), _heap) = fromJust $ H.uncons heap
--   nextVisited = S.insert idx visited
--   edges       = map (getNodeAtIndexExpanded nodeList) -- pass as parameter
--     $ filter (`S.notMember` nextVisited) _edges
--   (nextHeap, nextCosts) = foldl
--     (\(_nextHeap, _costs) adj -> dijkstraTraverse idx adj _nextHeap _costs)
--     (_heap, costs)
--     edges
-- dijkstraTraverse
--   :: Int -> Node -> NodeMinHeap -> NodeCosts -> (NodeMinHeap, NodeCosts)
-- dijkstraTraverse currIdx adjNode@(Node adjIdx adjWeight _) heap costs =
--   if newCost < adjCost
--     then
--       ( H.insert (NodeDist (newCost, adjNode)) heap
--       , M.alter (\_ -> Just newCost) adjIdx costs
--       )
--     else (heap, costs)
--  where
--   newCost = fromJust (M.lookup currIdx costs) + adjWeight
--   adjCost = fromMaybe infinity (M.lookup adjIdx costs)
