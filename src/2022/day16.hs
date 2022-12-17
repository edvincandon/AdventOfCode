-- 🐢 should probably use a hash map or any
-- other O(1) lookup datastructure :
-- the Data.Map lookup/update time is O(log n)
-- which slows down the recursive lookups
-- + use a mutable datastructure

import           AOCUtils        (readInt, toIndexedList)
import           Data.Bifunctor  (bimap, second)
import           Data.Bits       (shift, (.&.), (.|.))
import           Data.Char       (isSpace)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)

type Flows = Map String Int
type Nodes = Map String [String]
type Distances = Map String (Map String Int)
type PathsMask = Map String Int -- store paths as visited valves bitmask
type Results = Map Int Int -- Path mask - score

data State =
  State
    { valve    :: String
    , time     :: Int
    , path     :: Int
    , pressure :: Int
    }

data Def =
  Def
    { flows :: Flows
    , dists :: Distances
    , masks :: PathsMask
    }

main :: IO ()
main = do
  (_flows, nodes) <- parse . lines <$> readFile "./src/2022/data/day16.txt"
  let flows = M.filter (> 0) _flows
  let masks = createBitMask flows
  let dists = distances nodes
  putStrLn "---Part 1 -------------"
  let paths = M.toList $ solve (Def flows dists masks) (State "AA" 30 0 0)
  print $ maximum $ snd <$> paths
  putStrLn "---Part 2 -------------"
  let paths' = M.toList $ solve (Def flows dists masks) (State "AA" 26 0 0)
  let tailPaths =
        [ p + p'
        | (path, p) <- paths'
        , (path', p') <- paths'
        , (.&.) path path' == 0
        ]
  print $ maximum tailPaths

parse :: [String] -> (Flows, Nodes)
parse =
  foldl
    (\(rates, nodes) xs ->
       let [def, other] = splitOn ";" xs
           id = [def !! 6, def !! 7]
           rate = readInt $ drop 23 def
           links = map (dropWhile isSpace) <$> splitOn "," $ drop 23 other
        in (M.insert id rate rates, M.insert id links nodes))
    (M.empty, M.empty)

-- resolve minimum distance from
-- any non-zero valve to every other
-- non-zero valve (Floyd Warshall)
-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
distances :: Nodes -> Distances
distances nodes =
  let valves = M.keys nodes
      ns = M.toList nodes
      init v vs =
        if v `elem` vs
          then 1
          else 999999
   in foldl
        (\ds (k, i, j) ->
           M.adjust (\m -> M.adjust (`min` ((m ! k) + (ds ! k) ! j)) j m) i ds)
        (M.fromList [(v, M.fromList (second (init v) <$> ns)) | v <- valves])
        [(k, i, j) | k <- valves, i <- valves, j <- valves]

createBitMask :: Flows -> PathsMask
createBitMask xs =
  M.fromList (bimap fst (shift (1 :: Int)) <$> toIndexedList (M.toList xs))

-- recursively solve and keep track
-- of the best score for a given path
-- bit mask.
solve :: Def -> State -> Results
solve defs@(Def flows dists masks) state = solve' state valves M.empty
  where
    valves = M.keys masks
    solve' :: State -> [String] -> Results -> Results
    solve' _ [] res = res
    solve' state@(State valve time path pressure) (v:rest) res
      | (.&.) (masks ! v) path /= 0 || dt <= 0 = solve' state rest res
      | otherwise = solve' (State v dt path' p') valves next
      where
        p' = pressure + dt * (flows ! v)
        res' = M.alter (\p -> Just (max (fromMaybe 0 p) p')) path res
        dt = time - 1 - (dists ! valve ! v)
        path' = (.|.) path (masks ! v)
        next = solve' state rest res'
