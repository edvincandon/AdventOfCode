{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module AOCUtils where

import           Data.Heap       (Heap)
import qualified Data.Heap       as Heap
import           Data.List       (genericLength, group, groupBy, sort, sortOn)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Bits       (xor)
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Stack      (Stack, stackPop, stackPush)

-- Helpers --
-- DATA PARSING --
readInt :: String -> Int
readInt = read

readIntLast :: String -> Int
readIntLast = readInt . last . words

parseNums :: String -> [Int]
parseNums = fmap readInt . splitOn ","

-- STATS --
mean :: (Real a, Fractional b) => [a] -> b
mean [] = error "mean imposible"
mean xs = realToFrac (sum xs) / genericLength xs

median :: (Real a, Fractional b) => [a] -> b
median [] = error "median impossible"
median xs
  | length xs == 1 = realToFrac $ head xs
  | odd split = realToFrac $ (!!) sorted $ (split - 1) `div` 2
  | otherwise =
    realToFrac
      $ mean [(!!) sorted $ split `div` 2 - 1, (!!) sorted $ split `div` 2]
  where
    sorted = sort xs
    split = genericLength sorted

frequencies :: Ord a => [a] -> Map.Map a Int
frequencies = Map.fromListWith (+) . map (, 1)

-- LISTS --
toIndexedList :: [a] -> [(a, Int)]
toIndexedList = flip zip [0 ..]

toIndexedMatrix2 :: [[a]] -> [[(a, (Int, Int))]]
toIndexedMatrix2 rows =
  (\(cols, y) -> (\(xs, x) -> (xs, (x, y))) <$> toIndexedList cols)
    <$> toIndexedList rows

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing

safeIndex1 :: [a] -> Int -> a -> a
safeIndex1 xs i d = fromMaybe d (safeIndex xs i)

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs
  | n > 0 = take n xs : groupByN n (drop n xs)
  | otherwise = error "invalid group arguments"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt ((length xs + 1) `div` 2) xs

midPoint :: [a] -> a
midPoint xs = xs !! (length xs `div` 2)

-- 2D Matrix --
type Coord = (Int, Int)

type Dir = Coord

(.+) :: Coord -> Coord -> Coord
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

(.-) :: Coord -> Coord -> Coord
(x1, y1) .- (x2, y2) = (x1 - x2, y1 - y2)

allDirs :: [Dir]
allDirs = [(0, 1), (1, 0), (1, 1), (-1, 1), (0, -1), (-1, 0), (-1, -1), (1, -1)]

translate :: Coord -> Dir -> Coord
translate (x, y) (dx, dy) = (x + dx, y + dy)

elemAtCoord :: [[a]] -> Coord -> Maybe a
elemAtCoord grid (x, y) = do
  row <- safeIndex grid y
  safeIndex row x

index2D :: [[a]] -> [[(a, Coord)]]
index2D grid =
  [zipWith (\x val -> (val, (x, y))) [0 ..] cols | (y, cols) <- zip [0 ..] grid]

coordToIndex :: Int -> Coord -> Int
coordToIndex width (x, y) =
  if x < 0 || x >= width
    then -1
    else y * width + x

indexToCoord :: Int -> Int -> Coord
indexToCoord width idx = (idx `mod` width, idx `div` width)

-- BINARY --
bin2num :: [Int] -> Int
bin2num list = parse (reverse list) 0
  where
    parse [] _     = 0
    parse (x:xs) n = x * (2 ^ n) + parse xs (n + 1)

-- TUPLES --
newtype Tuple4 a =
  Tuple4 (a, a, a, a)
  deriving (Show, Eq)

instance Num a => Semigroup (Tuple4 a) where
  (<>) :: Tuple4 a -> Tuple4 a -> Tuple4 a
  (<>) (Tuple4 (a, b, c, d)) (Tuple4 (a', b', c', d')) =
    Tuple4 (a + a', b + b', c + c', d + d')

instance Ord a => Ord (Tuple4 a) where
  compare :: Tuple4 a -> Tuple4 a -> Ordering
  compare (Tuple4 a) (Tuple4 b) = compare a b

instance Functor Tuple4 where
  fmap :: (a -> b) -> Tuple4 a -> Tuple4 b
  fmap f (Tuple4 (a, b, c, d)) = Tuple4 (f a, f b, f c, f d)

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

toTuple :: [a] -> (a, a)
toTuple (x:y:_) = (x, y)
toTuple _       = error "List needs at least 2 elements"

uniqueTuples :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
uniqueTuples = minUniqueTuples 1

minUniqueTuples :: (Ord a, Ord b) => Int -> [(a, b)] -> [(a, b)]
minUniqueTuples minRep =
  fmap head
    <$> (concatMap
           (filter ((>= minRep) . length)
              . groupBy (\a b -> snd a == snd b)
              . sortOn snd)
           . groupBy (\a b -> fst a == fst b)
           . sortOn fst)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- STACKS --
popMultiple :: Int -> Stack a -> Maybe (Stack a, [a])
popMultiple n stack = pop n (Just (stack, []))
  where
    pop :: Int -> Maybe (Stack a, [a]) -> Maybe (Stack a, [a])
    pop 0 result = result
    pop _ Nothing = Nothing
    pop n (Just (s, values)) =
      pop (n - 1) ((\(s, v) -> (s, values ++ [v])) <$> stackPop s)

pushMultiple :: Stack a -> [a] -> Stack a
pushMultiple stack [] = stack
pushMultiple stack xs = pushMultiple (stackPush stack $ last xs) (init xs)

-- HASHING --
hash :: String -> Int
hash = foldl (\h c -> 33 * h `xor` fromEnum c) 5381

-- DJIKSTRA's Algorithm --
data Distance a
  = Dist a
  | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  (<=) :: Ord a => Distance a -> Distance a -> Bool
  Infinity <= Infinity = True
  Infinity <= Dist _   = False
  Dist _ <= Infinity   = True
  Dist x <= Dist y     = x <= y

type Node = (String, Int)

newtype DjikstraGraph = Graph
  { edges :: Map String [Node]
  } deriving (Show)

type DjikstraMinHeap = Heap Heap.FstMinPolicy (Distance Int, String)

type DjikstraDistMap = Map String (Distance Int)

data Djikstra = Djikstra
  { visited   :: Set String
  , distances :: DjikstraDistMap
  , queue     :: DjikstraMinHeap
  }

-- adder that accounts for Infinity
-- values when summing distances
dAddDist :: (Num a) => Distance a -> Distance a -> Distance a
dAddDist (Dist x) (Dist y) = Dist (x + y)
dAddDist _ _               = Infinity

-- looks up a distance in our
-- distances map and either unwraps
-- it or returns Infinity
dDistAt :: (Ord k, Eq k) => Map k (Distance d) -> k -> Distance d
dDistAt distances key = fromMaybe Infinity (Map.lookup key distances)

djikstra :: DjikstraGraph -> String -> String -> Distance Int
djikstra graph start end = dDistAt (process state) end
  where
    visited = Set.empty
    distances = Map.singleton start (Dist 0)
    queue = Heap.fromList [(Dist 0, start)] :: DjikstraMinHeap
    state = Djikstra visited distances queue
    process :: Djikstra -> DjikstraDistMap
    process state@(Djikstra visited distances queue) =
      case Heap.view queue of
        Nothing -> distances -- heap is empty return distances
        Just ((minDist, node), queue') ->
          if node == end -- we reached the destination node
            then distances -- return all distances
            else if Set.member node visited -- node has been visited
                   then process (state {queue = queue'}) -- continue at next queue
                        -- 1. Add node to visited set
                        -- 2. find unvisited neighbors
                        -- 3. fold over these unvisited neighbors
                        -- and update djikstra state accordingly
                   else let visited' = Set.insert node visited
                            neighbors =
                              fromMaybe [] (Map.lookup node (edges graph))
                            unvisited =
                              filter
                                (not . flip Set.member visited' . fst)
                                neighbors
                         in process
                              $ foldl
                                  (foldNeighbor node)
                                  (Djikstra visited' distances queue')
                                  unvisited
          -- fold over every unvisited neighbor for
          -- the current node at the next state (distances not updated yet) :
          -- 1. Compute distance from node to neighbor
          -- 2. If distance is smaller than value in distance map
          -- then update map and update value in MinHeap
          -- else continue
          where foldNeighbor :: String -> Djikstra -> Node -> Djikstra
                foldNeighbor current state@(Djikstra _visited' _distances _queue') (neighbor, cost) =
                  let dist = dAddDist (dDistAt _distances current) (Dist cost)
                   in if dist < dDistAt _distances neighbor
                        then Djikstra
                               _visited'
                               (Map.insert neighbor dist _distances)
                               (Heap.insert (dist, neighbor) _queue')
                        else state
