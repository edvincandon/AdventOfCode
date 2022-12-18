{-# LANGUAGE InstanceSigs #-}

import           AOCUtils       (hash, readInt)
import           Data.Bifunctor (bimap, first, second)
import           Data.Map       as Map (Map, (!))
import qualified Data.Map       as M
import           Data.Set       as Set (Set, fromList)
import qualified Data.Set       as Set

type Coord = (Int, Int)
type Rock = Set Coord

data RockState a
  = Falling a
  | Rest a
  deriving (Show)

data State =
  State
    { coords :: Set Coord
    , rock   :: Rock
    , jets   :: String
    , count  :: Int
    , rCount :: Int
    }

instance Show State where
  show :: State -> String
  show state =
    unlines $
    reverse $
    zipWith
      (curry
         (\c ->
            if Set.member c $ coords state
              then '#'
              else '.'))
      [0 .. 6] .
    repeat <$>
    [(max 0 (yMax - 50)) .. yMax] -- Show 50 max rows
    where
      ys = Set.map snd (coords state)
      yMax = Set.findMax ys

-- 🧮 optimization : always keep a grid of
-- max height 2*window in memory, we assume
-- it's a reasonable amount for at least one
-- column to contain at least a single coord.
-- When looking for cycles, search for patterns
-- of size window.
window :: Int
window = 25

main :: IO ()
main = do
  jets <- readFile "./src/2022/data/day17.txt"
  putStrLn "---Part 1 -------------"
  let initial = Set.fromList $ zip [0 .. 6] (repeat (-1))
  let part1 = loop (State initial (getRock 0 3) jets 0 0) 2022
  print $ getHeight part1
  putStrLn "---Part 2 -------------"
  let part2 = loop (State initial (getRock 0 3) jets 0 0) 1000000000000
  print $ getHeight part2

getRock :: Int -> Int -> Rock
getRock n y = Set.fromList (bimap (+ 2) (+ y) <$> rock)
  where
    idx = n `mod` 5
    rock
      | idx == 0 = [(0, 0), (1, 0), (2, 0), (3, 0)]
      | idx == 1 = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
      | idx == 2 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
      | idx == 3 = [(0, 0), (0, 1), (0, 2), (0, 3)]
      | idx == 4 = [(0, 0), (1, 0), (0, 1), (1, 1)]
      | otherwise = error "impossible"

moveX :: Rock -> Char -> Rock
moveX rock dir =
  if min < 0 || max > 6
    then rock
    else next
  where
    dx =
      if dir == '<'
        then (-1)
        else 1
    next = Set.map (first (+ dx)) rock
    (min, _) = Set.findMin next
    (max, _) = Set.findMax next

moveY :: Rock -> Int -> Rock
moveY rock dir = Set.map (second (+ dir)) rock

loop :: State -> Int -> State
loop state max = loop' state M.empty max
  where
    loop' :: State -> Map Int (Int, Int, Int) -> Int -> State
    loop' state@(State grid rock jets count rCount) seen max
      | M.member key seen =
        let cycle@(count', rCount', height') = (seen ! key)
            remRocks = max - rCount
            rocksPerCycle = rCount - rCount'
            remCycles = (remRocks `div` rocksPerCycle)
            dh = (getHeight state - height') * remCycles
            state' =
              state
                { coords = Set.map (second (dh +)) grid
                , rock = Set.map (second (dh +)) rock
                , count = count + ((count - count') * remCycles)
                , rCount = rCount + (remCycles * rocksPerCycle)
                }
         in loop' (step state') M.empty max
      | rCount == max = state
      | otherwise =
        loop'
          (step state)
          (if count > length jets
             then M.insert key (count, rCount, getHeight state) seen
             else seen)
          max
      where
        key = getKey state

step :: State -> State
step s@(State coords _ [] _ _) = s
step s@(State coords rock jets count rCount) =
  let count' = count + 1
   in case fall rock (jets !! (count `mod` length jets)) coords of
        Falling rock' -> step (s {rock = rock', count = count'})
        Rest rock' ->
          let coords' = Set.union coords rock'
              maxY = maximum (snd <$> Set.toList coords') + 1
           in gc
                s
                  { coords = coords'
                  , rock = getRock (rCount + 1) (maxY + 3)
                  , rCount = rCount + 1
                  , count = count'
                  }

fall :: Rock -> Char -> Set Coord -> RockState Rock
fall rock dir coords = res
  where
    nextX =
      let next = moveX rock dir
       in if Set.disjoint coords next
            then next
            else rock
    nextY = moveY nextX (-1)
    res =
      if Set.disjoint coords nextY
        then Falling nextY
        else Rest nextX

getHeight :: State -> Int
getHeight s = maximum (snd <$> Set.toList (coords s)) + 1

-- 🐢 flush the coords set after a certain
-- heuristic max Y value (window) to keep
-- our memory footprint reasonably smaller
gc :: State -> State
gc state = state {coords = coords'}
  where
    maxY = Set.findMax $ Set.map snd $ coords state
    coords' = Set.filter (\(_, y) -> y > maxY - (window * 2)) (coords state)

-- hash/encode the current state's 30 top
-- rows with the current jet index and the
-- current rock index
getKey :: State -> Int
getKey (State coords _ jets count rCount) = hash key
  where
    coords' = Set.toList coords
    yMax = Set.findMax $ Set.map snd coords
    top50 = Set.filter (\(_, y) -> y > (yMax - window)) coords
    key =
      show (Set.map (second (yMax -)) top50) ++
      show (count `mod` length jets) ++ show (rCount `mod` 5)
