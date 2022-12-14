{-# LANGUAGE TupleSections #-}

import           AOCUtils        (readInt)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map

type Coord = (Int, Int)
type Floor = Int
type Walls = (Int, Int)
type Grid = Map Coord ()

main :: IO ()
main = do
  (grid, walls, floor) <- parse <$> readFile "./src/2022/data/day14.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  let part1 = flow (500, 0) grid walls maxBound
  print $ length part1 - length grid
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  let part2 = flow (500, 0) grid (minBound, maxBound) floor
  print $ length part2 - length grid

parse :: String -> (Grid, Walls, Floor)
parse xs =
  ( Map.fromList ((, ()) <$> coords)
  , (minimum $ map fst coords, maximum $ map fst coords)
  , 1 + maximum (map snd coords))
  where
    splits = map (map readInt . splitOn ",") . splitOn "->" <$> lines xs
    def = map (\(x:delta:_) -> (x, delta)) <$> splits
    coords =
      concatMap
        (\obs ->
           foldl
             (\grid coord -> grid ++ spread (last grid) coord)
             [head obs]
             (tail obs))
        def

spread :: Coord -> Coord -> [Coord]
spread start@(x, y) end@(x', y')
  | x == x' = map (\delta -> (x, y + delta)) (op <$> take (abs (y' - y)) [1 ..])
  | y == y' = map (\delta -> (x + delta, y)) (op <$> take (abs (x' - x)) [1 ..])
  | otherwise = error "impossible"
  where
    op :: Int -> Int
    op =
      if x' < x || y' < y
        then (* (-1))
        else id

fall :: Coord -> Grid -> Walls -> Floor -> Maybe Coord
fall start grid bounds@(min, max) floor = step start
  where
    step :: Coord -> Maybe Coord
    step pos@(x, y)
      | y == floor = Just pos
      | fst nextLeft < min || snd nextRight > max = Nothing
      | Map.notMember next grid = step next
      | Map.notMember nextLeft grid = step nextLeft
      | Map.notMember nextRight grid = step nextRight
      | otherwise =
        if Map.notMember pos grid
          then Just pos
          else Just (x, y - 1)
      where
        next = (x, y + 1)
        nextLeft = (x - 1, y + 1)
        nextRight = (x + 1, y + 1)

flow :: Coord -> Grid -> Walls -> Floor -> Grid
flow start grid bounds floor =
  if Map.notMember start grid
    then case fall start grid bounds floor of
           Just coord -> flow start (Map.insert coord () grid) bounds floor
           Nothing    -> grid
    else grid
