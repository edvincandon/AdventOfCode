{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

import           AOCUtils           (Coord, coordToIndex, indexToCoord)
import           Data.Array.Unboxed
import           Data.Bifunctor     (bimap)
import qualified Data.HashSet       as HS
import           Data.List          (elemIndex)
import           Data.Maybe         (isJust, mapMaybe)

type Grid = (UArray Int Char, (Int, Int)) -- grid, (width, length)

type Reader = Grid -> Int -> Maybe Char

type State = Maybe (Int, Int) -- idx, dir

dir :: Int -> (Int, Int)
dir 0 = (0, -1)
dir 1 = (1, 0)
dir 2 = (0, 1)
dir 3 = (-1, 0)
dir _ = error "invalid"

readPos :: Reader
readPos (arr, (_, size)) idx
  | idx >= 0 && idx < size = Just (arr ! idx)
  | otherwise = Nothing

parse :: String -> (Grid, State)
parse xs =
  let xs' = lines xs
      width = length $ head xs'
      flatGrid = concat xs'
      size = length flatGrid - 1
      grid = (listArray (0, size) flatGrid, (width, size))
      state = ((, 0) <$> elemIndex '^' flatGrid)
   in (grid, state)

move :: Int -> (Int, Int) -> (Int, Int)
move width (idx, d) =
  let (x, y) = indexToCoord width idx
   in (coordToIndex width $ bimap (x +) (y +) (dir d), d)

next :: Grid -> Reader -> State -> State
next grid@(_, (width, _)) reader = (=<<) findPath
  where
    doMove = move width
    doRead = reader grid
    findPath curr@(idx, d) =
      doRead (fst $ doMove curr) >>= \case
        '#' -> Just (idx, (d + 1) `mod` 4)
        _ -> Just (doMove curr)

candidates :: Grid -> State -> [Int]
candidates grid =
  mapMaybe (fst <$>) . takeWhile isJust . iterate (next grid readPos)

loopCount :: Grid -> State -> Int
loopCount grid@(g, width) state =
  sum [1 | p <- positions, checkLoop (withSwap p) HS.empty state]
  where
    xs = candidates grid state
    startIdx = head xs
    -- exclude initial state
    positions = HS.toList . HS.fromList $ filter (startIdx /=) xs
    -- hitting a visited node means we looped
    checkLoop :: Reader -> HS.HashSet (Int, Int) -> Maybe (Int, Int) -> Bool
    checkLoop _ _ Nothing = False
    checkLoop reader seen pos@(Just val) =
      let pos' = next grid reader pos
       in case pos' of
            Nothing -> False
            Just val' ->
              val' `HS.member` seen
                || checkLoop reader (HS.insert val seen) pos'
    -- swap start position with '#'
    withSwap :: Int -> Reader
    withSwap at grid idx =
      if at == idx
        then Just '#'
        else readPos grid idx

main :: IO ()
main = do
  input <- parse <$> readFile "./src/2024/data/day06.txt"
  putStrLn "---Part 1 -------------"
  print . solve1 $ input
  putStrLn "---Part 2 -------------"
  print . solve2 $ input
  where
    solve1 = HS.size . HS.fromList . uncurry candidates
    solve2 = uncurry loopCount
