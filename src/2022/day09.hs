import           AOCUtils (readInt)
import           Data.Set (Set)

import qualified Data.Set as Set

type Coord = (Int, Int)

type Rope = ((Coord, [Coord]), Set Coord)

main :: IO ()
main = do
  moves <-
    concatMap ((\(dir:n:_) -> replicate (readInt n) dir) . words) <$>
    (lines <$> readFile "./src/2022/data/day09.txt")
  putStrLn "---Part 1 -------------"
  let part1 = (((0, 0), [(0, 0)]), Set.empty) :: Rope
  print $ length . snd $ foldl (flip applyTailMove) part1 moves
  putStrLn "---Part 2 -------------"
  let part2 = (((0, 0), replicate 9 (0, 0)), Set.empty) :: Rope
  print $ length . snd $ foldl (flip applyTailMove) part2 moves

step :: Int -> Int -> Int
step a b =
  if a > b
    then 1
    else -1

moveHead :: Coord -> String -> Coord
moveHead (hx, hy) dir =
  case dir of
    "U" -> (hx, hy + 1)
    "R" -> (hx + 1, hy)
    "D" -> (hx, hy - 1)
    "L" -> (hx - 1, hy)
    _   -> error "unknown move"

moveTail :: Coord -> Coord -> Coord
moveTail (hx', hy') (tx, ty) = nextTail
  where
    nextTail
      | abs (hx' - tx) <= 1 && abs (hy' - ty) <= 1 = (tx, ty)
      | hx' == tx = (tx, ty + step hy' ty)
      | hy' == ty = (tx + step hx' tx, ty)
      | otherwise = (tx + step hx' tx, ty + step hy' ty)

applyTailMove :: String -> Rope -> Rope
applyTailMove dir ((_head, tails), tSet) =
  ((head', tails'), Set.insert (last tails') tSet)
  where
    head' = moveHead _head dir
    tails' =
      foldl
        (\tails' t' -> tails' ++ [moveTail (last tails') t'])
        [moveTail head' (head tails)] $
      tail tails
