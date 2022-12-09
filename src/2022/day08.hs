import           AOCUtils        (readInt, safeIndex, toIndexedMatrix2)
import           Control.Monad   (join)
import           Data.Bifunctor  (first, second)
import           Data.List       (transpose, zip4)
import           Data.List.Split (splitOn)

type Coord = (Int, Int)

type Tree = (Int, Coord)

main :: IO ()
main = do
  grid <-
    fmap (map readInt . drop 1 . splitOn "") . lines <$>
    readFile "./src/2022/data/day08.txt"
  putStrLn "---Part 1 -------------"
  print $ visibleTrees grid
  putStrLn "---Part 2 -------------"
  let treeM = toIndexedMatrix2 grid
  print $ maximum (concatMap (fmap (`scoreAtTreeWith` treeM)) treeM)

-- too lazy to do it in a single pass so
-- transpose & reverse 🐢 🐢 🐢
visibleTrees :: [[Int]] -> Int
visibleTrees grid =
  length . filter (== True) $
  (\(a, b, c, d) -> a || b || c || d) <$> zip4 top right bottom left
  where
    left = concat $ raycast grid
    right = concatMap reverse (raycast (reverse <$> grid))
    top = concat $ transpose (raycast $ transpose grid)
    bottom = concat $ reverse $ transpose (raycast $ transpose $ reverse grid)

-- raycast from left to right
raycast :: [[Int]] -> [[Bool]]
raycast =
  map (fst <$>) <$>
  (foldl
     (\acc h ->
        if null acc
          then [(True, [h])]
          else let (v', hs) = last acc
                in (acc ++ [(all (h >) hs, hs ++ [h])]))
     ([] :: [(Bool, [Int])]) <$>)

scoreAtTreeWith :: Tree -> [[Tree]] -> Int
scoreAtTreeWith (h, coord) grid = score
  where
    score =
      product $
      getNextScore coord <$>
      [second (flip (-) 1), second (+ 1), first (flip (-) 1), first (+ 1)]
    getNextScore :: (Int, Int) -> (Coord -> Coord) -> Int
    getNextScore (x, y) dir =
      case next of
        Just (h', nextCoord) ->
          1 +
          if h' < h
            then getNextScore nextCoord dir
            else 0
        Nothing -> 0
      where
        (x', y') = dir (x, y)
        next = flip safeIndex x' =<< safeIndex grid y'
