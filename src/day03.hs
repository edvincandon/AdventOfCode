main = do
  contents <- readFile "./data/day03.txt"
  -- print $ traverseSlope contents 3 1
  print . product $ map (uncurry (traverseSlope contents))
                        [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

traverseSlope :: String -> Int -> Int -> Int
traverseSlope xs dx dy = foldl
  (\count idx ->
    (if [(sl !! idx) !! mod (idx * dx) xMax] == "#" then count + 1 else count)
  )
  0
  [ i | i <- takeWhile (< yMax) [0 ..], i `mod` dy == 0 ]
 where
  sl   = lines xs
  xMax = length . head $ sl
  yMax = length $ words xs
