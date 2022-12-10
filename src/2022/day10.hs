-- ###...##...##..####.#..#.#....#..#.####.
-- #..#.#..#.#..#.#....#.#..#....#..#.#....
-- ###..#..#.#....###..##...#....####.###..
-- #..#.####.#....#....#.#..#....#..#.#....
-- #..#.#..#.#..#.#....#.#..#....#..#.#....
-- ###..#..#..##..####.#..#.####.#..#.#....
import           AOCUtils        (readInt)
import           Data.List.Split (chunksOf)

type Register = (Int, Int) -- (Cycle count, X Value)

type Instructions = [Int] -- (Add to X)

type Cycles = [Int] -- (List of N steps to take)

main :: IO ()
main = do
  ins <- concatMap parse . lines <$> readFile "./src/2022/data/day10.txt"
  putStrLn "---Part 1 -------------"
  print $ sum $ uncurry (*) <$> applyCycles ins (20 : replicate 5 40)
  putStrLn "---Part 2 -------------"
  putStrLn $
    unlines . chunksOf 40 $
    concat $ reverse $ drawPixel <$> applyCycles ins (replicate 240 1)

parse :: String -> Instructions
parse xs =
  case tail of
    [] -> [0]
    _  -> [0, readInt $ head tail]
  where
    _:tail = words xs

applyCycles :: Instructions -> Cycles -> [Register]
applyCycles ins cycles =
  init $
  fst <$>
  foldl
    (\cycles' step -> applyCyclesN (head cycles') step : cycles')
    [((0, 1), 0 : ins)]
    cycles

applyCyclesN :: (Register, Instructions) -> Int -> (Register, Instructions)
applyCyclesN ((count, xval), ins) n
  | null ins = ((count, xval), ins)
  | n == 0 = ((count, xval), ins)
  | n == 1 = ((count + 1, xval + add), ins')
  | otherwise = applyCyclesN ((count + 1, xval + add), ins') (n - 1)
  where
    add:ins' = ins

drawPixel :: Register -> String
drawPixel (count, xval) =
  if match
    then "#"
    else "."
  where
    match = elem xval $ [id, (1 +), flip (-) 1] <*> [(count - 1) `mod` 40]
