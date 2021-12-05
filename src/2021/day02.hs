import           AOCUtils                       ( readInt )
main :: IO ()
main = do
  instructions <- lines <$> readFile "./src/2021/data/day02.txt"
  let rawData               = map words instructions
  let (pos, depth)          = getPosition rawData
  let ((pos2, depth2), aim) = getPosition2 rawData
  putStrLn
    $  "Part 1 = "
    ++ show (pos, depth, pos * depth)
    ++ "\n"
    ++ "Part 2 = "
    ++ show (pos2, depth2, aim, pos2 * depth2)

-- Part 1 --
getPosition :: [[String]] -> (Int, Int)
getPosition = foldl (flip parseInstruction) initState where initState = (0, 0)

parseInstruction :: [String] -> (Int, Int) -> (Int, Int)
parseInstruction xs (pos, depth) = case head xs of
  "forward" -> (pos + value, depth)
  "up"      -> (pos, depth - value)
  "down"    -> (pos, depth + value)
  _         -> error "invalid instruction"
  where value = readInt $ last xs


-- Part 2 --
getPosition2 :: [[String]] -> ((Int, Int), Int)
getPosition2 = foldl (flip parseInstruction2) initState
  where initState = ((0, 0), 0)

parseInstruction2 :: [String] -> ((Int, Int), Int) -> ((Int, Int), Int)
parseInstruction2 xs ((pos, depth), aim) = case head xs of
  "forward" -> ((pos + value, depth + aim * value), aim)
  "up"      -> ((pos, depth), aim - value)
  "down"    -> ((pos, depth), aim + value)
  _         -> error "invalid instruction"
  where value = readInt $ last xs

