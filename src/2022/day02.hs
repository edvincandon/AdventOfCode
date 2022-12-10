type Round = (Int, Int)

main :: IO ()
main = do
  rounds <-
    map ((\(xs:tail) -> (leftScore xs, rightScore $ head tail)) . words) . lines <$>
    readFile "./src/2022/data/day02.txt"
  putStrLn "---Part 1 -------------"
  print $ sum $ score <$> rounds
  putStrLn "---Part 2 -------------"
  print $ sum $ score . update <$> rounds

leftScore :: String -> Int
leftScore xs =
  case xs of
    "A" -> 1
    "B" -> 2
    "C" -> 3
    _   -> 0

rightScore :: String -> Int
rightScore xs =
  case xs of
    "X" -> 1
    "Y" -> 2
    "Z" -> 3
    _   -> 0

-- +-------------+----------+----------+----------+
-- | Δ + 3 % 3   |   R=1    |   P=2    |   S=3    |
-- +-------------+----------+----------+----------+
-- | R'=1        | 0 (draw) | 1 (loss) | 2 (win)  |
-- | P'=2        | 2 (win)  | 0 (draw) | 1 (loss) |
-- | S'=3        | 1 (loss) | 2 (win)  | 0 (draw) |
-- +-------------+----------+----------+----------+
score :: Round -> Int
score (left, right) = right + outcome
  where
    outcome =
      case (left - right + 3) `mod` 3 of
        0 -> 3
        1 -> 0
        2 -> 6
        _ -> 0

-- +--------------------+----------+----------+----------+
-- | \ (R + L) % 3 + 1  |   L=1    |   L=2    |   L=3    |
-- +--------------------+----------+----------+----------+
-- | R = 1 (loss)       | 3 (loss) | 1 (loss) | 2 (loss) |
-- | R = 2 (draw)       | 1 (draw) | 2 (draw) | 3 (draw) |
-- | R = 3 (win)        | 2 (win)  | 3 (win)  | 1 (win)  |
-- +--------------------+----------+----------+----------+
update :: Round -> Round
update (left, right) = (left, (left + right) `mod` 3 + 1)
