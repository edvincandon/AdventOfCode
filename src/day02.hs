import           Data.List.Split                ( splitOn )

main = do
  contents <- readFile "./data/day02.txt"
  print . length . filter parseLine' . lines $ contents

-- day02 - A
-- parseLine :: String -> Bool
-- parseLine xs = lenTarget >= min && lenTarget <= max
--   where w = words xs
--         [min, max] = map read (splitOn "-" (head w))
--         [target] = head (splitOn ":" (w !! 1))
--         lenTarget = foldr (\a b -> if a == target then b + 1 else b) 0 (last w)

-- day02 - B
parseLine' :: String -> Bool
parseLine' xs =
  (pw !! min == target && pw !! max /= target)
    || (pw !! max == target && pw !! min /= target)
 where
  w          = words xs
  [min, max] = map (\x -> read x - 1) $ splitOn "-" (head w)
  [target]   = head $ splitOn ":" (w !! 1)
  pw         = last w



