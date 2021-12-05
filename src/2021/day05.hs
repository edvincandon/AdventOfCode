import           AOCUtils                       ( readInt
                                                , parseNums
                                                , minUniqueTuples
                                                )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( group
                                                , groupBy
                                                , sort
                                                , sortOn
                                                )

main :: IO ()
main = do
  rawData <- lines <$> readFile "./src/2021/data/day05.txt"
  putStrLn "---Part 1 -------------"
  print $ computeOverlapCount False $ parseCoords rawData
  putStrLn "---Part 2 -------------"
  print $ computeOverlapCount True $ parseCoords rawData

-- Types --
type Coord = (Int, Int)
type Segment = (Coord, Coord)

-- Helpers --
parseCoords :: [String] -> [Segment]
parseCoords =
  map
    $ (\x -> (head x, head $ tail x))
    . sortOn fst -- sort on min x value to simplify during segment interpolation
    . map ((\x -> (head x, head $ tail x)) . map readInt . splitOn ",")
    . splitOn " -> "

keepPerps :: [Segment] -> [Segment]
keepPerps = filter (\(a, b) -> fst a == fst b || snd a == snd b)

interpolateSegment :: Segment -> [Coord]
interpolateSegment ((x1, y1), (x2, y2))
  | x1 == x2  = zip (replicate dy x1) [min y1 y2 ..]
  | y1 == y2  = zip [min x1 x2 ..] (replicate dx y1)
  | otherwise = interpolateDiag ((x1, y1), (x2, y2))
 where
  dx = abs (x2 - x1) + 1
  dy = abs (y2 - y1) + 1

interpolateDiag :: Segment -> [Coord]
interpolateDiag ((minX, y1), (maxX, y2)) = zip [minX ..]
  $ (if y1 < y2 then id else reverse) [minY .. maxY]
 where
  minY = min y1 y2
  maxY = max y1 y2

-- Part 1/2 --
computeOverlapCount :: Bool -> [Segment] -> Int
computeOverlapCount diag xs =
  length
    $ minUniqueTuples 2
    $ concatMap interpolateSegment
    $ (if diag then id else keepPerps) xs
