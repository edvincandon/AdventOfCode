import           AOCUtils        (readInt)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Coord3D = (Int, Int, Int)
type Bounds = (Coord3D, Coord3D)

main :: IO ()
main = do
  cubes <- map parseCoord3D . lines <$> readFile "./src/2022/data/day18.txt"
  putStrLn "---Part 1 -------------"
  print $ length [s | c <- cubes, s <- neighbours c, s `notElem` cubes]
  putStrLn "---Part 2 -------------"
  let bounds@(minBound, _) = getBounds cubes
  let outerfill = Set.toList $ floodfill minBound cubes bounds
  print $
    length
      [ s
      | c <- cubes
      , s <- neighbours c
      , s `elem` outerfill && s `notElem` cubes
      ]

parseCoord3D :: String -> Coord3D
parseCoord3D xs =
  let (x:y:z:_) = readInt <$> splitOn "," xs
   in (x, y, z)

neighbours :: Coord3D -> [Coord3D]
neighbours (x, y, z) =
  [ (x - 1, y, z)
  , (x + 1, y, z)
  , (x, y - 1, z)
  , (x, y + 1, z)
  , (x, y, z - 1)
  , (x, y, z + 1)
  ]

-- get the bounding cube from
-- from the droplet coordinates
getBounds :: [Coord3D] -> Bounds
getBounds cubes =
  let low = (minimum xs - 1, minimum ys - 1, minimum zs - 1)
      high = (maximum xs + 1, maximum ys + 1, maximum zs + 1)
   in (low, high)
  where
    xs = (\(x, _, _) -> x) <$> cubes
    ys = (\(_, y, _) -> y) <$> cubes
    zs = (\(_, _, z) -> z) <$> cubes

inBounds :: Coord3D -> (Coord3D, Coord3D) -> Bool
inBounds (x, y, z) ((xMin, yMin, zMin), (xMax, yMax, zMax)) =
  xMin <= x && x <= xMax && yMin <= y && y <= yMax && zMin <= z && z <= zMax

floodfill :: Coord3D -> [Coord3D] -> Bounds -> Set Coord3D
floodfill coord cubes bounds = fill' cubes bounds Set.empty coord
  where
    fill' :: [Coord3D] -> Bounds -> Set Coord3D -> Coord3D -> Set Coord3D
    fill' cubes bounds visited coord
      | Set.member coord visited = visited -- already visiterd
      | not $ inBounds coord bounds = visited -- out of bounds
      | coord `elem` cubes = visited' -- matched a cube
      | otherwise = foldl (fill' cubes bounds) visited' (neighbours coord)
      where
        visited' = Set.insert coord visited
