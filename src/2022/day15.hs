import           Data.Bifunctor  (first)
import qualified Data.Foldable   as Set
import           Data.List.Split (splitOneOf)
import           Data.Maybe      (catMaybes)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Text.Read       (readMaybe)

type Coord = (Int, Int)
type Beacon = Coord
type Sensor = Coord
type Definition = (Sensor, Beacon, Int)

main :: IO ()
main = do
  items <- fmap parse . lines <$> readFile "./src/2022/data/day15.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  print $ countCandidates items 2000000
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  print $ uncurry (+) (first (* 4000000) $ findDistress items)

parse :: String -> Definition
parse xs = (p0, p1, manhattan p0 p1)
  where
    parts :: [Maybe Int]
    parts = readMaybe <$> splitOneOf "=,;:" xs
    coords = catMaybes parts
    p0 = (head coords, coords !! 1)
    p1 = (coords !! 2, last coords)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

intersectBoundsAt :: Definition -> Int -> Set Coord
intersectBoundsAt xs@((x0, y0), _, dist) y =
  if dy > dist
    then Set.empty
    else Set.singleton (x0 - dx, x0 + dx)
  where
    dy = abs (y0 - y)
    dx = dist - dy

intervalsAt :: [Definition] -> Int -> Set Coord
intervalsAt items at =
  foldl
    (\set item -> Set.union set (intersectBoundsAt item at))
    (Set.empty :: Set Coord)
    items

-- assume sorted intervals
gap :: [Coord] -> Int -> Maybe Int
gap [] x = Just x
gap [(xMin, xMax)] x =
  if x < xMin
    then Just x
    else Nothing
gap (b0@(xMin, xMax):b1@(xMin', xMax'):rest) x
  | x < xMin = Just x
  | b0 == b1 = gap (b1 : rest) x
  | xMin' <= xMax && xMax' <= xMax = gap (b0 : rest) x
  | xMax >= xMin' = gap ((xMin, max xMax xMax') : rest) x
  | otherwise = gap rest (max x (xMax + 1))

countCandidates :: [Definition] -> Int -> Int
countCandidates items y = 1 + max - min - length beacons
  where
    beacons =
      Set.fromList $
      (\(_, p1, _) -> p1) <$> filter (\(_, (_, y1), _) -> y1 == y) items
    intersects = intervalsAt items y
    min = fst $ Set.minimum intersects
    max = snd $ Set.maximum intersects

findDistress :: [Definition] -> Beacon
findDistress items =
  head
    [ (x, y)
    | y <- [0 .. 4000000]
    , Just x <- [gap (Set.toAscList $ intervalsAt items y) 0]
    ]
