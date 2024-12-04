import           AOCUtils  (Coord, allDirs, elemAtCoord, index2D, safeIndex,
                            translate)
import           Data.List (isInfixOf)

type Dir = Coord
type Item = (Char, Coord)
type Grid = [[Item]]

diags :: [[Dir]]
diags = [[(-1, -1), (0, 0), (1, 1)], [(-1, 1), (0, 0), (1, -1)]]

getDiags :: Grid -> Coord -> Maybe [String]
getDiags grid coord = traverse getDiag diags
  where
    getDiag :: [Dir] -> Maybe String
    getDiag = (map fst <$>) . traverse (elemAtCoord grid . translate coord)

match :: Grid -> String -> Coord -> Dir -> Bool
match _ "XMAS" _ _ = True
match grid curr (x, y) (dx, dy) =
  isInfixOf curr "XMAS"
    && (case elemAtCoord grid (x + dx, y + dy) of
          Just (char, coord') -> match grid (curr ++ [char]) coord' (dx, dy)
          Nothing             -> False)

match' :: Grid -> Coord -> Bool
match' grid coord = matches (getDiags grid coord)
  where
    matches (Just m) = all valid m
    matches _        = False
    valid = (||) <$> (== "MAS") <*> (== "SAM")

count :: Grid -> Item -> Int
count grid (char, coord) =
  if char == 'X'
    then length . filter id $ map (match grid [char] coord) allDirs
    else 0

count' :: Grid -> Item -> Int
count' grid (char, coord) =
  if char == 'A' && match' grid coord
    then 1
    else 0

occurences :: (Grid -> Item -> Int) -> Grid -> Int
occurences matchFn grid =
  foldl (\total item -> total + matchFn grid item) 0 $ concat grid

main :: IO ()
main = do
  input <- readFile "./src/2024/data/day04.txt"
  putStrLn "---Part 1 -------------"
  print . solve $ input
  putStrLn "---Part 2 -------------"
  print . solve' $ input
  where
    parse = index2D . lines
    solve = occurences count . parse
    solve' = occurences count' . parse
