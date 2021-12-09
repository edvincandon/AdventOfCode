{-# LANGUAGE LambdaCase #-}

import           AOCUtils                       ( readInt
                                                , toIndexedList
                                                , safeIndex1
                                                , safeIndex
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Char                      ( isSpace )
import           Data.List                      ( find
                                                , nubBy
                                                , sort
                                                )

main :: IO ()
main = do
  points <- parsePoints . lines <$> readFile "./src/2021/data/day09.txt"
  putStrLn "---Part 1 -------------"
  print $ getRiskLevel points
  putStrLn "---Part 2 -------------"
  print $ product . take 3 . reverse . sort $ length <$> getBasins points


-- Types --
type Coord = (Int, Int)

-- Helpers --
parsePoints :: [String] -> [[Int]]
parsePoints = map (map readInt . filter (not . all isSpace) . splitOn "")

getValueAtCoord :: [[Int]] -> Coord -> Maybe Int
getValueAtCoord xs (x, y) = safeIndex (safeIndex1 xs y []) x

toCoords :: [[Int]] -> [[(Int, Coord)]]
toCoords xs =
  map (\(row, y) -> map (\(val, x) -> (val, (x, y))) (toIndexedList row))
    $ toIndexedList xs

isLowPoint :: [[Int]] -> (Int, Coord) -> Bool
isLowPoint xs (val, (x, y)) = foldl
  (\acc coord ->
    acc
      && (\case
           Just _val -> val < _val
           Nothing   -> True
         )
           (getValueAtCoord xs coord)
  )
  True
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

ensureUniqueCoords :: [(Int, Coord)] -> [(Int, Coord)]
ensureUniqueCoords = nubBy (\(_, c1) (_, c2) -> c1 == c2)

-- Part 1 --
getRiskLevel :: [[Int]] -> Int
getRiskLevel = sum . map ((+ 1) . fst) . getLowPoints

getLowPoints :: [[Int]] -> [(Int, Coord)]
getLowPoints xs = concat (filter (isLowPoint xs) <$> toCoords xs)

-- Part 2 --
matchBasinPoints :: [[Int]] -> (Int, Coord) -> [(Int, Coord)]
matchBasinPoints xs (val, (x, y))
  | null match
  = [(val, (x, y))]
  | otherwise
  = ensureUniqueCoords $ (val, (x, y)) : concatMap (matchBasinPoints xs) match
 where
  match = foldl
    (\acc coord ->
      acc
        ++ (\case
             Just _val -> [ (_val, coord) | val < _val && _val /= 9 ]
             Nothing   -> []
           )
             (getValueAtCoord xs coord)
    )
    []
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getBasins :: [[Int]] -> [[(Int, Coord)]]
getBasins xs = map (matchBasinPoints xs) (getLowPoints xs)
