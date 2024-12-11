import           AOCUtils        (Coord, (.+), (.-))
import           Data.List       (find, inits, tails)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as S

type Point = (Coord, Char)
type Map = ([Point], (Int, Int))

parseMap :: String -> Map
parseMap xs =
  let rows = lines xs
      width = length rows
      height = length $ head rows
   in ( [ ((x, y), freq)
        | (row, y) <- zip rows [0 ..]
        , (freq, x) <- zip row [0 ..]
        , freq /= '.'
        ]
      , (width, height))

solve :: Map -> (Coord -> Coord -> [Coord]) -> Int
solve (xs, (w, h)) getAntinodes =
  S.size
    $ S.fromList
        [ antinode
        | (a, fa) <- xs
        , (b, fb) <- xs
        , a /= b && fa == fb
        , antinode <-
            takeWhile (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h)
              $ getAntinodes a b
        ]

main :: IO ()
main = do
  nodes <- parseMap <$> readFile "./src/2024/data/day08.txt"
  putStrLn "---Part 1 -------------"
  print $ solve nodes (\a b -> [b .+ (b .- a)])
  putStrLn "---Part 2 -------------"
  print $ solve nodes (\a b -> iterate ((b .- a) .+) b)
