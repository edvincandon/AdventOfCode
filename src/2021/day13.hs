import           Data.List.Split                ( splitOn )
import           Data.Char                      ( isSpace )
import           AOCUtils                       ( readInt )
import           Data.List                      ( find
                                                , nub
                                                )
import           Data.Bifunctor                 ( second
                                                , first
                                                )
main :: IO ()
main = do
  page <- lines <$> readFile "./src/2021/data/day13.txt"
  let (coords, instruct) = parseInstructions page
  putStrLn "---Part 1-------------\n"
  let firstPass = foldIt (coords, getBounds coords) $ head instruct
  putStrLn $ "Total points : " ++ show (length $ fst firstPass)
  putStrLn "\n---Part 2-------------"
  printCoords $ foldl foldIt (coords, getBounds coords) instruct


-- Types --
type Coords = (Int, Int)

-- Helpers --
parseInstructions :: [String] -> ([Coords], [Coords])
parseInstructions xs = (coords, instruct)
 where
  s      = span (/= "") xs
  coords = map ((\(x : y : _) -> (x, y)) . map readInt . splitOn ",") $ fst s
  instruct =
    map
        ( (\(inst : val : _) ->
            if last inst == 'x' then (readInt val, 0) else (0, readInt val)
          )
        . splitOn "="
        )
      $ filter (not . all isSpace)
      $ snd s

printCoords :: ([Coords], Coords) -> IO ()
printCoords (coords, (maxX, maxY)) = do
  putStrLn $ "\n" ++ concatMap (\x -> concat x ++ "\n") res ++ "\n"
 where
  res = map
    ( zipWith (\x y -> maybe "." (const "#") $ find (== (x, y)) coords)
              [0 .. maxX]
    . repeat
    )
    [0 .. maxY]

getBounds :: [Coords] -> (Int, Int)
getBounds coords = (maxX, maxY)
 where
  maxX = maximum $ map fst coords
  maxY = maximum $ map snd coords

foldIt :: ([Coords], Coords) -> Coords -> ([Coords], Coords)
foldIt (coords, bounds) line = res
 where
  target    = if fst line == 0 then snd else fst
  transform = if fst line == 0 then second else first
  foldAt    = target line
  res =
    ( nub $ map
      (\coord -> if target coord > foldAt
        then transform (\val -> foldAt - (val - (target bounds - foldAt))) coord
        else coord
      )
      coords
    , transform (const (foldAt - 1)) bounds
    )

