import           Data.List.Split                ( splitOn
                                                , chunksOf
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , fromJust
                                                , isJust
                                                )
import           AOCUtils                       ( readInt
                                                , toIndexedList
                                                )
import           Data.Char                      ( isSpace )
import           Data.Bifunctor                 ( first )
import           Data.List                      ( find )

main :: IO ()
main = do
  state <- parseState <$> readFile "./src/2021/data/day11.txt"
  putStrLn "---Part 1-------------"
  let (newState, count) = computeTotalFlashesAfterNSteps 100 state
  putStrLn "simulation after 100 steps"
  putStrLn $ "total flashes : " ++ show count
  printLevel newState
  putStrLn "---Part 2-------------"
  let (allState, iter) = simulateUntilAllFlash state 0
  putStrLn $ "all flashes after iteration : " ++ show iter
  printLevel allState


-- Types --
type Levels = [Int]
type State = (Levels, (Int, Int))

-- Helpers --
parseState :: String -> State
parseState xs = (concat parsed, (length $ head parsed, length parsed))
 where
  parsed = map readInt . filter (not . all isSpace) . splitOn "" <$> lines xs

printLevel :: State -> IO ()
printLevel (level, (x, _)) =
  putStrLn $ concatMap (("\n" ++) . show) (chunksOf x level) ++ "\n"

getKernel :: Int -> (Int, Int) -> [Int]
getKernel pos (maxX, maxY) = map (\(xx, yy) -> maxY * yy + xx) $ filter
  (\(x, y) -> x >= 0 && y >= 0 && x < maxX && y < maxY)
  [ (x    , y + 1)
  , (x    , y - 1)
  , (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]
 where
  x = pos `mod` maxX
  y = pos `div` maxY

applyKernel :: State -> (Int, Int) -> Levels
applyKernel (levels, max) (_, idx) =
  map
      (\(v, i) -> if (i == idx) || (v == 0)
        then 0
        else v + (if i `elem` kernels then 1 else 0)
      )
    $ toIndexedList levels
  where kernels = getKernel idx max

  -- add transformaiton function on end --
applyKernels :: ((Int, Int) -> Bool) -> State -> State
applyKernels predicate (levels, max) = if isJust $ find (> 9) pl
  then applyKernels ((> 9) . fst) (pl, max)
  else (pl, max)
 where
  pl = foldl
    (\acc curr -> if predicate curr then applyKernel (acc, max) curr else acc)
    levels
    (toIndexedList levels)

step :: State -> State
step (levels, m) = applyKernels ((0 ==) . fst)
                                (map (applyBounds . (1 +)) levels, m)
 where
  applyBounds :: Int -> Int
  applyBounds x = if x > 9 then 0 else x

countFlashes :: State -> Int
countFlashes = length . filter (0 ==) . fst

-- Part 1 --
computeTotalFlashesAfterNSteps :: Int -> State -> (State, Int)
computeTotalFlashesAfterNSteps n state = foldl
  (\acc _ -> (\newState -> (newState, snd acc + countFlashes newState))
    $ step (fst acc)
  )
  (state, countFlashes state)
  [0 .. (n - 1)]


-- Part 2 --
simulateUntilAllFlash :: State -> Int -> (State, Int)
simulateUntilAllFlash state n = if stop
  then (newState, n + 1)
  else simulateUntilAllFlash newState (n + 1)
 where
  newState = step state
  stop     = sum (fst newState) == 0
