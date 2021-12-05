{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Map                      as M
import           Data.Char                      ( isSpace )
import           Data.List                      ( groupBy
                                                , find
                                                )
import           Data.List.Split.Internals      ( splitOn )
import           Control.Monad.State            ( runState
                                                , MonadState(put, get)
                                                , State
                                                )

import           AOCUtils                       ( readInt
                                                , parseNums
                                                , groupByN
                                                )

main :: IO ()
main = do
  rawData <- lines <$> readFile "./src/2021/data/day04.txt"
  let nums   = parseNums $ head rawData
  let boards = toBingoState 5 $ tail rawData
  putStrLn "---Part 1 -------------"
  let bingo = runState (runGame nums) GameState { boards, result = Nothing }
  print $ computeScore $ fst bingo
  putStrLn "---Part 2 -------------"
  let bingo = runState (runGame2 nums) GameState { boards, result = Nothing }
  print $ computeScore $ fst bingo

-- Types --
type Board = M.Map Int ((Int, Int), Bool)
type AxisCounter = M.Map Int Int
type Bingo = Maybe Board
type BingoResult = (Bingo, Int)

data BoardState = BoardState
  { board :: Board
  , rows  :: AxisCounter
  , cols  :: AxisCounter
  , bingo :: Bool
  }
  deriving Show

data GameState = GameState
  { boards :: [BoardState]
  , result :: Bingo
  }
  deriving Show


-- Helpers --
createCoords :: (Int, Int) -> [(Int, Int)]
createCoords (n, m) = zip (take (n * n) $ cycle $ take n [0 ..])
                          (concatMap (replicate m) $ take m [0 ..])

createAxisCounter :: Int -> AxisCounter
createAxisCounter n = M.fromList $ zip [0 ..] (replicate n 0)

toBingoState :: Int -> [String] -> [BoardState]
toBingoState n xs =
  map
      ( (\board -> BoardState { board = board
                              , rows  = createAxisCounter n
                              , cols  = createAxisCounter n
                              , bingo = False
                              }
        )
      . M.fromList
      . flip zip (zip (createCoords (n, n)) (repeat False))
      . concatMap (map readInt . filter (not . all isSpace) . splitOn " ")
      )
    $ groupByN n (filter (not . all isSpace) xs)

-- Part 1 --
doBingoTurn :: Int -> BoardState -> BoardState
doBingoTurn n state@(BoardState board rows cols _) = BoardState b r c _bingo
 where
  b      = M.adjust (\(coords, _) -> (coords, True)) n board
  match  = M.lookup n board
  (r, c) = case match of
    Nothing          -> (rows, cols)
    Just ((x, y), _) -> (M.adjust (+ 1) y rows, M.adjust (+ 1) x cols)
  filterWinningBingoAxis = filter (\(_, count) -> count == 5) . M.toList
  fullRow                = filterWinningBingoAxis r
  fullCol                = filterWinningBingoAxis c
  _bingo                 = length fullRow == 1 || length fullCol == 1


isBoardBingo :: BoardState -> Bool
isBoardBingo (BoardState _ _ _ bingo) = bingo

findBingo :: [BoardState] -> Bingo
findBingo xs = (\s@(BoardState board _ _ _) -> board) <$> find isBoardBingo xs

computeScore :: BingoResult -> Maybe Int
computeScore (board, last) =
  (* last)
    .   foldl (\acc (val, _) -> val + acc) 0
    .   filter (\(_, (_, visited)) -> not visited)
    .   M.toList
    <$> board

runGame :: [Int] -> State GameState BingoResult

runGame [] = do
  return (Nothing, -1)
runGame (x : xs) = do
  GameState { boards, result } <- get
  let _boards = map (doBingoTurn x) boards
  let _bingo  = findBingo _boards
  put $ GameState { boards = _boards, result = _bingo }
  maybe (runGame xs) (return . (\b -> (Just b, x))) _bingo

-- Part 2 --
runGame2 :: [Int] -> State GameState BingoResult

runGame2 [] = do
  return (Nothing, -1)
runGame2 (x : xs) = do
  GameState { boards, result } <- get
  let _boards = map (doBingoTurn x) boards
  let rest = if length _boards == 1
        then _boards
        else filter (not . isBoardBingo) _boards
  let _bingo = findBingo rest
  put $ GameState { boards = rest, result = _bingo }
  maybe (runGame2 xs) (return . (\b -> (Just b, x))) _bingo

