import           AOCUtils        (readInt, readIntLast, replaceNth)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)

data Monkey =
  Monkey
    { idx     :: Int
    , items   :: [Integer]
    , operate :: Integer -> Integer
    , divisor :: Integer
    , next    :: Integer -> Int
    , count   :: Int
    }

instance Show Monkey where
  show (Monkey idx items _ _ _ count) =
    show "{Monkey " ++ show idx ++ " " ++ show items ++ " " ++ show count ++ "}"

main :: IO ()
main = do
  raw <- splitOn [""] . lines <$> readFile "./src/2022/data/day11.txt"
  putStrLn "---Part 1 -------------"
  let monkeys = parseDefinition (`div` 3) <$> raw
  let part1 = applyRounds 20 monkeys
  print $ product . take 2 . sortBy (flip compare) $ count <$> part1
  putStrLn "---Part 2 -------------"
  -- mod with the product of all
  -- divisors to ensure worry levels
  -- remain divisible by each divisor
  -- at any step. modulo could have been
  -- computed on parse 🐢
  let modulo = product $ divisor <$> monkeys
  let monkeys = parseDefinition (`mod` modulo) <$> raw
  let part2 = applyRounds 10000 monkeys
  print $ product . take 2 . sortBy (flip compare) $ count <$> part2

parseDefinition :: (Integer -> Integer) -> [String] -> Monkey
parseDefinition postOp def =
  Monkey
    { idx = readInt [last $ init $ head def]
    , items = read <$> (splitOn ", " . last $ splitOn ":" $ def !! 1)
    , operate = postOp . parseOp (def !! 2)
    , divisor = divisor
    , next = next
    , count = 0
    }
  where
    (next, divisor) = parseNext def

parseOp :: String -> (Integer -> Integer)
parseOp xs = operation
  where
    op:val:_ = drop 4 $ concatMap words $ splitOn ":" xs
    operation :: Integer -> Integer
    operation x =
      (case op of
         "+" -> (+)
         "*" -> (*)
         _   -> error "unknown op")
        (case val of
           "old" -> x
           _     -> read val)
        x

parseNext :: [String] -> (Integer -> Int, Integer)
parseNext xs = (next, divisor)
  where
    divisor = toInteger $ readIntLast $ xs !! 3
    truthy = readIntLast $ xs !! 4
    falsy = readIntLast $ xs !! 5
    next :: Integer -> Int
    next ys =
      if ys `rem` divisor == 0 && ys /= 0
        then truthy
        else falsy

popItem :: Monkey -> (Monkey, Integer)
popItem (Monkey id items op d next count) =
  (Monkey id (tail items) op d next (count + 1), head items)

pushItem :: Monkey -> Integer -> Monkey
pushItem (Monkey id items op d next count) item =
  Monkey id (item : items) op d next count

itemStep :: Monkey -> Integer -> (Int, Integer) -- (next, new worry)
itemStep monkey level = (nextMonkey, nextLevel)
  where
    nextLevel = operate monkey level
    nextMonkey = next monkey nextLevel

monkeyStep :: Monkey -> [Monkey] -> [Monkey]
monkeyStep (Monkey id [] op _ next _) monkeys = monkeys
monkeyStep monkey monkeys = monkeyStep monkey' monkeys'
  where
    (monkey', level) = popItem monkey
    (nextMonkey, nextLevel) = itemStep monkey level
    monkeys' =
      replaceNth nextMonkey (pushItem (monkeys !! nextMonkey) nextLevel) $
      replaceNth (idx monkey) monkey' monkeys

monkeyRound :: [Monkey] -> [Monkey]
monkeyRound ms = monkeyRound' [0 .. (length ms - 1)] ms
  where
    monkeyRound' :: [Int] -> [Monkey] -> [Monkey]
    monkeyRound' [] ms      = ms
    monkeyRound' (i:ids) ms = monkeyRound' ids (monkeyStep (ms !! i) ms)

applyRounds :: Int -> [Monkey] -> [Monkey]
applyRounds 0 ms = ms
applyRounds n ms = applyRounds (n - 1) (monkeyRound ms)
