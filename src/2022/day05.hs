import           AOCUtils        (popMultiple, pushMultiple, readInt,
                                  replaceNth, toIndexedList)
import           Data.Char       (isAlpha)
import           Data.List.Split (chunksOf, splitWhen, wordsBy)
import           Data.Maybe      (fromJust)
import           Data.Stack      (Stack, stackNew, stackPeek, stackPop,
                                  stackPush)

type Instruction = (Int, Int, Int) -- (Move n, From, To)

type CrateElement = (Int, Char) -- (Stack index, Value)

type CrateState = [Stack Char]

main :: IO ()
main = do
  (state, instructions) <-
    parseInput . lines <$> readFile "./src/2022/data/day05.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  print $
    fromJust . stackPeek <$>
    foldl (flip applyInstruction9000) state instructions
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  print $
    fromJust . stackPeek <$>
    foldl (flip applyInstruction9001) state instructions

-- Crate stack utils --
pushAtStack :: Int -> Char -> CrateState -> CrateState
pushAtStack n xs stacks = replaceNth n (stackPush (stacks !! n) xs) stacks

parseInput :: [String] -> (CrateState, [Instruction])
parseInput input = (stacks, instructions)
  where
    (xs:ys:_) = splitWhen null input
    crates = reverse $ concatMap parseCratesLine (init xs)
    instructions = parseInstructions <$> ys
    n = readInt $ last (words $ last xs)
    stacks =
      foldl
        (\_stacks crate ->
           case crate of
             Just (idx, crateId) -> pushAtStack idx crateId _stacks
             Nothing             -> _stacks)
        (replicate n (stackNew :: Stack Char))
        crates

parseCratesLine :: [Char] -> [Maybe CrateElement]
parseCratesLine xs = crates
  where
    crates =
      (\(crate, idx) ->
         if null $ (unwords . words) crate
           then Nothing
           else Just (idx, crate !! 1)) <$>
      toIndexedList (chunksOf 4 xs)

parseInstructions :: String -> Instruction
parseInstructions xs = (\(n:from:to:_) -> (n, from - 1, to - 1)) s
  where
    delims = ["move", "from", "to"]
    s = readInt <$> wordsBy isAlpha xs

applyInstruction9000 :: Instruction -> CrateState -> CrateState
applyInstruction9000 (0, _, _) stacks = stacks
applyInstruction9000 (n, from, to) stacks = _stacks
  where
    (source, crateId) = fromJust $ stackPop $ stacks !! from
    _stacks =
      applyInstruction9000 (n - 1, from, to) $
      pushAtStack to crateId (replaceNth from source stacks)

applyInstruction9001 :: Instruction -> CrateState -> CrateState
applyInstruction9001 (0, _, _) stacks = stacks
applyInstruction9001 (n, from, to) stacks = _stacks
  where
    (source, crateIds) = fromJust $ popMultiple n (stacks !! from)
    target = pushMultiple (stacks !! to) crateIds
    _stacks = replaceNth to target (replaceNth from source stacks)
