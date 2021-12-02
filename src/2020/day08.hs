import           Text.ParserCombinators.ReadP
import           Data.Char                      ( isDigit )


main :: IO ()
main = do
  instructions <- readP_to_S parseInstructions
    <$> readFile "./src/2020/data/day08.txt"
  print "PART #1"
  print $ programLoop $ ProgramState 0 0 (fst . head $ instructions) [] False
  print "PART #2"
  print $ backTrack (ProgramState 0 0 (fst . head $ instructions) [] False, 0)

-- acc (+ | -)number
-- jmp (+ | -)number
-- nop +0

data Sign = Minus | Plus deriving (Show)
data Instruct = Acc Sign Int | Jmp Sign Int | Nop Sign Int deriving  (Show)

data ProgramState = ProgramState
  { acc        :: Int
  , next       :: Int
  , prog       :: [Instruct]
  , visited    :: [Int]
  , terminated :: Bool
  }
  deriving Show

parseInstruction :: ReadP Instruct
parseInstruction = do
  instruct <- choice [string "acc", string "jmp", string "nop"]
  skipSpaces
  sign <- count 1 $ choice [char '-', char '+']
  num  <- many1 $ satisfy isDigit
  return $ getInstruct instruct (getSign sign) (read num)

parseInstructions :: ReadP [Instruct]
parseInstructions = do
  xs <- endBy1 parseInstruction $ char '\n'
  eof
  return xs

getInstruct :: String -> Sign -> Int -> Instruct
getInstruct ins sign num = case ins of
  "acc" -> Acc sign num
  "jmp" -> Jmp sign num
  "nop" -> Nop sign num
  _     -> error "invalid instruct"

getSign :: String -> Sign
getSign s = case s of
  "-" -> Minus
  "+" -> Plus
  _   -> error "invalid sign"

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b) where (a, _ : b) = splitAt n ls

programLoop :: ProgramState -> ProgramState
programLoop state@(ProgramState acc next prog visited _) =
  if nextVisited || terminated
    then ProgramState acc next [] [] terminated
    else programLoop $ evalInstruction state
 where
  nextVisited = next `elem` visited
  terminated  = next == length prog - 1


evalInstruction :: ProgramState -> ProgramState
evalInstruction (ProgramState acc curr prog visited _) = case prog !! curr of
  (Acc Plus  x) -> ProgramState (acc + x) (curr + 1) prog newVisited False
  (Acc Minus x) -> ProgramState (acc - x) (curr + 1) prog newVisited False
  (Jmp Plus  x) -> ProgramState acc (curr + x) prog newVisited False
  (Jmp Minus x) -> ProgramState acc (curr - x) prog newVisited False
  (Nop _     _) -> ProgramState acc (curr + 1) prog newVisited False
  where newVisited = visited ++ [curr]

changeProgramInstruct :: ProgramState -> Int -> ProgramState
changeProgramInstruct state@(ProgramState _ _ prog visited terminated) idx =
  ProgramState 0 0 (replaceAtIndex idx newInstruct prog) [] False
 where
  newInstruct = case prog !! idx of
    (Acc s x) -> Acc s x
    (Jmp s x) -> Nop s x
    (Nop s x) -> Jmp s x

backTrack :: (ProgramState, Int) -> (ProgramState, Int)
backTrack (state, idx) = if not terminated
  then backTrack (state, idx + 1)
  else (test, idx)
 where
  test@(ProgramState _ _ prog _ terminated) =
    programLoop $ changeProgramInstruct state idx

