import           Text.ParserCombinators.ReadP
import           Data.Char                      ( isDigit )


main :: IO ()
main = do
  instructions <- readP_to_S parseInstructions <$> readFile "./data/day08.txt"
  print $ programLoop $ ProgramState 0 0 0 (fst . head $ instructions) []

-- acc (+ | -)number
-- jmp (+ | -)number
-- nop +0

data Sign = Minus | Plus deriving (Show)
data Instruct = Acc Sign Int | Jmp Sign Int | Nop deriving  (Show)

data ProgramState = ProgramState
  { acc     :: Int
  , next    :: Int
  , prev    :: Int
  , prog    :: [Instruct]
  , visited :: [Int]
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
  "nop" -> Nop

getSign :: String -> Sign
getSign s = case s of
  "-" -> Minus
  "+" -> Plus

programLoop :: ProgramState -> ProgramState
programLoop state@(ProgramState acc next prev prog visited) =
  if next `elem` visited
    then ProgramState acc next prev [] []
    else programLoop $ evalInstruction state

evalInstruction :: ProgramState -> ProgramState
evalInstruction (ProgramState acc curr prev prog visited) =
  case prog !! curr of
    (Acc Plus  x) -> ProgramState (acc + x) (curr + 1) curr prog newVisited
    (Acc Minus x) -> ProgramState (acc - x) (curr + 1) curr prog newVisited
    (Jmp Plus  x) -> ProgramState acc (curr + x) curr prog newVisited
    (Jmp Minus x) -> ProgramState acc (curr - x) curr prog newVisited
    Nop           -> ProgramState acc (curr + 1) curr prog newVisited
  where newVisited = visited ++ [curr]
