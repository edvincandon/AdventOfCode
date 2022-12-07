-- the parsing is completely overkill, I thought
-- Part 2 would have introduced CMD changes so I
-- went too far with the parsing - Also maybe could
-- have been resolved using a tree structure..
import           AOCUtils        (readInt)
import           Data.List.Split (chunksOf, split, whenElt)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)

data Cmd
  = CD String
  | LS [Stdout]
  deriving (Show)

data Stdout
  = Dir String
  | File Int
  deriving (Show)

main :: IO ()
main = do
  output <- lines <$> readFile "./src/2022/data/day07.txt"
  let dirs = dirSizes $ parseCommands output
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  print $ sum $ snd <$> Map.toList (Map.filter (<= 100000) dirs)
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  let min = fromJust (Map.lookup "/" dirs) - (70000000 - 30000000)
  print $ minimum $ snd <$> Map.toList (Map.filter (>= min) dirs)

parseCommands :: [String] -> [Cmd]
parseCommands xs =
  (\(cmd:param, output) ->
     case cmd of
       "cd" -> CD $ head param
       "ls" ->
         LS
           ((\out ->
               case head $ words out of
                 "dir" -> Dir (drop 4 out)
                 val   -> File $ readInt val) <$>
            output)
       _ -> error "invalid command") <$>
  cmds
  where
    chunks = chunksOf 2 $ tail $ split (whenElt (\a -> head a == '$')) xs
    cmds = (\((cmd:_):b:_) -> (tail $ words cmd, b)) <$> chunks

dirSizes :: [Cmd] -> Map String Int
dirSizes =
  fst .
  foldl
    (\(dirs, pwd) cmd ->
       case cmd of
         CD dir ->
           ( dirs
           , case dir of
               "/"  -> [dir]
               ".." -> init pwd
               _    -> pwd ++ [dir])
         LS stdout ->
           foldl
             (\(_dirs, paths) out ->
                case out of
                  Dir _ -> (_dirs, pwd)
                  File size ->
                    ( foldl
                        (\m p -> Map.insertWith (+) p size m)
                        _dirs
                        (distinctDirs paths)
                    , pwd))
             (dirs, pwd)
             stdout)
    (Map.empty :: Map String Int, [])

-- handle same dir name at different
-- depths : AoC you bastards..
distinctDirs :: [String] -> [String]
distinctDirs xs =
  zipWith
    (\i str ->
       concatMap
         (++ (if str == "/"
                then ""
                else "/")) $
       take i xs)
    [1 ..]
    xs
