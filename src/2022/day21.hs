import           AOCUtils        (readInt)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import           Text.Read       (readMaybe)

main :: IO ()
main = do
  input <- lines <$> readFile "./src/2022/data/day21.txt"
  let nodes = parse input
  putStrLn "---Part 1 -------------"
  print $ solve id nodes
  putStrLn "---Part 2 -------------"
  print $ binarySolve nodes

data Node
  = Value String Int
  | Add String Node Node
  | Sub String Node Node
  | Mult String Node Node
  | Div String Node Node
  deriving (Show)

parse :: [String] -> Node
parse xs = branch "root"
  where
    map = M.fromList $ (\(id:value:_) -> (id, value)) . splitOn ": " <$> xs
    branch :: String -> Node
    branch id =
      case res of
        Just val -> Value id val
        _ ->
          case op of
            "+" -> Add id (branch node1) (branch node2)
            "-" -> Sub id (branch node1) (branch node2)
            "*" -> Mult id (branch node1) (branch node2)
            "/" -> Div id (branch node1) (branch node2)
            _   -> error "unprocessable operation"
      where
        value = map ! id
        res = readMaybe value
        [node1, op, node2] = words value

solve :: (Node -> Node) -> Node -> Int
solve m node =
  case m node of
    Value _ x  -> x
    Add _ a b  -> solve m a + solve m b
    Sub _ a b  -> solve m a - solve m b
    Mult _ a b -> solve m a * solve m b
    Div _ a b  -> solve m a `div` solve m b

replace :: Int -> Node -> Node
replace val node =
  case node of
    (Value "humn" _) -> Value "humn" val
    xs               -> xs

-- treat the root node as an equality statement
-- binary search assumes result is monotonic with
-- respect to the searched value
binarySolve :: Node -> Int
binarySolve n@(Add _ a b) = search (0, 99999999999999)
  where
    search :: (Int, Int) -> Int
    search bounds@(min, max)
      | a' == b' = mid
      | otherwise =
        search
          (if a' > b'
             then (mid + 1, max)
             else (min, mid))
      where
        mid = (min + max) `div` 2
        a' = solve (replace mid) a
        b' = solve (replace mid) b
binarySolve _ = error "invalid root node"
