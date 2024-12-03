import           AOCUtils        (readInt)
import           Data.Array      (Array, elems)
import           Data.List       (isInfixOf)
import           Data.List.Split (splitOn)
import           Text.Regex.TDFA (MatchText, getAllTextMatches, (=~))

parse :: String -> Array Int (MatchText String)
parse xs =
  getAllTextMatches (xs =~ "mul\\(([0-9]+,[0-9]+)\\)|do\\(\\)|don't\\(\\)")

instructions :: String -> [String]
instructions xs = map (head . map fst . elems) . elems $ parse xs

applyMul :: String -> Int
applyMul = product . map readInt . splitOn "," . (init . drop 4)

sanitize :: [String] -> [String]
sanitize xs =
  fst
    $ foldl
        (\(acc, check) ins ->
           case ins of
             "do()" -> (acc, True)
             "don't()" -> (acc, False)
             _ ->
               ( if check
                   then ins : acc
                   else acc
               , check))
        ([], True)
        xs

main :: IO ()
main = do
  input <- readFile "./src/2024/data/day03.txt"
  putStrLn "---Part 1 -------------"
  print . solve1 . instructions $ input
  putStrLn "---Part 2 -------------"
  print . solve2 . instructions $ input
  where
    solve = sum . map applyMul
    solve1 = solve . filter (isInfixOf "mul")
    solve2 = solve . sanitize
