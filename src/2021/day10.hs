import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , fromJust
                                                )
import           Data.List                      ( find
                                                , sort
                                                )

main :: IO ()
main = do
  code <- lines <$> readFile "./src/2021/data/day10.txt"
  putStrLn "---Part 1 -------------"
  print $ sum $ getSyntaxErrorScore . findFirstIncorrect <$> code
  putStrLn "---Part 2 -------------"
  print $ getMiddleScore code

-- Helpers --
openCase :: [String]
openCase = ["<", "(", "[", "{"]

closeCase :: [String]
closeCase = [">", ")", "]", "}"]

bestCases :: [String]
bestCases = zipWith (++) openCase closeCase

closing :: String -> Maybe String
closing xs = case xs of
  "<" -> Just ">"
  "(" -> Just ")"
  "[" -> Just "]"
  "{" -> Just "}"
  _   -> Nothing

isOpenCase :: String -> Bool
isOpenCase = flip elem openCase

isCloseCase :: String -> Bool
isCloseCase = flip elem closeCase

isValid :: String -> String -> Bool
isValid a b = b == fromMaybe "" (closing a)

isIncomplete :: String -> Bool
isIncomplete = isNothing . findFirstIncorrect

clearValid :: String -> String
clearValid xs = if length res > 1 then clearValid $ concat res else concat res
  where res = foldl (\acc curr -> concatMap (splitOn curr) acc) [xs] bestCases

-- Part 1 --
findFirstIncorrect :: String -> Maybe (Char, Char)
findFirstIncorrect xs = incorrect
 where
  sanitized = clearValid xs
  incorrect =
    find
        (\(prev, next) -> isOpenCase [prev] && isCloseCase [next] && not
          (isValid [prev] [next])
        )
      $ zip sanitized (tail sanitized)

getSyntaxErrorScore :: Maybe (Char, Char) -> Int
getSyntaxErrorScore xs = case xs of
  Nothing -> 0
  Just (_, err) ->
    (case err of
      '>' -> 25137
      '}' -> 1197
      ']' -> 57
      ')' -> 3
      _   -> 0
    )

-- Part 2 --
completePattern :: String -> String
completePattern xs =
  concatMap ((fromJust . closing) . (: [])) (reverse (clearValid xs))

getAutocompleteScore :: String -> Int
getAutocompleteScore = foldl (\acc curr -> 5 * acc + getCharScore curr) 0
 where
  getCharScore :: Char -> Int
  getCharScore xs = case xs of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> 0

getMiddleScore :: [String] -> Int
getMiddleScore xs = sortedScores !! idx
 where
  sortedScores =
    sort
      $   getAutocompleteScore
      .   completePattern
      .   clearValid
      <$> filter isIncomplete xs
  idx = length sortedScores `div` 2
