import           Data.List                      ( sortBy )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                )

readInt :: String -> Char
readInt = read

type Round = (String, String)


main :: IO ()
main = do
  rounds <- map (toRound . words) . lines <$> readFile
    "./src/2022/data/day02.txt"
  putStrLn "---Part 1 -------------"
  print $ roundScore $ catMaybes rounds
  putStrLn "---Part 2 -------------"
  print $ roundScore $ updateChoice <$> catMaybes rounds


toRound :: [String] -> Maybe Round
toRound (xs : ys : _) = Just (xs, ys)
toRound []            = Nothing
toRound (_ : _)       = Nothing


choiceScore :: Round -> Int
choiceScore (xs, ys) = choice
 where
  choice = case ys of
    "X" -> 1
    "Y" -> 2
    "Z" -> 3
    _   -> 0

outcomeScore :: Round -> Int
outcomeScore (xs, "X") = case xs of
  "A" -> 3
  "B" -> 0
  "C" -> 6
  _   -> 0
outcomeScore (xs, "Y") = case xs of
  "A" -> 6
  "B" -> 3
  "C" -> 0
  _   -> 0
outcomeScore (xs, "Z") = case xs of
  "A" -> 0
  "B" -> 6
  "C" -> 3
  _   -> 0
outcomeScore (_, _) = 0

roundScore :: [Round] -> Int
roundScore xs = sum $ (\x -> outcomeScore x + choiceScore x) <$> xs

draw :: String -> Maybe String
draw "A" = Just "X"
draw "B" = Just "Y"
draw "C" = Just "Z"
draw _   = Nothing

win :: String -> Maybe String
win "A" = Just "Y"
win "B" = Just "Z"
win "C" = Just "X"
win _   = Nothing

loss :: String -> Maybe String
loss "A" = Just "Z"
loss "B" = Just "X"
loss "C" = Just "Y"
loss _   = Nothing

updateChoice :: Round -> Round
updateChoice (xs, ys) = (xs, update)
 where
  update = fromMaybe
    ys
    (case ys of
      "X" -> loss xs
      "Y" -> draw xs
      "Z" -> win xs
      _   -> Nothing
    )
