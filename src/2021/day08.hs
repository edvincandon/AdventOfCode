
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import           Data.List.Split                ( splitOn )
import           Data.Bifunctor                 ( bimap
                                                , Bifunctor(first, second)
                                                )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.List                      ( sort
                                                , (\\)
                                                , intercalate
                                                , find
                                                , nub
                                                )
import qualified Data.Map                      as M
import           AOCUtils                       ( readInt )

main :: IO ()
main = do
  signals <- map readSignalData . lines <$> readFile "./src/2021/data/day08.txt"
  putStrLn "---Part 1 -------------"
  print $ countUniqueSignals signals snd
  putStrLn "---Part 2 -------------"
  print $ sum $ map decodeLine signals

digitMap :: M.Map String Char
digitMap = M.fromList
  [ ("abcefg" , '0')
  , ("cf"     , '1')
  , ("acdeg"  , '2')
  , ("acdfg"  , '3')
  , ("bcdf"   , '4')
  , ("abdfg"  , '5')
  , ("abdefg" , '6')
  , ("acf"    , '7')
  , ("abcdefg", '8')
  , ("abcdfg" , '9')
  ]


-- Types --
type Signal = [String]
type SignalIO = (Signal, Signal)

-- Helpers --
readSignalData :: String -> SignalIO
readSignalData xs = bimap parse parse (head io, last io)
 where
  io    = splitOn " | " xs
  parse = map sort <$> splitOn " "

parseUniqueSignals :: String -> (Maybe Int, String)
parseUniqueSignals xs =
  ( case len of
    2 -> Just 1
    3 -> Just 7
    4 -> Just 4
    7 -> Just 8
    _ -> Nothing
  , xs
  )
  where len = length xs

getUniqueSignals :: Signal -> [(Int, String)]
getUniqueSignals xs = map (first fromJust) match
  where match = filter (isJust . fst) (map parseUniqueSignals xs)

groupCount :: [String] -> [(Char, Int)]
groupCount xs = M.toList $ foldl (flip (M.adjust (+ 1))) counts $ intercalate
  ""
  xs
  where counts = M.fromList $ zip ['a' .. 'g'] $ repeat 0

-- Part 1 --
countUniqueSignals :: [SignalIO] -> (SignalIO -> Signal) -> Int
countUniqueSignals xs selector =
  length $ concatMap (getUniqueSignals . selector) xs


-- Part 2 --

-- Imperative algo
-- 1. diff (1) and (7) strings to infer <a> value
-- 2. find letter with 9 occurences to infer <c>
-- 2. add <a> and <c> value to every element
--    find letter with 8 occurences to infer <b>
-- 3. add <a>,<b>,<c> to every element
--    find letter with 4 occurences for <e> and 6 for <f> 
-- 3. take (4) string -> append <a> and <b> then diff with (8) to get <d>
-- 4. <g> is the remaining letter
-- https://en.wikipedia.org/wiki/Seven-segment_display --
decodeLine :: SignalIO -> Int
decodeLine signal = readInt $ map
  ( (fromJust . flip M.lookup digitMap)
  . (sort . map (fromJust . flip M.lookup result))
  )
  output
 where
  input    = fst signal
  output   = snd signal
  count1   = groupCount input
  maybeSig = getUniqueSignals input
  one      = fromJust $ find (flip (==) 1 . fst) maybeSig
  four     = fromJust $ find (flip (==) 4 . fst) maybeSig
  seven    = fromJust $ find (flip (==) 7 . fst) maybeSig
  eight    = fromJust $ find (flip (==) 8 . fst) maybeSig
  a        = head $ head $ splitOn "" (snd seven) \\ splitOn "" (snd one)
  c        = fst . fromJust $ find (flip (==) 9 . snd) count1
  count2   = groupCount $ map (nub . (++) [a] . (++) [c]) input
  b        = fst . fromJust $ find (flip (==) 8 . snd) count2
  count3   = groupCount $ map (nub . (++) [a] . (++) [c] . (++) [b]) input
  e        = fst . fromJust $ find (flip (==) 4 . snd) count3
  f        = fst . fromJust $ find (flip (==) 6 . snd) count3
  counts_4 = groupCount
    $ map (nub . (++) [a] . (++) [c] . (++) [b] . (++) [e] . (++) [f]) input
  d      = head $ snd eight \\ (snd four ++ [a] ++ [e])
  g      = head $ filter (`notElem` [a, b, c, d, e, f]) ['a' .. 'g']
  result = M.fromList
    [(a, 'a'), (b, 'c'), (c, 'f'), (d, 'g'), (e, 'e'), (f, 'b'), (g, 'd')]
