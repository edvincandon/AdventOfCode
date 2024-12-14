{-# LANGUAGE LambdaCase #-}

import qualified Data.Foldable as Seq
import           Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (><), (|>))
import qualified Data.Sequence as Seq

data Block
  = File Int Int -- (id, size)
  | Free Int -- (size)
  deriving (Show, Eq)

debug :: [Block] -> String
debug =
  concatMap
    (\case
       File id size -> concat $ replicate size (show id)
       Free size -> replicate size '.')

parse :: String -> Seq Block
parse input = Seq.fromList $ parseBlocks input 0 True
  where
    parseBlocks [] _ _ = []
    parseBlocks (x:xs) fileId isFile
      | size == 0 = parseBlocks xs fileId (not isFile)
      | isFile = File fileId size : parseBlocks xs (fileId + 1) False
      | otherwise = Free size : parseBlocks xs fileId True
      where
        size = read [x]

compact :: Seq Block -> [Block]
compact Empty = []
compact (f1@(File {}) :<| queue) = f1 : compact queue
compact (f1@(Free s) :<| (xs :|> (Free {}))) = compact (f1 :<| xs)
compact (f1@(Free s) :<| (xs :|> f2@(File fId s')))
  | s' == s = f2 : compact xs
  | s' < s = f2 : compact (Free (s - s') :<| xs)
  | s' > s = File fId s : compact (xs :|> File fId (s' - s))

compact' :: Seq Block -> Seq Block
compact' Empty = Empty
compact' (queue :|> f1@(Free {})) = compact' queue :|> f1
compact' (queue :|> f1@(File _ s')) =
  let (q', ins) = slot f1 queue
   in (if ins
         then compact' q' :|> Free s'
         else compact' q' :|> f1)

slot :: Block -> Seq Block -> (Seq Block, Bool)
slot (Free _) _ = error "invalid"
slot f1 Empty = (Empty, False)
slot f1 (f2@(File {}) :<| q) =
  let (q', ins) = slot f1 q
   in (f2 :<| q', ins)
slot f1@(File fId s') (f2@(Free s) :<| q)
  | s' == s = (f1 :<| q, True)
  | s' < s = (f1 :<| (Free (s - s') :<| q), True)
  | otherwise =
    let (q', ins) = slot f1 q
     in (f2 :<| q', ins)

solve :: [Block] -> Int
solve = sum . zipWith (*) [0 ..] . concatMap toIds
  where
    toIds (File id size) = replicate size id
    toIds (Free size)    = replicate size 0

main :: IO ()
main = do
  fs <- parse . head . lines <$> readFile "./src/2024/data/day09.txt"
  putStrLn "---Part 1 -------------"
  print $ solve . compact $ fs
  putStrLn "---Part 2 -------------"
  print $ solve . Seq.toList $ compact' fs
