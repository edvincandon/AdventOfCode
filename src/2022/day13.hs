{-# LANGUAGE InstanceSigs #-}

import           AOCUtils        (readInt)
import           Data.Char       (isDigit)
import           Data.List       (sort)
import           Data.List.Split (chunksOf, oneOf, split)
import           Data.Ord        (comparing)

data Packet a
  = Elem a
  | List [Packet a]
  deriving (Eq, Show)

-- Ordering instance for Packet :
-- Elem vs Elem - compare inner values
-- Elem vs List - wrap elem in List
-- List vs List - compare each inner value
-- and ensure left length <= right length
instance (Ord a) => Ord (Packet a) where
  compare :: Ord a => Packet a -> Packet a -> Ordering
  compare (Elem a) (Elem b) = compare a b
  compare a@(Elem _) b = compare (List [a]) b
  compare a b@(Elem _) = compare a (List [b])
  compare (List a) (List b) = foldr (<>) (comparing length a b) $ zipWith compare a b

main :: IO ()
main = do
  packets <- map parsePacket . filter (not . null) . lines <$> readFile "./src/2022/data/day13.txt"
  -- Part 1 --
  putStrLn "---Part 1 -------------"
  let pairs = map (\(a:b:_) -> (a, b)) . chunksOf 2 $ packets
  print $ sum $ map fst . filter (uncurry (<=) . snd) $ zip [1 ..] pairs
  -- Part 2 --
  putStrLn "---Part 2 -------------"
  let divider1 = List [List [Elem 2]]
  let divider2 = List [List [Elem 6]]
  let sorted = sort (divider1 : divider2 : packets)
  let idx1 = length $ takeWhile (<= divider1) sorted
  let idx2 = length $ takeWhile (<= divider2) sorted
  print $ idx1 * idx2

sanitize :: String -> [String]
sanitize xs = filter (\str -> str /= "," && (not . null) str) $ split (oneOf "[,]") xs

parsePacket :: String -> Packet Int
parsePacket xs = fst $ parse (List [], sanitize xs)
  where
    parse :: (Packet Int, [String]) -> (Packet Int, [String])
    parse (list, []) = (list, [])
    parse (Elem _, _) = error "parse error"
    parse (List list, xs@(h:rest))
      | h == "]" = (List list, rest)
      | h == "[" =
        let (l', rest') = parse (List [], rest)
         in parse (List (list ++ [l']), rest')
      | otherwise = parse (List (list ++ [Elem (readInt h)]), rest)
