import           AOCUtils        (Tuple4 (..), readInt)
import           Data.Char       (isDigit)
import           Data.List       (sortBy)
import           Data.List.Split (splitOneOf)

type Sub = Tuple4 Int -- (geode,obsidian,clay,ore)
type State = (Sub, Sub)
type BluePrint = (Int, [State])

main :: IO ()
main = do
  blueprints <- map parseLine . lines <$> readFile "./src/2022/data/day19.txt"
  putStrLn "---Part 1 -------------"
  let part1 = [idx * best | bp@(idx, _) <- blueprints, let best = solve bp 24]
  print $ sum part1
  putStrLn "---Part 2 -------------"
  let part2 = [best | bp@(idx, _) <- take 3 blueprints, let best = solve bp 32]
  print $ product part2

-- for each blueprint index output
-- the possible branches as a list
-- of (cost, make) tuples + include
-- the noop action
parseLine :: String -> BluePrint
parseLine xs =
  ( idx
  , [ (Tuple4 (0, 0, 0, ooc), Tuple4 (0, 0, 0, 1)) -- ore
    , (Tuple4 (0, 0, 0, coc), Tuple4 (0, 0, 1, 0)) -- clay
    , (Tuple4 (0, 0, obcc, oboc), Tuple4 (0, 1, 0, 0)) -- obsidian
    , (Tuple4 (0, gobc, 0, goc), Tuple4 (1, 0, 0, 0)) -- geode
    , (Tuple4 (0, 0, 0, 0), Tuple4 (0, 0, 0, 0)) -- noop
    ])
  where
    words = splitOneOf " ,:." xs
    [idx, ooc, coc, oboc, obcc, goc, gobc] =
      readInt <$>
      filter (and . ([not . null, isDigit . head] <*>) . (: [])) words

-- on each branch where a robot can be created
-- add the resources created by all robots minus
-- the cost to create this robot + add the robot
-- to our count. keep only 500 best branches with
-- the most res + count
solve :: BluePrint -> Int -> Int
solve (idx, def) t = best
  where
    Tuple4 (best, _, _, _) =
      fst $ head $ branch' t [(Tuple4 (0, 0, 0, 0), Tuple4 (0, 0, 0, 1))]
    branch' :: Int -> [State] -> [State]
    branch' t branches
      | t == 0 = branches
      | otherwise =
        branch'
          (t - 1)
          (take 500 $
           sortBy (\a b -> compare (uncurry (<>) b) (uncurry (<>) a)) $ -- 🐢
           branches ++ subBranches)
      where
        subBranches =
          [ (res <> count <> cost', count <> for)
          | (res, count) <- branches
          , (cost, for) <- def
          , let cost' = (* (-1)) <$> cost
                Tuple4 (g, ob, c, o) = (>= 0) <$> (res <> cost')
          , g && ob && c && o
          ]
