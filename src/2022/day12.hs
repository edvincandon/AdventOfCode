import           AOCUtils       (DjikstraGraph (Graph), djikstra, edges,
                                 safeIndex, toIndexedMatrix2)
import           Data.Bifunctor (first, second)
import           Data.Char      (ord)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (mapMaybe)
import           GHC.Char       (chr)

type PathMatrix = [[(Char, (Int, Int))]]

main :: IO ()
main = do
  matrix <- toIndexedMatrix2 . lines <$> readFile "./src/2022/data/day12.txt"
  putStrLn "---Part 1 -------------"
  let graph = toGraph matrix
  print $ djikstra graph "S" "E"
  putStrLn "---Part 1 -------------"
  print $
    minimum $
    fmap (flip (djikstra graph) "E") $
    Map.keys $ Map.filterWithKey (\(k:_) _ -> k == 'a') (edges graph)

toGraph :: PathMatrix -> DjikstraGraph
toGraph matrix = Graph (Map.unions subgraphs)
  where
    subgraphs =
      concatMap
        (map
           (\node@(ch, _) ->
              if ch == 'E'
                then Map.singleton "E" []
                else Map.fromList
                       [ ( getNodeUID node
                         , (\neighbor -> (getNodeUID neighbor, 1)) <$>
                           neighbors node matrix)
                       ]))
        matrix

neighbors :: (Char, (Int, Int)) -> PathMatrix -> [(Char, (Int, Int))]
neighbors (curr, coords) m =
  filter
    (\(next, _) -> sanitizeChr next <= chr (ord (sanitizeChr curr) + 1))
    neighbors
  where
    neighbors =
      mapMaybe
        (\(x, y) -> flip safeIndex x =<< safeIndex m y)
        ([first (flip (-) 1), first (+ 1), second (flip (-) 1), second (+ 1)] <*>
         [coords])

sanitizeChr :: Char -> Char
sanitizeChr xs
  | xs == 'S' = 'a'
  | xs == 'E' = 'z'
  | otherwise = xs

-- Because my current implementation of
-- djikstra only support nodes of type
-- String and not of type Coord (todo..)
-- ⚠️ account for overlaps add '-'
getNodeUID :: (Char, (Int, Int)) -> String
getNodeUID ('S', _)    = "S"
getNodeUID ('E', _)    = "E"
getNodeUID (c, (x, y)) = [c] ++ show x ++ "-" ++ show y
