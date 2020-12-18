-- TODO : clean-up - learn parsers
import qualified Data.Bifunctor                as Bifunctor
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set


import           Text.ParserCombinators.ReadP   ( ReadP
                                                , between
                                                , char
                                                , endBy
                                                , eof
                                                , many
                                                , readP_to_S
                                                , satisfy
                                                , sepBy
                                                , string
                                                )

type BagDefinition = (String, [(Int, String)])

main :: IO ()
main = do
  bags <- parseData <$> readFile "./data/day07.txt"
  print $ length . findMatchingBags ["shiny gold"] $ bags
  print $ countTotalSubBags "shiny gold" bags

anyChar :: Char -> Bool
anyChar = const True

parseBagLine :: ReadP (String, [(String, String)])
parseBagLine = do
  res <- endBy (many . satisfy $ anyChar) $ string " bags"
  satisfy (== ' ')
  ns <- between (string "contain ")
                (char '.')
                (sepBy (many . satisfy $ anyChar) (char ','))
  eof
  return
    ( head res
    , map
      ( (\x -> (head x, unwords $ take 2 $ drop 1 x))
      . words
      . (\str -> if str == "no other bags" then "0 sub bags" else str)
      )
      ns
    )

parseData :: String -> [BagDefinition]
parseData =
  map
      ( Bifunctor.second (map $ Bifunctor.first read)
      . fst
      . head
      . readP_to_S parseBagLine
      )
    . lines

findMatchingBags :: [String] -> [BagDefinition] -> Set String
findMatchingBags [] _ = Set.empty
findMatchingBags colors xs =
  Set.fromList match `Set.union` findMatchingBags match xs
 where
  match = foldr
    (\bag matches ->
      matches
        ++ ([ fst bag | any (\subBag -> snd subBag `elem` colors) (snd bag) ])
    )
    []
    xs

countTotalSubBags :: String -> [BagDefinition] -> Int
countTotalSubBags []    _  = 0
countTotalSubBags color xs = foldr
  (\subBag total ->
    total + fst subBag + fst subBag * countTotalSubBags (snd subBag) xs
  )
  0
  match
  where match = concatMap snd (filter (\bag -> fst bag == color) xs)


