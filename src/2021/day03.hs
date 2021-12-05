import           AOCUtils                       ( readInt
                                                , bin2num
                                                )
main :: IO ()
main = do
  rawData <- lines <$> readFile "./src/2021/data/day03.txt"
  let diagnostic = parseDiagnostic rawData
  putStrLn
    $  "---Part 1 -------------"
    ++ "\nPower consumption = "
    ++ show (readPowerConsumption diagnostic)
    ++ "\n---Part 2 -------------"
    ++ "\nOxygen = "
    ++ show (readOxygenRating diagnostic)
    ++ "\nC02 = "
    ++ show (readCO2Rating diagnostic)
    ++ "\nLife Support = "
    ++ show (readCO2Rating diagnostic * readOxygenRating diagnostic)

-- Helpers --
type BitSignificancePair = (Int, Int)
type Counter = Int
type BitList = [Int]
type RatingPredicate = BitSignificancePair -> Counter -> BitList -> Bool


parseDiagnostic :: [String] -> [BitList]
parseDiagnostic = map (fmap (readInt . pure))

zipCountBits :: BitList -> [(Int, Int)] -> [(Int, Int)]
zipCountBits =
  zipWith (\bit (c0, c1) -> if bit == 0 then (c0 + 1, c1) else (c0, c1 + 1))

getSignificantBits :: [BitList] -> BitSignificancePair -> [BitSignificancePair]
getSignificantBits xs fallbackOnEq = counts
 where
  acc = replicate (length $ head xs) (0, 0)
  counts =
    map
      (\(c0, c1) -> if c0 == c1
        then fallbackOnEq
        else if (<) c0 c1 then (1, 0) else (0, 1)
      )
      (foldl (flip zipCountBits) acc xs) :: [(Int, Int)]


-- Part 1 --
readPowerConsumption :: [BitList] -> Int
readPowerConsumption xs = bin2num gammaRate * bin2num epsilonRate
 where
  result      = getSignificantBits xs (1, 0)
  gammaRate   = map fst result
  epsilonRate = map snd result


-- Part 2 --
getRating
  :: [BitList] -> RatingPredicate -> BitSignificancePair -> Counter -> BitList
getRating xs pred fallbackOnEq n
  | len == 0  = error "invalid rating"
  | len == 1  = head match
  | otherwise = getRating match pred fallbackOnEq (n + 1)
 where
  sigBits = getSignificantBits xs fallbackOnEq !! n
  match   = (filter $ pred sigBits n) xs
  len     = length match

readOxygenRating :: [BitList] -> Int
readOxygenRating xs = bin2num rating
 where
  rating = getRating xs (\(most, _) n match -> most == match !! n) (1, 0) 0

readCO2Rating :: [BitList] -> Int
readCO2Rating xs = bin2num rating
 where
  rating = getRating xs (\(_, least) n match -> least == match !! n) (1, 0) 0
