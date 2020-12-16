import           Data.List.Split                ( splitOn
                                                , splitOneOf
                                                , splitWhen
                                                )
import           Data.List                      ( isPrefixOf
                                                , find
                                                )
import           Data.Char                      ( isDigit
                                                , isAlpha
                                                , isHexDigit
                                                )

-- DEFINITIONS
-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

main = do
  contents <- readFile "./data/day04.txt"
  print $ length . filter (== True) . map (isValid . splitOneOf " \n") $ splitOn
    "\n\n"
    contents

validators :: [(String, String -> Bool)]
validators =
  [ ("byr", (<?) (1920, 2002) . read)
  , ("iyr", (<?) (2010, 2020) . read)
  , ("eyr", (<?) (2020, 2030) . read)
  , ("hgt", parseHeight)
  , ("hcl", parseColor)
  , ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", \x -> length x == 9 && foldr (\a b -> isDigit a && b) True x)
  ]

(<?) :: Ord a => (a, a) -> a -> Bool
(<?) (min, max) x = x >= min && x <= max

parseHeight :: String -> Bool
parseHeight xs = case last $ splitWhen isDigit xs of
  "cm" -> (<?) (150, 193) h
  "in" -> (<?) (59, 76) h
  _    -> False
  where h = read . head $ splitWhen isAlpha xs :: Int

parseColor :: String -> Bool
parseColor xs =
  isPrefixOf "#" xs
    && length hex
    == 6
    && foldr (\a b -> b && isHexDigit a) True hex
  where hex = last $ splitOn "#" xs


isValid' :: [String] -> Bool
isValid' xs = foldr
  (\a b ->
    b
      && (case find (isPrefixOf a) xs of
           Just _  -> True
           Nothing -> False
         )
  )
  True
  ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: [String] -> Bool
isValid xs = foldr
  (\a b ->
    b
      && (case find (isPrefixOf $ fst a) xs of
           Just match -> snd a $ last $ splitOn ":" match
           Nothing    -> False
         )
  )
  True
  validators


