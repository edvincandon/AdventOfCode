main = do
  contents <- readFile "./data/day01.txt"
  print . findTupleResult . map read . words $ contents

findTupleResult :: [Int] -> Int
findTupleResult x = product $ head [ [a, b] | a <- x, b <- x, a + b == 2020 ]
