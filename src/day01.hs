main = do
  contents <- readFile "./data/day01.txt"
  print . findTupleResult . map read . words $ contents
  print . findTripleResult . map read . words $ contents

findTupleResult :: [Int] -> Int
findTupleResult x = product $ head [ [a, b] | a <- x, b <- x, a + b == 2020 ]

findTripleResult :: [Int] -> Int
findTripleResult x =
  product $ head [ [a, b, c] | a <- x, b <- x, c <- x, a + b + c == 2020 ]
