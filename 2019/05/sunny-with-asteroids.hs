import Computer

main :: IO ()
main = do memory <- map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "input.txt"
          putStr "Part 1: "
          print $ compute memory [1]
          putStr "Part 2: "
          print $ compute memory [5]
