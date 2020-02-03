import Data.List (permutations)

import Computer

amplify :: [Int] ->  [Int] -> Int
amplify program modes = let amplifiers = configureAmplifier <$> modes
                        in foldl (flip ($)) 0 amplifiers 
                        where configureAmplifier mode input = compute program [mode, input]

maxOutput :: [Int] -> Int
maxOutput program = maximum $ amplify program <$> permutations [0 .. 4]

main :: IO ()
main = do program <- map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "input.txt"
          putStr "Part 1: "
          print $ maxOutput program
