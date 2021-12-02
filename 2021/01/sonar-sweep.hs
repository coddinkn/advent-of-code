import Data.List (foldl')

deltas :: Num n => [n] -> [n]
deltas ns = zipWith (-) ns $ head ns : ns

threeWindowSums :: Num n => [n] -> [n]
threeWindowSums ns = zipWith3 (\x y z -> x + y + z) ns (drop 1 ns) $ drop 2 ns

countIncreases :: (Num n, Ord n) => [n] -> Int
countIncreases = foldl' (\count delta -> if delta > 0 then count + 1 else count) 0

main :: IO ()
main = do
    depths <- map read . lines <$> readFile "input.txt"
    putStr "Part 1: "
    print . countIncreases $ deltas depths
    putStr "Part 2: "
    print . countIncreases . deltas $ threeWindowSums depths
