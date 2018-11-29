import Data.List

check :: [Int] -> Int
check xs = max - min
    where max = maximum xs
          min = minimum xs

check2 :: [Int] -> Int
check2 xs = check2' $ sort xs 

check2' :: [Int] -> Int
check2' (x:xs) = let filtered = filter (\y -> y `mod` x == 0) xs in
                    if null filtered
                    then check2' xs
                    else (head filtered) `div` x


main = do
    content <- readFile "input.txt"
    let ls = map (\s -> map (\c -> if c == '\t' then ' ' else c) s) $ lines content
    let ws = map words ls
    let rows = map (\xs -> map (\x -> read x :: Int) xs) ws 
    print $ sum $ map check2 rows
