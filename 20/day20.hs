getElves :: Int -> [Int]
getElves house = (map (\n -> house `div` n) someDivisors) ++ someDivisors
    where max          = floor $ sqrt $ fromIntegral house
          someDivisors = [x | x <- [1..max], house `mod` x == 0] 

newGetElves :: Int -> [Int]
newGetElves house = extractHouses $ map (\n -> (n, house `div` n)) someDivisors
    where max          = floor $ sqrt $ fromIntegral house
          someDivisors = [x | x <- [1..max], house `mod` x == 0]

extractHouses :: [(Int, Int)] -> [Int]
extractHouses ((a, b):rest)
    | b <= 50 && a <= 50 = (a:b:(extractHouses rest))
    | b <= 50            = (a:(extractHouses rest))
    | a <= 50            = (b:(extractHouses rest))
    | otherwise          = extractHouses rest
extractHouses [] = []

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x:(removeDuplicates xs)
removeDuplicates [] = []

firstWithAtLeast :: Int -> Int
firstWithAtLeast = firstWithAtLeast' 1

firstWithAtLeast' :: Int -> Int -> Int
firstWithAtLeast' n presents
    | (sum $ removeDuplicates $ getElves n) >= presents = n
    | otherwise                                         = firstWithAtLeast' (n + 1) presents

newCountPresents :: Int -> Int
newCountPresents = (*) 11 . sum . removeDuplicates . newGetElves  

presents = 36000000 
nonInflatedPresents = presents `div` 10

main :: IO ()
main = do
    --print $ firstWithAtLeast nonInflatedPresents 
    print $ head $ filter (\n -> newCountPresents n >= presents) [1..]
