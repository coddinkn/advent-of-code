import System.Environment
import Data.List

evaluateCombination :: [Int] -> [Bool] -> Bool
evaluateCombination containers useWhich = (sum $ map fst $ filter (\(_, using) -> using) $ zip containers useWhich) == 150

makeCombinations :: Int -> [[Bool]] -> [[Bool]]
makeCombinations max [] = makeCombinations max (zero:[])
    where zero = take max $ cycle [False]
makeCombinations max current
    | (length $ filter (\i -> i) $ head current) < max = makeCombinations max ((combinationIncrement (head $ current)):current)
    | otherwise                                        = current

combinationIncrement :: [Bool] -> [Bool]
combinationIncrement (x:xs)
    | x == True = False:(combinationIncrement xs) 
    | x == False = True:xs

main = do
    args <- getArgs
    input <- readFile $ head args
    let containers = map read $ lines input
    let numContainers = length containers
    let validCombinations = filter (evaluateCombination containers) $ makeCombinations numContainers [] 
    let minimumNumber = (\x -> length $ filter (\i -> i) x) $ last $ sortBy (\x y -> compare (length $ filter (\i -> i) y) (length $ filter (\i -> i) x)) $ validCombinations
    print $ length $ filter (\x -> (length $ filter (\i -> i) x) == minimumNumber) validCombinations
