import System.Environment
import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

isNum :: String -> Bool
isNum = and . map isDigit

changeToStar :: String -> String
changeToStar = map (\c -> if (not $ isDigit c) && (c /= '-') then '*' else c)

makeIntegerList :: String -> [Int]
makeIntegerList = map read . filter (not . null) . splitOn "*" . changeToStar

rangeObjects :: String -> Int -> [Int] -> [(Int, Int)]
rangeObjects (c:cs) n ns
    | c == '{'  = rangeObjects cs (n + 1) (n:ns)
    | c == '}'  = ((head ns, n):rangeObjects cs (n + 1) (drop 1 ns))
    | otherwise = rangeObjects cs (n + 1) ns
rangeObjects _ _ _ = []

findObjectContaining :: [(Int, Int)] -> Int -> (Int, Int)
findObjectContaining ranges x = (\(a, b, _) -> (a, b)) $ head $ sortBy (\(_, _, h) (_, _, g) -> compare h g) $ filter (\(s, e, _) -> (s <= x) && (e >= x)) withDistance 
    where withDistance = map (\(s, e) -> (s, e, abs (s - e))) ranges 

findAllRed :: Int -> String -> [Int]
findAllRed n s
    | n < (length s) && ((take 6 $ drop n $ s) == ":\"red\"") = (n:(findAllRed (n + 6) s))
    | n < (length s)                                          = findAllRed (n + 1) s 
    | otherwise                                               = []

starInRange :: (Int, Int) -> String -> String
starInRange = starInRange' 0

starInRange' :: Int -> (Int, Int) -> String -> String
starInRange' n (s, e) (c:cs) = if (n > s) && (n < e)
    then ('*':(starInRange' (n + 1) (s, e) cs))
    else (c:(starInRange' (n + 1) (s, e) cs))
starInRange' _ _ _ = []

starReds :: [(Int, Int)] -> String -> String
starReds (l:ls) s = starReds ls (starInRange l s)
starReds [] s = s

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    let ranges = rangeObjects input 0 []
    let redRanges = map (findObjectContaining ranges) $ findAllRed 0 input
    print $ sum $ makeIntegerList $ starReds redRanges input
