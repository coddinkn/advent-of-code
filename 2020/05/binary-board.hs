import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)

fold :: (Int, Int) -> Bool -> (Int, Int)
fold (digit, current) b =
    let nextDigit = digit + 1
        next = current + (2 ^ digit)
    in (nextDigit, bool current next b)

get :: Char -> String -> Int
get char str =
    let bools = map (== char) $ reverse str
    in snd $ foldl fold (0, 0) bools

getRow :: String -> Int
getRow = get 'B'

getCol :: String -> Int
getCol = get 'R'

getId :: String -> Int
getId str =
    let rowStr = take 7 str
        colStr = drop 7 str
    in (getRow rowStr * 8) + (getCol colStr)

mySeat :: [Int] -> Int
mySeat ids = 
    let gap n = and [ n - 1 `elem` ids
                    , not $ n `elem` ids
                    , n + 1 `elem` ids
                    ]
    in fromJust $ find gap [minimum ids..]

main :: IO ()
main = do
    strs <- lines <$> readFile "input.txt"
    putStr "Part 1: "
    let ids = getId <$> strs
    print $ maximum ids
    putStr "Part 2: "
    print $ mySeat ids
