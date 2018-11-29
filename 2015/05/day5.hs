import System.Environment
import Data.List
import Control.Monad

noSpecialSequences :: String -> Bool
noSpecialSequences s = not $ isInfixOf "ab" s || isInfixOf "cd" s || isInfixOf "pq" s || isInfixOf "xy" s

hasThreeVowels :: String -> Bool
hasThreeVowels s = (length $ filter (`elem` "aeiou") s) >= 3

hasDoubleLetter :: String -> Bool
hasDoubleLetter (a:b:s) = a == b || hasDoubleLetter (b:s)
hasDoubleLetter _ = False

hasSandwich :: String -> Bool
hasSandwich (a:b:c:s) = (a == c) || hasSandwich (b:c:s)
hasSandwich _ = False

hasDoubleTwice :: String -> Bool
hasDoubleTwice (a:b:c:s) = isInfixOf (a:b:[]) (c:s) || isInfixOf (b:c:[]) s || hasDoubleTwice (c:s)
hasDoubleTwice _ = False

isNice :: String -> Bool
isNice s = hasDoubleLetter s && hasThreeVowels s && noSpecialSequences s

newIsNice :: String -> Bool
newIsNice s = hasDoubleTwice s && hasSandwich s

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    putStrLn $ show $ length $ filter newIsNice $ words input
