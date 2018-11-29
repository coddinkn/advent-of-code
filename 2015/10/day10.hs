import System.Environment
import Data.List
import Data.Char
import Control.Monad

lookAndSay :: String -> String
lookAndSay s = map intToDigit $ lookAndSayNumbers n
    where n = map digitToInt s 

lookAndSayNumbers :: [Int] -> [Int]
lookAndSayNumbers n = concat $ map (\ns -> (length ns):(head ns):[]) $ group n 

repeatLookAndSay :: String -> Int -> String
repeatLookAndSay s n = if n == 0 
    then s
    else repeatLookAndSay (lookAndSay s) (n - 1)     

main :: IO ()
main = do
   input <- getLine
   putStrLn $ show $ length $ repeatLookAndSay input 50 
