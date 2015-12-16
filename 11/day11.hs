import System.Environment
import Data.Char
import Data.List
import Control.Monad

type Password = [Int]

i = (ord 'i') - 97
o = (ord 'o') - 97
l = (ord 'l') - 97

isValidPassword :: Password -> Bool
isValidPassword p = (containsDoubleDouble p) && (containsIncreasingSequence p) && (containsNoIOL p)

containsDoubleDouble :: Password -> Bool
containsDoubleDouble p = 2 <= (length $ group $ filter (\ns -> (length ns) >= 2) gp)
    where gp = group p

containsNoIOL :: Password -> Bool
containsNoIOL p = (not $ i `elem` p) && (not $ o `elem` p) && (not $ l `elem` p)

containsIncreasingSequence :: Password -> Bool
containsIncreasingSequence (a:b:c:rest) = (b == (a + 1)) && (c == (b + 1)) || containsIncreasingSequence (b:c:rest)
containsIncreasingSequence _ = False

showPassword :: Password -> String
showPassword = map (chr . (+ 97))

makePassword :: String -> Password
makePassword = map ((\n -> n - 97) . ord)

incrementPassword :: Password -> Password
incrementPassword = reverse . incrementPassword' . reverse

incrementPassword' :: Password -> Password 
incrementPassword' (x:xs) = if (x + 1) `mod` 26 > x then (((x + 1) `mod` 26):xs) else (((x + 1) `mod` 26):(incrementPassword' xs))
incrementPassowrd' [] = []

getNextValidPassword :: Password -> Password
getNextValidPassword p = if isValidPassword p then p else getNextValidPassword $ incrementPassword p

main :: IO ()
main = do
   input <- getLine
   let badPassword = makePassword input
   let goodPassword = getNextValidPassword badPassword
   putStrLn $ showPassword $ goodPassword 

