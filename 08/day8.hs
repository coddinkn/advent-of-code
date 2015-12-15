import System.Environment
import Data.List
import Data.List.Split
import Control.Monad

numOf :: String -> Char -> Int
numOf (a:rest) b = if a == b then 1 + (numOf rest b) else numOf rest b
numOf [] _ = 0

encodeLength :: String -> Int
encodeLength s = 2 + (codeLength s) + (numOf s '\\') + (numOf s '"')

codeLength :: String -> Int
codeLength = length

memoryLength :: String -> Int
memoryLength = memoryLength' 0 . drop 1 . reverse . drop 1 . reverse

memoryLength' :: Int -> String -> Int
memoryLength' x (a:b:c:d:rest) = if a == '\\'
    then if b == 'x' then memoryLength' (x + 1) rest else memoryLength' (x + 1) (c:d:rest)
    else memoryLength' (x + 1) (b:c:d:rest)
memoryLength' x (a:b:rest) = if a == '\\' then memoryLength' (x + 1) rest else memoryLength' (x + 1) (b:rest)
memoryLength' x (a:rest) = memoryLength' (x + 1) rest
memoryLength' x _ = x

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let strings = lines input
    let code = sum $ map codeLength strings
    let memory = sum $ map memoryLength strings 
    let encode = sum $ map encodeLength strings
    print $ code - memory
    print $ encode - code
