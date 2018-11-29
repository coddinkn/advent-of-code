import System.Environment
import Data.List
import Control.Monad

data Dir = N | S | E | W

charToDir :: Char -> Dir
charToDir 'v' = S
charToDir '^' = N
charToDir '>' = E
charToDir '<' = W

getDirChange :: Dir -> (Int, Int)
getDirChange N = (0, 1)
getDirChange S = (0, -1)
getDirChange E = (1, 0)
getDirChange W = (-1, 0) 

moveDirection :: Dir -> (Int, Int) -> (Int, Int) 
moveDirection d (x, y) = (x + firstChange, y + secondChage) 
    where firstChange = fst $ getDirChange d
          secondChage = snd $ getDirChange d

removeDuplicates :: [(Int, Int)] -> [(Int, Int)]
removeDuplicates = map head . group . sort

stringToPos :: (Int, Int) -> [Char] -> [(Int, Int)] -> [(Int, Int)]
stringToPos p [] ps = (p:ps)
stringToPos p (c:cs) ps = (stringToPos (moveDirection (charToDir c) p) cs (p:ps))

takeHalf1 :: [a] -> [a]
takeHalf1 (x:_:xs) = ((x:[]) ++ (takeHalf1 xs))
takeHalf1 _ = []

takeHalf2 :: [a] -> [a]
takeHalf2 (_:x:xs) = ((x:[]) ++ (takeHalf2 xs))
takeHalf2 _ = []

divyTheWork :: [a] -> ([a], [a])
divyTheWork xs = ((takeHalf1 xs), (takeHalf2 xs)) 

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let (santa, roboSanta) = (divyTheWork input)
    let santaHouses = stringToPos (0, 0) santa []
    let roboSantaHouses = stringToPos (0, 0) roboSanta []
    let combinedNoRepeatHouses = removeDuplicates (roboSantaHouses ++ santaHouses)
    putStrLn $ show $ length $ combinedNoRepeatHouses
