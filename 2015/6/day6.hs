import System.Environment
import Data.List
import Data.List.Split
import Control.Monad

lightGrid = [[False | _ <- [1..1000]] | _ <- [1..1000]]
brightGrid = [[0 | _ <- [1..1000]] | _ <- [1..1000]]

data Action = OFF | ON | TOGGLE

brightAction :: Action -> Int -> Int
brightAction ON x = x + 1
brightAction OFF x = if x > 0 then x - 1 else 0
brightAction TOGGLE x = x + 2

whatAction :: String -> Action
whatAction s
    | head w == "toggle" = TOGGLE
    | otherwise = if w !! 1 == "on"
        then ON
        else OFF
    where w = words s

whichLights :: String -> ((Int, Int), (Int, Int))
whichLights = getRange . words

getRange :: [String] -> ((Int, Int), (Int, Int))
getRange ws = (makeTuple (splitOn "," (head $ drop 2 $ reverse ws)), makeTuple (splitOn "," (last ws)))

makeTuple :: [String] -> (Int, Int)
makeTuple (x:y:z) = (read x, read y)

type Instruction = (Action, ((Int, Int), (Int, Int)))

applyRange :: (Int, Int) -> (a -> a) -> [a] -> [a] 
applyRange r f l = applyRange' 0 r f l

applyRange' :: Int -> (Int, Int) -> (a -> a) -> [a] -> [a]
applyRange' c (x, y) f (l:ls)
    | c >= x && c <= y = ((f l):(applyRange' (c + 1) (x, y) f ls))
    | otherwise = (l:(applyRange' (c + 1) (x, y) f ls))
applyRange' _ _ _ [] = []

doAction :: Action -> Bool -> Bool
doAction TOGGLE x = not x
doAction ON _ = True
doAction OFF _ = False

doInstruction :: Instruction -> [[Bool]] -> [[Bool]]
doInstruction (a, ((x1, y1), (x2, y2))) g = applyRange (x1, x2) (change (y1, y2) a) g
             where change (x, y) a ls = applyRange (x, y) (doAction a) ls 

doBrightInstruction :: Instruction -> [[Int]] -> [[Int]]
doBrightInstruction (a, ((x1, y1), (x2, y2))) g = applyRange (x1, x2) (bright (y1, y2) a) g
             where bright (x, y) a ls = applyRange (x, y) (brightAction a) ls

doIt :: [[Bool]] -> [Instruction] -> [[Bool]]
doIt g (x:xs) = doIt (doInstruction x g) xs
doIt g [] = g

brightDoIt :: [[Int]] -> [Instruction] -> [[Int]]
brightDoIt g (x:xs) = brightDoIt (doBrightInstruction x g) xs
brightDoIt g [] = g

sentenceToInstruction :: String -> Instruction
sentenceToInstruction s = (whatAction s, whichLights s)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let instructions = lines input
    let instructionList = map sentenceToInstruction instructions
    let final = doIt lightGrid instructionList
    let bright = brightDoIt brightGrid instructionList
    putStrLn $ show $ sum $ map (sum . map (\b -> if b then 1 else 0)) final 
    putStrLn $ show $ sum $ map (sum) bright
