import System.Environment
import Data.List
import Data.List.Split
import Data.Bits
import Data.Char
import Data.Word
import Control.Monad.Fix
import Control.Monad

type Label = String
type Value = Word16

data Input = VAL Value 
    | AND Label Label
    | ANDN Label Value 
    | OR Label Label
    | ORN Label Value
    | LSHIFT Label Int 
    | RSHIFT Label Int 
    | NOT Label
    | ALIAS Label
    deriving (Show)

type Wire = (Label, Input)

buildWire :: String -> Wire
buildWire s = (last w, parseInput $ words $ head $ splitOn " -> " s)
    where w = words s

parseInput :: [String] -> Input
parseInput (x:o:y:[])
    | o == "AND"    = case (isNumber x, isNumber y) of (False, False) -> AND x y
                                                       (True, False)  -> ANDN y $ read x 
                                                       (False, True)  -> ANDN x $ read y     
    | o == "OR"     = case (isNumber x, isNumber y) of (False, False) -> OR x y
                                                       (True, False)  -> ORN y $ read x
                                                       (False, True)  -> ORN x $ read y
    | o == "RSHIFT" = RSHIFT x $ read y
    | o == "LSHIFT" = LSHIFT x $ read y
    | otherwise     = error "broken input!"
    where isNumber x = and $ map isDigit x
parseInput s
    | length s == 1 && (isNumber $ head s) = VAL $ read $ head s
    | length s == 1                        = ALIAS $ head s
    | length s == 2 && head s == "NOT"     = NOT $ s !! 1
    | otherwise                            = error "broken input!"
    where isNumber x = and $ map isDigit x

getWire :: [Wire] -> Label -> Wire
getWire wires label = head $ filter (\(l, i) -> l == label) wires

measureWire :: [Wire] -> Label -> Value
measureWire wires label = evaluateInput wires $ snd $ getWire wires label 

evaluateInput :: [Wire] -> Input -> Value
evaluateInput _ (VAL x)          = x
evaluateInput wires (AND x y)    = (measureWire wires x) .&. (measureWire wires y) 
evaluateInput wires (ANDN x y)   = (measureWire wires x) .&. y
evaluateInput wires (OR x y)     = (measureWire wires x) .|. (measureWire wires y)
evaluateInput wires (ORN x y)    = (measureWire wires x) .|. y
evaluateInput wires (RSHIFT x y) = shiftR (measureWire wires x) y
evaluateInput wires (LSHIFT x y) = shiftL (measureWire wires x) y
evaluateInput wires (NOT x)      = complement $ measureWire wires x
evaluateInput wires (ALIAS x)    = measureWire wires x

isValueAlready :: [Wire] -> Label -> Bool
isValueAlready wires label = isValue $ getWire wires label

isValue :: Wire -> Bool 
isValue (_, VAL _) = True
isValue _ = False

takeOneStep :: [Wire] -> Wire -> Wire
takeOneStep _ (l, VAL x)          = (l, VAL x)
takeOneStep wires (l, AND x y)    = if isValueAlready wires x && isValueAlready wires y then (l, VAL $ (measureWire wires x) .&. (measureWire wires y)) else (l, AND x y) 
takeOneStep wires (l, ANDN x y)   = if isValueAlready wires x then (l, VAL $ (measureWire wires x) .&. y) else (l, ANDN x y)
takeOneStep wires (l, OR x y)     = if isValueAlready wires x && isValueAlready wires y then (l, VAL $ (measureWire wires x) .|. (measureWire wires y)) else (l, OR x y)
takeOneStep wires (l, ORN x y)    = if isValueAlready wires x then (l, VAL $ (measureWire wires x) .|. y) else (l, ORN x y)
takeOneStep wires (l, RSHIFT x y) = if isValueAlready wires x then (l, VAL $ shiftR (measureWire wires x) y) else (l, RSHIFT x y)
takeOneStep wires (l, LSHIFT x y) = if isValueAlready wires x then (l, VAL $ shiftL (measureWire wires x) y) else (l, LSHIFT x y)
takeOneStep wires (l, NOT x)      = if isValueAlready wires x then (l, VAL $ complement $ measureWire wires x) else (l, NOT x)
takeOneStep wires (l, ALIAS x)    = if isValueAlready wires x then (l, VAL $ measureWire wires x) else (l, ALIAS x)

fullyMeasure wires = if and $ map isValue (map (takeOneStep wires) wires) then (map (takeOneStep wires) wires) else fullyMeasure (map (takeOneStep wires) wires)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let inputs = lines input
    let wires = map buildWire inputs 
    let final = fullyMeasure wires
    print $ snd $ getWire final "a" 
