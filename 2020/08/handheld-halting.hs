import Data.Set (Set, member, insert, empty)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

data Instruction = Acc Int
                 | Jmp Int
                 | Nop Int
                 deriving (Eq, Ord, Show)
                
parseOffset :: String -> Int
parseOffset = read . dropWhile (\c -> c /= '-' && (not $ isDigit c))

parseInstruction :: String -> Instruction
parseInstruction string =
    let offset = parseOffset string
    in case take 3 string of
        "acc" -> Acc offset
        "jmp" -> Jmp offset
        "nop" -> Nop offset 

type HandheldState = (Int, Int, Set Int)
-- Accumulator Instruction SeenInstructions

runHandheld :: [Instruction] -> HandheldState -> HandheldState
runHandheld instructions (acc, pc, seen) =
    if pc == length instructions
    then (acc, pc, seen)
    else let next = instructions !! pc
         in if pc `member` seen
            then (acc, pc, seen)
            else let newSeen = pc `insert` seen
                 in runHandheld instructions
                     $ case next of
                         Acc n -> (acc + n, pc + 1, newSeen)
                         Jmp n -> (acc, pc + n, newSeen)
                         Nop _ -> (acc, pc + 1, newSeen)

bootHandheld :: [Instruction] -> Int
bootHandheld instructions =
    let (acc, _, _) = runHandheld instructions (0, 0, empty)
    in acc

twiddle :: Instruction -> Instruction
twiddle (Jmp n) = Nop n
twiddle (Nop n) = Jmp n
twiddle x = x

fix :: Int -> [Instruction] -> [Instruction]
fix pc instructions =
    let indexed = [1..] `zip` instructions 
    in map (\(n, ins) -> if pc == n then twiddle ins else ins) indexed

testFix :: [Instruction] -> Int -> Maybe Int
testFix instructions pc =
    let pastPc = length instructions
        (acc, finalPc, _) = runHandheld (fix pc instructions) (0, 0, empty)
    in if finalPc == pastPc
       then Just acc
       else Nothing

terminateHandheld :: [Instruction] -> Int
terminateHandheld instructions =
    let lastPc = (length instructions) - 1
    in head $ mapMaybe (testFix instructions) [0 .. lastPc]

main :: IO ()
main = do
    instructions <- map parseInstruction . lines <$> readFile "input.txt"
    putStr "Part 1: " 
    print $ bootHandheld instructions
    putStr "Part 2: "
    print $ terminateHandheld instructions
