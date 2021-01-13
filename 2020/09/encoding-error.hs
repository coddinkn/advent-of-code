import Data.List (find, inits, tails)
import Data.Set (fromList, member)

-- x is notHalf of y
notHalf :: Int -> Int -> Bool
notHalf y x = x * 2 /= y

validate :: [Int] -> Int -> Bool
validate previous target =
    let previousSet = fromList previous
        noHalves = filter (notHalf target) previous
    in or . map (`member` previousSet) . map (\n -> target - n) $ noHalves

firstInvalid :: Int -> [Int] -> Maybe Int
firstInvalid preambleLength xs
    | length xs < preambleLength = error "not enough numbers"
    | length xs == preambleLength = Nothing
    | otherwise =
        let previous = take preambleLength xs
            toValidate = head . take 1 $ drop preambleLength xs
        in if validate previous toValidate
           then firstInvalid preambleLength $ drop 1 xs  
           else Just toValidate

findContiguousSet :: Int -> [Int] -> Maybe [Int]
findContiguousSet target xs =
    let sumsToTarget ys = sum ys == target
        contiguousSets = concat $ inits <$> tails xs
    in find sumsToTarget contiguousSets

encryptionWeakness :: [Int] -> Int -> Maybe Int
encryptionWeakness xs target = do
    contiguousSet <- findContiguousSet target xs
    return $ minimum contiguousSet + maximum contiguousSet

printMaybe :: Show a => Maybe a -> IO ()
printMaybe = maybe (return ()) print 

main :: IO ()
main = do
    numbers <- map read . lines <$> readFile "input.txt"
    putStr "Part 1: "
    let invalid = firstInvalid 25 numbers
    printMaybe invalid 
    putStr "Part 2: "
    printMaybe $ invalid >>= encryptionWeakness numbers
