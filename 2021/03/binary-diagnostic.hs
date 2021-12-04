import Data.Bits (xor, shift, testBit)
import Data.Char (digitToInt)
import Data.List (foldl', transpose)

parseBinary :: String -> Int
parseBinary = foldl' (\acc x -> acc * 2 + digitToInt x) 0

mostCommon :: String -> Char
mostCommon chars =
    let ones = sum $ map digitToInt chars
        l = length chars
        half = l `div` 2 + l `mod` 2
    in if ones >= half
       then '1'
       else '0'

leastCommon :: String -> Char
leastCommon chars = if mostCommon chars == '1' then '0' else '1'

computeRate :: (String -> Char) -> [String] -> Int
computeRate bitSelector = parseBinary . map bitSelector . transpose

gammaRate :: [String] -> Int
gammaRate = computeRate mostCommon

epsilonRate :: [String] -> Int
epsilonRate = computeRate leastCommon

powerConsumption :: [String] -> Int
powerConsumption strings = gammaRate strings * epsilonRate strings

ratingCalculator :: ([String] -> Int) -> [String] -> Int
ratingCalculator rateCalculator strings = parseBinary . head . foldl' folder strings $ [size - 1, size - 2 .. 0]
    where size = length . head $ strings
          folder left currentBit =
              if length left == 1
              then left
              else filter (not . (flip testBit) currentBit . xor (rateCalculator left) . parseBinary) left

oxygenGeneratorRating :: [String] -> Int
oxygenGeneratorRating = ratingCalculator gammaRate

co2ScrubberRating :: [String] -> Int
co2ScrubberRating = ratingCalculator epsilonRate

lifeSupportRating :: [String] -> Int
lifeSupportRating strings = oxygenGeneratorRating strings * co2ScrubberRating strings

main :: IO ()
main = do
    strings <- lines <$> readFile "input.txt"
    putStr "Part 1: "
    print $ powerConsumption strings
    putStr "Part 2: "
    print $ lifeSupportRating strings
