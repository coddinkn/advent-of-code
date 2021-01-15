import Data.List (sort, group)
import Data.Set (Set, member, fromList, insert)
import Data.Function (fix)

type Joltage = Int

outlet :: Joltage
outlet = 0

builtInJoltage :: [Joltage] -> Joltage
builtInJoltage adapters = maximum adapters + 3

joltageDiffDistribution :: [Joltage] -> [Int]
joltageDiffDistribution adapters =
    let orderedAdapters = sort adapters ++ [builtInJoltage adapters]
        (_, diffs) = foldl diffFold (outlet, []) orderedAdapters
    in map length . group $ sort diffs
    where diffFold (last, diffs) current = (current, current - last : diffs)

computePart1 :: [Int] -> Int
computePart1 diffs = maximum diffs * minimum diffs

ways :: Set Joltage -> (Joltage -> Integer) -> Joltage -> Integer
ways adapterSet f 0 = 1 
ways adapterSet f current = sum . map (\diff -> f (current - diff)) 
    $ filter (\diff -> (current - diff) `member` adapterSet) [1, 2, 3]

memoize :: (Joltage -> a) -> Joltage -> a
memoize f = (map f [0 ..] !!)

-- technique taken from https://wiki.haskell.org/Memoization#Memoization_with_recursion
possibleWays :: Set Joltage -> Joltage -> Integer 
possibleWays adapterSet = fix $ memoize . ways (0 `insert` adapterSet)

-- why doesn't explicit recursion work, unlike example above?
-- my guess is something to do with adapterSet being threaded through every recursive call 
possibleWays' :: Set Joltage -> Joltage -> Integer
possibleWays' adapterSet = (map ways'' [0 ..] !!)
    where ways' set 0 = 1
          ways' set current = sum . map (\diff -> possibleWays' set (current - diff)) 
             $ filter (\diff -> (current - diff) `member` set) [1, 2, 3]
          ways'' = ways' (0 `insert` adapterSet)

main :: IO ()
main = do
    adapters <- map read . lines <$> readFile "input.txt"
    putStr "Part 1: "
    print . computePart1 $ joltageDiffDistribution adapters
    putStr "Part 2: "
    let adapterSet = fromList adapters
    print $ possibleWays adapterSet (builtInJoltage adapters)
