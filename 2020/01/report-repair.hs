import qualified Data.HashSet as Set
import Data.List (find)

findPair :: Int -> [Int] -> Maybe (Int, Int)
findPair target numbers =
    let set = Set.fromList numbers
        first = find (\n -> (target - n) `Set.member` set) numbers
    in (\n -> (n, target - n)) <$> first

findTriple :: Int -> [Int] -> Maybe (Int, Int, Int)
findTriple target numbers =
    let set = Set.fromList numbers
        pairs = (,) <$> numbers <*> numbers
        first = find (\(x, y) -> (target - (x + y)) `Set.member` set) pairs
    in (\(x, y) -> (x, y, target - (x + y))) <$> first

main :: IO ()
main = do
    expenseReport <- map read . lines <$> readFile "input.txt"
    putStr "Part 1: "
    putStrLn $ case findPair 2020 expenseReport of
                Just (x, y) -> show $ x * y
                Nothing -> "Error :("
    putStr "Part 2: "
    putStrLn $ case findTriple 2020 expenseReport of
                Just (x, y, z) -> show $ x * y * z
                Nothing -> "Error :("
