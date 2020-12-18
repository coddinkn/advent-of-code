import Data.List (intercalate)
import Data.List.Split (splitWhen)
import Data.Set (Set, fromList, union, size, intersection)

type CustomsForm = Set Char
type Group = [CustomsForm]

parseIndividual :: String -> CustomsForm
parseIndividual = fromList

parseGroup :: [String] -> Group
parseGroup = map parseIndividual

parseGroups :: [String] -> [Group]
parseGroups = map parseGroup . splitWhen (== "")

countAnyYes :: Group -> Int
countAnyYes = size . foldl1 union

countAllYes :: Group -> Int
countAllYes = size . foldl1 intersection

main :: IO ()
main = do
   groups <- parseGroups . lines <$> readFile "input.txt"
   putStr "Part 1: "
   print . sum $ map countAnyYes groups
   putStr "Part 2: "
   print . sum $ map countAllYes groups
