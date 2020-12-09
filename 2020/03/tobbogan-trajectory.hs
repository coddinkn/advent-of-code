import qualified Data.Set as S
import Data.List (unfoldr)
import Data.List.Index (ifoldl)

type TreeMap = S.Set (Int, Int)

isTree :: Char -> Bool
isTree = (== '#')

mapTree :: Int -> TreeMap -> Int -> Bool -> TreeMap
mapTree row treeMap col tree =
    if tree 
    then S.insert (row, col) treeMap
    else treeMap

mapRow :: TreeMap -> Int -> [Bool] -> TreeMap
mapRow treeMap row trees = ifoldl (mapTree row) treeMap trees

mapTrees :: [[Bool]] -> TreeMap
mapTrees trees = ifoldl mapRow S.empty trees 

checkTree :: TreeMap -> Int -> (Int, Int) -> Bool
checkTree treeMap width (row, col) =
    (row, col `mod` width) `S.member` treeMap

makeSpot :: Int -> Int -> Int -> (Int, Int) -> Maybe ((Int, Int), (Int, Int))
makeSpot bottom right down (row, col) =
    if row < bottom
    then let next = (row + down, col + right)
         in Just (next, next)
    else Nothing

spotsToCheck :: Int -> Int -> Int -> [(Int, Int)]
spotsToCheck bottom right down = unfoldr (makeSpot bottom right down) (0, 0)

findTrees :: [[Bool]] -> Int -> Int -> Int
findTrees trees right down =
    let bottom = length trees
        width = length $ head trees 
        toCheck = spotsToCheck bottom right down
        treeMap = mapTrees trees
    in length $ filter (checkTree treeMap width) toCheck

main :: IO ()
main = do
    trees <- map (map isTree) . lines <$> readFile "input.txt"
    putStr "Part 1: "
    print $ findTrees trees 3 1 
    putStr "Part 2: "
    print . product $ map (uncurry $ findTrees trees) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
