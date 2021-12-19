import Data.List (groupBy, transpose, find, partition)
import Data.Set (fromList, isSubsetOf)

type Board = [[Int]]

getGroups :: String -> [[String]]
getGroups = filter (/= [""]) . groupBy (\l1 l2 -> l1 /= "" && l2 /= "") . lines

parseBoard :: [String] -> Board
parseBoard = map (map read . words)

parseDrawn :: [String] -> [Int]
parseDrawn strs = read $ ('[' : head strs) ++ "]"

parseAll :: String -> ([Board], [Int])
parseAll fileContents =
    let groups = getGroups fileContents
        boards = map parseBoard $ drop 1 groups
        drawn = parseDrawn $ head groups
    in (boards, drawn)

sequenceWins :: [Int] -> [Int] -> Bool
sequenceWins drawn sequence =
    fromList sequence `isSubsetOf` fromList drawn

wins :: [Int] -> Board -> Bool
wins drawn board =
    let rowWin = any (sequenceWins drawn) board 
        colWin = any (sequenceWins drawn) $ transpose board
    in  rowWin || colWin

firstWinningBoard :: [Int] -> Int -> [Board] -> (Board, Int)
firstWinningBoard drawn numDrawn boards =
    case find (wins $ take numDrawn drawn) boards of
        Just board -> (board, numDrawn)
        Nothing -> firstWinningBoard drawn (numDrawn + 1) boards

allWinningBoards :: [(Board, Int)] -> [Int] -> Int -> [Board] -> [(Board, Int)]
allWinningBoards winningBoards drawn numDrawn boards =
    if numDrawn == length drawn
    then winningBoards
    else let (newWinningBoards, nonWinningBoards) = partition (wins $ take numDrawn drawn) boards
             newWinningBoardPairs = zip newWinningBoards $ repeat numDrawn
         in  allWinningBoards (winningBoards ++ newWinningBoardPairs) drawn (numDrawn + 1) nonWinningBoards

scoreBoard :: [Int] -> Board -> Int
scoreBoard drawn board =
    let sumOfUnmarked = sum . filter (not . flip elem drawn) $ concat board
    in  sumOfUnmarked * last drawn

part1 :: [Board] -> [Int] -> Int
part1 boards drawn =
    let (winningBoard, numberDrawn) = firstWinningBoard drawn 5 boards
        winningDrawn = take numberDrawn drawn
    in  scoreBoard winningDrawn winningBoard

part2 :: [Board] -> [Int] -> Int
part2 boards drawn =
    let (lastWinningBoard, numberDrawn) = last $ allWinningBoards [] drawn 5 boards
        lastWinningDrawn = take numberDrawn drawn
    in  scoreBoard lastWinningDrawn lastWinningBoard

main :: IO ()
main = do
    (boards, drawn) <- parseAll <$> readFile "input.txt"
    putStrLn "Part 1: "
    print $ part1 boards drawn
    putStrLn "Part 2: "
    print $ part2 boards drawn
