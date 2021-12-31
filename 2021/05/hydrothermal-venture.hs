{-# LANGUAGE OverloadedStrings #-}

import Data.List (unfoldr, foldl')
import Data.Map  (Map, insertWith)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type Point = (Int, Int)

data LineSegment =
    LineSegment Int Int Int Int
    deriving (Show, Eq)

parseLineSegment :: Parser LineSegment
parseLineSegment = do
    x1 <- read <$> some digitChar
    char ','
    y1 <- read <$> some digitChar
    space
    string "->"
    space
    x2 <- read <$> some digitChar
    char ','
    y2 <- read <$> some digitChar
    return $ LineSegment x1 y1 x2 y2

verticalOrHorizontal :: LineSegment -> Bool
verticalOrHorizontal (LineSegment x1 y1 x2 y2) =
    x1 == x2 || y1 == y2

getPoints :: LineSegment -> [Point]
getPoints (LineSegment x1 y1 x2 y2) =
    let dx = if y2 - y1 == 0 then signum $ x2 - x1 else x2 - x1
        dy = if x2 - x1 == 0 then signum $ y2 - y1 else y2 - y1
        dgcd = gcd dy dx
        dX = dx `div` dgcd
        dY = dy `div` dgcd
        unfolder (x, y) =
            if x == x2 + dX && y == y2 + dY
            then Nothing
            else Just ((x, y), (x + dX, y + dY))
    in  unfoldr unfolder (x1, y1)

plotSegments :: Map Point Int -> LineSegment -> Map Point Int
plotSegments pointCounts =
    foldl' (\pcs point -> insertWith (+) point 1 pcs) pointCounts . getPoints

countOverlaps :: [LineSegment] -> Int
countOverlaps segments =
    let pointCounts = foldl' plotSegments Map.empty segments
    in Map.foldr (\count sum -> if count >= 2 then sum + 1 else sum) 0 pointCounts

main :: IO ()
main = do
    segmentsParse <- sequence . map (runParser parseLineSegment "") . map pack . lines <$> readFile "input.txt"
    putStrLn "Part 1: "
    let segments = either (error "parse error :(") id segmentsParse
    print . countOverlaps $ filter verticalOrHorizontal segments
    putStrLn "Part 2: "
    print $ countOverlaps segments
