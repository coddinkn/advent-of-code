import Data.List (group)

digitize :: Int -> [Int]
digitize = map read . map pure . show

doubleAdjacent :: Int -> Bool
doubleAdjacent = any (\group -> length group >= 2) . group . digitize

onlyDoubleAdjacent :: Int -> Bool
onlyDoubleAdjacent = any (\group -> length group == 2) . group . digitize

correctLength :: Int -> Bool
correctLength = (>= 6) . length . show

monotonic :: Int -> Bool
monotonic = check . digitize
    where check (a:b:rest) = if a > b then False else check (b:rest)
          check _ = True

main :: IO ()
main = do let input = [145852 .. 616942]
          putStr "Part 1: "
          print . length $ filter (\number -> correctLength number
                                           && doubleAdjacent number
                                           && monotonic number) input
          putStr "Part 2: "
          print . length $ filter (\number -> correctLength number
                                           && onlyDoubleAdjacent number
                                           && monotonic number) input
            
