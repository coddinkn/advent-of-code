import System.Environment
import Data.List.Split

toThreeTuple :: [String] -> (Int, Int, Int)
toThreeTuple (x:y:z:[]) = (read x, read y, read z)

formatDimensions :: String -> (Int, Int, Int)
formatDimensions s = toThreeTuple $ splitOn "x" s

computeSqFootage :: (Int, Int, Int) -> Int
computeSqFootage (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l) + (smallestSide (l, w, h)) 

computeRibbon :: (Int, Int, Int) -> Int
computeRibbon (l, w, h) = (l*w*h) + (smallestWrap (l, w, h))

smallestWrap :: (Int, Int, Int) -> Int
smallestWrap (l, w, h) = minimum ((l+w+l+w):(w+h+w+h):(h+l+h+l):[])

smallestSide :: (Int, Int, Int) -> Int
smallestSide (l, w, h) = minimum ((l*w):(w*h):(h*l):[])

computeTotalSqFootage :: String -> Int
computeTotalSqFootage = sum . map computeSqFootage . map formatDimensions . words 

computeTotalRibbon :: String -> Int
computeTotalRibbon = sum . map computeRibbon . map formatDimensions. words

main :: IO ()
main = do 
    args <- getArgs
    input <- readFile (args !! 0)
    putStrLn $ show $ computeTotalSqFootage input 
    putStrLn $ show $ computeTotalRibbon input 
