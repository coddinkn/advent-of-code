import Data.List.Split

dimension :: String -> (Int, Int, Int)
dimension s = (read l, read w, read h) 
    where l:w:h:[] = splitOn "x" s

surfaceArea (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l) + (smallestSide (l, w, h)) 

smallestSide (l, w, h) = minimum [side1, side2, side3]
    where side1 = l * w
          side2 = w * h
          side3 = l * h

partOne = sum . map surfaceArea

ribbon dimensions = (+bow) $ smallestWrap dimensions
    where (l, w, h) = dimensions
          bow = l * w * h

smallestWrap (l, w, h) = 2 * minimum [face1, face2, face3] 
    where face1 = l + w
          face2 = w + h
          face3 = l + h

partTwo = sum . map ribbon 

main = do dimensions <- map dimension . words <$> readFile "input.txt"
          print $ partOne dimensions 
          print $ partTwo dimensions
