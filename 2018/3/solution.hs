import Data.List.Split (splitOn)
import Data.List (group, sort)

--                 Id# (x,     y) (w,     h)
data Claim = Claim Int (Int, Int) (Int, Int)
    deriving (Show)

getId (Claim id _ _) = id

getPos (Claim _ pos _) = pos

getDim (Claim _ _ dim) = dim

parseClaim string = Claim id pos dim
    where idStr:_:posStr:dimStr:_ = words string
          id = read $ tail idStr
          xStr:yStr:_ = splitOn "," $ init $ posStr
          wStr:hStr:_ = splitOn "x" $ dimStr
          pos = (read xStr, read yStr)
          dim = (read wStr, read hStr)

getPoints :: Claim -> [(Int, Int)]
getPoints (Claim _ (x, y) (w, h)) = (,) <$> [x + 1 .. x + w] <*> [ y + 1 .. y + h]

partOne = length . filter (>1) . map length . group . sort . (=<<) getPoints

collision claim1 claim2 = x1 + w1 > x2
                       && x1 < x2 + w2
                       && y1 < y2 + h2
                       && y1 + h1 > y2
    where (x1, y1) = getPos claim1
          (x2, y2) = getPos claim2
          (w1, h1) = getDim claim1
          (w2, h2) = getDim claim2

noCollisions claims claim = (1==) $ length $ filter id $ map (collision claim) claims

partTwo claims = case filter (noCollisions claims) claims of
                      claim:_   -> Just claim
                      _ -> Nothing

main = do claims <- map parseClaim . lines <$> readFile "input.txt"           
          print $ partOne claims
          putStrLn $ case partTwo claims of
                       Just claim -> show $ getId claim
                       Nothing    -> "no such claim"
