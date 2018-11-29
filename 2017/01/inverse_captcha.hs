import Data.Char

type Captcha = String

-- Takes a captcha and a match offset and solves the captcha
solve :: Captcha -> Int -> Int

solve captcha offset = sum $ map fst $ filter (\(a, b) -> a == b) $ zipped
    where zipped  = zip ints rotated
          ints    = map digitToInt captcha
          l       = length captcha
          rotated = drop offset $ take (l + offset) $ cycle ints 
