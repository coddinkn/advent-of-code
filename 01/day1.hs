perenthesisToChange :: Char -> Int
perenthesisToChange p = if p == ')'
    then -1
    else 1

zeroPosition :: Int -> [Int] -> Int -> Int
zeroPosition 0 _ p = p
zeroPosition f (x:xs) p = zeroPosition (f + x) xs (p + 1)  
