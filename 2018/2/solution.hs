import Data.List (group, sort, uncons)

checkId id = (hasExactlyTwo, hasExactlyThree)
    where numberOfEachLetter = map length $ group $ sort id
          hasExactlyTwo   = 2 `elem` numberOfEachLetter
          hasExactlyThree = 3 `elem` numberOfEachLetter

partOne ids = twos * threes
    where (hasTwos, hasThrees) = unzip $ map checkId ids
          count = length . filter id
          (twos, threes) = (count hasTwos, count hasThrees)

close id1 id2 = numDifferent == 1
    where numDifferent = length $ filter not $ zipWith (==) id1 id2 

commonLetters id1 id2 = map fst $ filter (uncurry (==)) $ zip id1 id2

getCloseCommon id1 id2 = if close id1 id2
                         then commonLetters id1 id2
                         else ""

partTwo :: [String] -> Maybe String
partTwo ids = (fst <$>) $ uncons closeLetters 
    where closeLetters = filter (not . null) $ getCloseCommon <$> ids <*> ids 

main = do ids <- lines <$> readFile "input.txt"
          print $ partOne ids
          print $ case partTwo ids of
                       Just letters -> letters
                       Nothing      -> "no common letters"
