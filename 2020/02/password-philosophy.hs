import Prelude hiding (maximum, minimum)

type Password = String

data Policy = Policy { letter  :: Char
                     , minimum :: Int
                     , maximum :: Int
                     } deriving Show

parse :: String -> (Password, Policy)
parse str =
    let min = read $ takeWhile (/= '-') str
        max = read . takeWhile (/= ' ') . drop 1 $ dropWhile (/= '-') str
        char = head . drop 1 $ dropWhile (/= ' ') str
        password = drop 2 $ dropWhile (/= ':') str
    in (password, Policy char min max)

validate :: Password -> Policy -> Bool
validate password policy =
    let count = sum $ map (\c -> if c == (letter policy) then 1 else 0) password
    in count >= (minimum policy) && count <= (maximum policy)

validate' :: Password -> Policy -> Bool
validate' password policy =
    let first  = (password !! (minimum policy - 1)) == (letter policy)
        second = (password !! (maximum policy - 1)) == (letter policy)
    in (first || second) && (not (first && second))

main :: IO ()
main = do
    passwords <- map parse . lines <$> readFile "input.txt"
    putStr "Part 1: "
    print . length $ filter (uncurry validate) passwords
    putStr "Part 2: "
    print . length $ filter (uncurry validate') passwords
