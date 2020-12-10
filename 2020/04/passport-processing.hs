import Data.List (intercalate, sort, isSubsequenceOf, find, reverse)
import Data.List.Split (splitWhen)
import Data.Char (isDigit, isAlpha, isLower)
import Data.Maybe (catMaybes)

type Passport = [(String, String)]

parseProperty :: String -> (String, String)
parseProperty string =
    let key = takeWhile (/= ':') string
        value = drop 1 $ dropWhile (/= ':') string
    in (key, value)

parsePassport :: [String] -> Passport
parsePassport strings =
    let properties = words $ intercalate " " strings
    in parseProperty <$> properties

parsePassports :: [String] -> [Passport]
parsePassports strings =
    let passportStrings = splitWhen (== "") strings
    in parsePassport <$> passportStrings

getProperties :: Passport -> [String]
getProperties = map fst

requiredProperties :: [String]
requiredProperties = [ "byr"
                     , "iyr"
                     , "eyr"
                     , "hgt"
                     , "hcl"
                     , "ecl"
                     , "pid"
                     ]

valid :: Passport -> Bool
valid passport = (sort requiredProperties) `isSubsequenceOf` (sort $ getProperties passport)

data Passport' =
    Passport'
        { birthYear :: Int
        , issueYear :: Int
        , expirationYear :: Int
        , height :: Int
        , hairColor :: String
        , eyeColor :: String
        , passportId :: Int
        } deriving Show

get :: Passport -> String -> Maybe String
get passport key = snd <$> find (\prop -> fst prop == key) passport

parseInt :: String -> Int -> Int -> Passport -> Maybe Int
parseInt key lower upper passport = do
    value <- read <$> passport `get` key
    if value >= lower && value <= upper
    then return value
    else Nothing

parseBirthYear :: Passport -> Maybe Int
parseBirthYear = parseInt "byr" 1920 2002

parseIssueYear :: Passport -> Maybe Int
parseIssueYear = parseInt "iyr" 2010 2020

parseExpirationYear :: Passport -> Maybe Int
parseExpirationYear = parseInt "eyr" 2020 2030

parseHeight :: Passport -> Maybe Int
parseHeight passport = do
    height <- passport `get` "hgt"
    let value = read $ takeWhile isDigit height
        unit = dropWhile isDigit height
    case unit of
        "cm" -> if value >= 150 && value <= 193
                then return value
                else Nothing
        "in" -> if value >= 59 && value <= 76
                then return value
                else Nothing
        _ -> Nothing

parseHairColor :: Passport -> Maybe String
parseHairColor passport = do
    hairColor <- passport `get` "hcl"
    case hairColor of
        '#':rest ->
            if and $ map (\c -> isDigit c || (isAlpha c && isLower c)) rest
            then return rest
            else Nothing
        _ -> Nothing

validEyeColors :: [String]
validEyeColors =
    [ "amb"
    , "blu"
    , "brn"
    , "gry"
    , "grn"
    , "hzl"
    , "oth"
    ]

parseEyeColor :: Passport -> Maybe String
parseEyeColor passport = do
    eyeColor <- passport `get` "ecl"
    if eyeColor `elem` validEyeColors
    then return eyeColor
    else Nothing

parsePassportId :: Passport -> Maybe Int
parsePassportId passport = do
    passportId <- passport `get` "pid" 
    if length passportId == 9 && (and $ map isDigit passportId)
    then return $ read passportId
    else Nothing

parse' :: Passport -> Maybe Passport'
parse' passport =
    Passport' <$>
        parseBirthYear passport <*>
        parseIssueYear passport <*>
        parseExpirationYear passport <*>
        parseHeight passport <*>
        parseHairColor passport <*>
        parseEyeColor passport <*>
        parsePassportId passport

main :: IO ()
main = do
   passports <- parsePassports . lines <$> readFile "input.txt"
   putStr "Part 1: "
   print . length $ filter valid passports 
   putStr "Part 2: "
   print . length . catMaybes . (map parse') $ filter valid passports
