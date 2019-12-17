requiredFuel :: Integer -> Integer
requiredFuel mass = (mass `div` 3) - 2

recursiveRequiredFuel :: Integer -> Integer
recursiveRequiredFuel mass = let fuel = requiredFuel mass
                             in if fuel <= 0
                                then 0
                                else fuel + recursiveRequiredFuel fuel

main :: IO ()
main = do modules <- map read . lines <$> readFile "input.txt"
          putStr "Part 1: "
          print . sum $ requiredFuel <$> modules
          putStr "Part 2: "
          print . sum $ recursiveRequiredFuel <$> modules
