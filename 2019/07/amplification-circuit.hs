import Data.List (permutations)

import Computer

linearAmplifier :: [Int] -> [Int] -> Int
linearAmplifier memory modes = let program = newProgram memory
                               in either error id . evalComputer $ 
    do file <- create "amplifier"
       write file [0]
       let runAmplifier mode = do prepend file [mode]
                                  exec program file >>= write file 
       sequence_ $ runAmplifier <$> modes                         
       last <$> Computer.read file

loopAmplifier :: [Int] -> [Int] -> Int
loopAmplifier memory [a, b, c, d, e] = let program = newProgram memory
                                       in either error id . evalComputer $
    do fileA <- create "A"
       write fileA [a, 0] 
       fileB <- create "B"
       write fileB [b]
       fileC <- create "C"
       write fileC [c]
       fileD <- create "D"
       write fileD [d]
       fileE <- create "E"
       write fileE [e]
       fork program fileA fileB
       fork program fileB fileC
       fork program fileC fileD
       fork program fileD fileE
       fork program fileE fileA
       wait
       last <$> Computer.read fileA

maxOutput :: [Int] -> ([Int] -> [Int] -> Int) -> [Int] -> Int
maxOutput memory amplifier modeOptions = maximum $ amplifier memory <$> permutations modeOptions

main :: IO ()
main = do memory <- map Prelude.read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "input.txt"
          putStr "Part 1: "
          print $ maxOutput memory linearAmplifier [0 .. 4]
          putStr "Part 2: "
          print $ maxOutput memory loopAmplifier [5 .. 9]
