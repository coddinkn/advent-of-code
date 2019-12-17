import Control.Monad.State
import Data.List

type Computer = State (Int, [Int])

data Instruction = Add Int Int Int
                 | Mul Int Int Int
                 | End
                 deriving (Eq, Show)

getTape :: Computer [Int]
getTape = gets snd

getProgramCounter :: Computer Int
getProgramCounter = gets fst

putProgramCounter :: Int -> Computer ()
putProgramCounter newPc = do tape <- getTape
                             put (newPc, tape)

next :: Computer Int
next = do pc <- getProgramCounter
          tape <- getTape
          putProgramCounter $ pc + 1 
          return $ tape !! pc

getInstruction :: Computer Instruction
getInstruction = do pc <- next
                    case pc of
                        1  -> Add <$> next <*> next <*> next
                        2  -> Mul <$> next <*> next <*> next
                        99 -> return End
                        _  -> error "Bad opcode!"

getValue :: Int -> Computer Int
getValue position = do tape <- getTape
                       return $ tape !! position

putValue :: Int -> Int -> Computer ()
putValue position value = do pc <- getProgramCounter
                             tape <- getTape
                             let (l, r)  = splitAt (position + 1) tape
                                 newTape = take position l ++ pure value ++ r
                             put (pc, newTape)

compute :: Computer Int
compute = do instruction <- getInstruction
             case instruction of 
               Add pos1 pos2 posR -> do arg1 <- getValue pos1
                                        arg2 <- getValue pos2
                                        let res = arg1 + arg2
                                        putValue posR res
                                        compute

               Mul pos1 pos2 posR -> do arg1 <- getValue pos1
                                        arg2 <- getValue pos2
                                        let res = arg1 * arg2
                                        putValue posR res
                                        compute 
               End -> getValue 0


computeWithTape tape = evalState compute (0, tape)

modifyTape :: [Int] -> (Int, Int) -> [Int]
modifyTape (x:_:_:xs) (a, b) = x:a:b:xs

computeWithModification :: [Int] -> (Int, Int) -> Int
computeWithModification tape modification = computeWithTape $ modifyTape tape modification

main :: IO ()
main = do tape <- map read . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "input.txt"
          putStr "Part 1: "
          print $ computeWithModification tape (12, 2)
          putStr "Part 2: "
          let pairs = (,) <$> [0..99] <*> [0..99]
          case find (\mod -> computeWithModification tape mod == 19690720) pairs of
                Just (noun, verb) -> print $ 100 * noun + verb
                Nothing -> error "Bad input?"
