module Computer (
    compute
) where

import Prelude hiding (Read)
import Control.Monad.RWS
import Data.List

data Program = Program { instructionPointer :: Int
                       , inputPointer :: Int
                       , memory :: [Int]
                       } deriving (Eq, Show)

newProgram :: [Int] -> Program
newProgram = Program 0 0

type Computer = RWS [Int] [Int] Program

data ParamType = Read
               | Write

data Mode = Position
          | Immediate
          deriving (Eq, Show)

data Instruction = Add      Mode Mode Mode
                 | Multiply Mode Mode Mode
                 | Input  Mode
                 | Output Mode
                 | JumpIfTrue  Mode Mode
                 | JumpIfFalse Mode Mode
                 | LessThan Mode Mode Mode
                 | Equals   Mode Mode Mode
                 | End
                 deriving (Eq, Show)

input :: Computer Int
input = do inpPt <- gets inputPointer
           modify $ \program -> program { inputPointer = inpPt + 1 }
           flip (!!) inpPt <$> ask

setInstructionPointer :: Int -> Computer ()
setInstructionPointer ip = modify $ \program -> program { instructionPointer = ip } 

next :: Computer Int
next = do ip <- gets instructionPointer
          setInstructionPointer $ ip + 1
          flip (!!) ip <$> gets memory

parseMode :: Int -> Mode
parseMode 0 = Position 
parseMode 1 = Immediate
parseMode bad = error $ "bad mode " ++ show bad

parseOpcode :: Int -> (Int, [Mode])
parseOpcode opcode = let digits = map read . map pure $ show opcode 
                     in if length digits < 3
                           then (opcode, map parseMode $ repeat 0)
                           else let ones:tens:rest = reverse digits
                                    operation = tens * 10 + ones
                                    paramModes = map parseMode $ rest ++ repeat 0   
                                in (operation, paramModes)

parseInstruction :: Int -> Instruction
parseInstruction opcode =
    let (operation, modes) = parseOpcode opcode
    in case operation of
            1  -> Add      (modes !! 0) (modes !! 1) (modes !! 2)
            2  -> Multiply (modes !! 0) (modes !! 1) (modes !! 2)
            3  -> Input    (modes !! 0)
            4  -> Output   (modes !! 0)
            5  -> JumpIfTrue  (modes !! 0) (modes !! 1)
            6  -> JumpIfFalse (modes !! 0) (modes !! 1)
            7  -> LessThan (modes !! 0) (modes !! 1) (modes !! 2)
            8  -> Equals   (modes !! 0) (modes !! 1) (modes !! 2)
            99 -> End
            bad -> error $ "bad opcode " ++ show bad

getParam :: ParamType -> Mode -> Computer Int
getParam paramType mode = case (paramType, mode) of
                               (Read, Position)   -> next >>= getValue
                               (Read, Immediate)  -> next
                               (Write, Position)  -> next
                               (Write, Immediate) -> error "Write parameter Mode not Position"

getValue :: Int -> Computer Int
getValue position = flip (!!) position <$> gets memory

putValue :: Int -> Int -> Computer ()
putValue position value = do (l, r) <- splitAt (position + 1) <$> gets memory
                             let newMemory = take position l ++ pure value ++ r
                             modify $ \program -> program { memory = newMemory } 

processInstruction :: Instruction -> Computer Bool
processInstruction instruction = 
             case instruction of 

                  Add mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                              param2 <- getParam Read  mode2
                                              resPos <- getParam Write mode3
                                              putValue resPos $ param1 + param2
                                              return True

                  Multiply mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ param1 * param2
                                                   return True

                  Input mode  -> do resPos <- getParam Write mode 
                                    input >>= putValue resPos
                                    return True

                  Output mode -> do param <- getParam Read mode
                                    tell $ pure param
                                    return True

                  JumpIfTrue mode1 mode2  -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test /= 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                return True
                  
                  JumpIfFalse mode1 mode2 -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test == 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                return True

                  LessThan mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 < param2
                                                                     then 1
                                                                     else 0
                                                   return True

                  Equals mode1 mode2 mode3   -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 == param2
                                                                     then 1
                                                                     else 0
                                                   return True

                  End -> return False

process :: Computer Int
process = do instruction <- parseInstruction <$> next
             continue <- processInstruction instruction
             if continue 
                then process
                else getValue 0

compute :: [Int] -> [Int] -> Int
compute memory input = last . snd $ evalRWS process input $ newProgram memory 
