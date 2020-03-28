module Program
( Program
, newProgram
, runProgram    
, ExecutionState(..)
) where

import Prelude hiding (Read, error)
import Control.Monad.RWS
import Control.Monad.Except
import Data.List

data Program = Program { instructionPointer :: Int
                       , inputPointer :: Int
                       , memory :: [Int]
                       } deriving (Eq, Show)

newProgram :: [Int] -> Program
newProgram = Program 0 0

data ExecutionState = WaitingForInput
                    | Error String
                    deriving (Eq, Show)

error :: String -> Execution a
error message = throwError $ Error message  

input :: Execution Int
input = do inpPt <- gets inputPointer
           inputLength <- length <$> ask
           if inpPt >= inputLength
              then do ip <- gets instructionPointer
                      setInstructionPointer $ ip - 2
                      throwError WaitingForInput 
              else do modify $ \program -> program { inputPointer = inpPt + 1 }
                      flip (!!) inpPt <$> ask

type Execution = ExceptT ExecutionState (RWS [Int] [Int] Program)

data ParamType = Read
               | Write

data Mode = Position
          | Immediate
          deriving (Eq, Show)

data Operation = AddOp
               | MultiplyOp
               | InputOp
               | OutputOp
               | JumpIfTrueOp
               | JumpIfFalseOp
               | LessThanOp
               | EqualsOp
               | EndOp

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

setInstructionPointer :: Int -> Execution ()
setInstructionPointer ip = modify $ \program -> program { instructionPointer = ip } 

next :: Execution Int
next = do ip <- gets instructionPointer
          setInstructionPointer $ ip + 1
          flip (!!) ip <$> gets memory

parseMode :: Int -> Execution Mode
parseMode 0 = return Position 
parseMode 1 = return Immediate
parseMode bad = error $ "bad mode " ++ show bad

parseOperation :: Int -> Execution Operation
parseOperation operation =
    case operation of
         1  -> return AddOp
         2  -> return MultiplyOp
         3  -> return InputOp
         4  -> return OutputOp
         5  -> return JumpIfTrueOp
         6  -> return JumpIfFalseOp
         7  -> return LessThanOp
         8  -> return EqualsOp
         99 -> return EndOp
         bad -> error $ "bad operation " ++ show bad

arity :: Operation -> Int
arity operation = 
    case operation of
         AddOp ->  3
         MultiplyOp ->  3
         InputOp ->  1
         OutputOp ->  1
         JumpIfTrueOp ->  2
         JumpIfFalseOp ->  2
         LessThanOp ->  3
         EqualsOp ->  3
         EndOp ->  0

makeInstruction :: Operation -> [Mode] -> Instruction
makeInstruction operation modes =
       case operation of
            AddOp      -> Add      (modes !! 0) (modes !! 1) (modes !! 2)
            MultiplyOp -> Multiply (modes !! 0) (modes !! 1) (modes !! 2)
            InputOp  -> Input    (modes !! 0)
            OutputOp -> Output   (modes !! 0)
            JumpIfTrueOp  -> JumpIfTrue  (modes !! 0) (modes !! 1)
            JumpIfFalseOp -> JumpIfFalse (modes !! 0) (modes !! 1)
            LessThanOp -> LessThan (modes !! 0) (modes !! 1) (modes !! 2)
            EqualsOp   -> Equals   (modes !! 0) (modes !! 1) (modes !! 2)
            EndOp -> End

parseInstruction :: Int -> Execution Instruction
parseInstruction opcode =
    uncurry makeInstruction <$> if length digits == 1
                                then do operation <- parseOperation $ head digits
                                        modes <- mapM parseMode $ take (arity operation) $ repeat 0
                                        return (operation, modes)
                                else do let ones:tens:rest = reverse digits
                                        operation <- parseOperation $ tens * 10 + ones
                                        modes <- mapM parseMode $ (++) rest $ take (arity operation) $ repeat 0
                                        return (operation, modes)
    where digits = map read . map pure $ show opcode 

getNextInstruction :: Execution Instruction
getNextInstruction = next >>= parseInstruction

getParam :: ParamType -> Mode -> Execution Int
getParam paramType mode = case (paramType, mode) of
                               (Read, Position)   -> next >>= getValue
                               (Read, Immediate)  -> next
                               (Write, Position)  -> next
                               (Write, Immediate) -> error "Write parameter Mode not Position"

getValue :: Int -> Execution Int
getValue position = flip (!!) position <$> gets memory

putValue :: Int -> Int -> Execution ()
putValue position value = do (l, r) <- splitAt (position + 1) <$> gets memory
                             let newMemory = take position l ++ pure value ++ r
                             modify $ \program -> program { memory = newMemory } 

execute :: Execution ()
execute = do instruction <- getNextInstruction
             case instruction of 

                  Add mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                              param2 <- getParam Read  mode2
                                              resPos <- getParam Write mode3
                                              putValue resPos $ param1 + param2
                                              execute

                  Multiply mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ param1 * param2
                                                   execute

                  Input mode  -> do resPos <- getParam Write mode 
                                    input >>= putValue resPos
                                    execute

                  Output mode -> do param <- getParam Read mode
                                    tell $ pure param
                                    execute

                  JumpIfTrue mode1 mode2  -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test /= 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                execute
                  
                  JumpIfFalse mode1 mode2 -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test == 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                execute

                  LessThan mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 < param2
                                                                     then 1
                                                                     else 0
                                                   execute

                  Equals mode1 mode2 mode3   -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 == param2
                                                                     then 1
                                                                     else 0
                                                   execute

                  End -> return ()


runProgram :: Program -> [Int] -> (Maybe ExecutionState, Program, [Int])
runProgram program input = let (result, nextProgram, output) = runRWS (runExceptT execute) input program
                               maybeExecutionState = either Just (const Nothing) result
                           in (maybeExecutionState, nextProgram, output)       
