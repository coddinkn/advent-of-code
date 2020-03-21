module Process where

import Prelude hiding (Read, error)
import Control.Monad.RWS
import Control.Monad.Except
import Data.List

data Program = Program { instructionPointer :: Int
                       , inputPointer :: Int
                       , outputPointer :: Int
                       , memory :: [Int]
                       } deriving (Eq, Show)

newProgram :: [Int] -> Program
newProgram = Program 0 0 0

data ProcessState = WaitingForInput
                  | Error String
                  deriving (Eq, Show)

error :: String -> Process a
error message = throwError $ Error message  

input :: Process Int
input = do inpPt <- gets inputPointer
           inputLength <- length <$> ask
           if inpPt >= inputLength
               then throwError WaitingForInput 
               else do modify $ \program -> program { inputPointer = inpPt + 1 }
                       flip (!!) inpPt <$> ask

type Process = ExceptT ProcessState (RWS [Int] [Int] Program)

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

setInstructionPointer :: Int -> Process ()
setInstructionPointer ip = modify $ \program -> program { instructionPointer = ip } 

next :: Process Int
next = do ip <- gets instructionPointer
          setInstructionPointer $ ip + 1
          flip (!!) ip <$> gets memory

parseMode :: Int -> Process Mode
parseMode 0 = return Position 
parseMode 1 = return Immediate
parseMode bad = error $ "bad mode " ++ show bad

parseOperation :: Int -> Process Operation
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

parseInstruction :: Int -> Process Instruction
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

getNextInstruction :: Process Instruction
getNextInstruction = next >>= parseInstruction

getParam :: ParamType -> Mode -> Process Int
getParam paramType mode = case (paramType, mode) of
                               (Read, Position)   -> next >>= getValue
                               (Read, Immediate)  -> next
                               (Write, Position)  -> next
                               (Write, Immediate) -> error "Write parameter Mode not Position"

getValue :: Int -> Process Int
getValue position = flip (!!) position <$> gets memory

putValue :: Int -> Int -> Process ()
putValue position value = do (l, r) <- splitAt (position + 1) <$> gets memory
                             let newMemory = take position l ++ pure value ++ r
                             modify $ \program -> program { memory = newMemory } 

process :: Process ()
process = do instruction <- getNextInstruction
             case instruction of 

                  Add mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                              param2 <- getParam Read  mode2
                                              resPos <- getParam Write mode3
                                              putValue resPos $ param1 + param2
                                              process

                  Multiply mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ param1 * param2
                                                   process

                  Input mode  -> do resPos <- getParam Write mode 
                                    input >>= putValue resPos
                                    process

                  Output mode -> do param <- getParam Read mode
                                    tell $ pure param
                                    process

                  JumpIfTrue mode1 mode2  -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test /= 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                process
                  
                  JumpIfFalse mode1 mode2 -> do test <- getParam Read mode1
                                                ip <- getParam Read mode2
                                                if test == 0
                                                   then setInstructionPointer ip
                                                   else return ()
                                                process

                  LessThan mode1 mode2 mode3 -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 < param2
                                                                     then 1
                                                                     else 0
                                                   process

                  Equals mode1 mode2 mode3   -> do param1 <- getParam Read  mode1
                                                   param2 <- getParam Read  mode2
                                                   resPos <- getParam Write mode3
                                                   putValue resPos $ if param1 == param2
                                                                     then 1
                                                                     else 0
                                                   process

                  End -> return ()


runProgram :: [Int] -> [Int] -> (Maybe ProcessState, Program, [Int])
runProgram memory input = let (result, program, output) = runRWS (runExceptT process) input $ newProgram memory
                              maybeProcessState = either Just (const Nothing) result
                          in (maybeProcessState, program, output)       
