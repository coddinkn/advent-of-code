module Computer 
( Computer,
  exec,
  wait,
  fork,
  create,
  destroy,
  append,
  prepend,
  read,
  write,
  clear,
  evalComputer,
  runComputer,
  module Program
) where

import Prelude hiding (lookup, read)

import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Map

import Program

data File = File { name :: String } deriving (Show, Eq, Ord)

data Process = Process { program :: Program
                       , input   :: File
                       , output  :: File
                       , state   :: Maybe ExecutionState
                       } deriving Show

type Computer = ExceptT String (State ([Process], Map File [Int]))

removeFile :: File -> ([Process], Map File [Int]) -> ([Process], Map File [Int])
removeFile file (processes, filesystem) = (processes, delete file filesystem)

changeData :: File -> [Int] -> ([Process], Map File [Int]) -> ([Process], Map File [Int])
changeData file contents (processes, filesystem) = (processes, insert file contents filesystem)

verifyFileExists :: File -> Computer ()
verifyFileExists file = do exists <- member file <$> gets snd
                           if exists
                              then return ()
                              else throwError $ "no such file: " ++ name file

create :: String -> Computer File
create fileName = let file = File fileName
                  in do modify $ changeData file []
                        return file

read :: File -> Computer [Int]
read file = do result <- lookup file <$> gets snd
               case result of
                    Just contents -> return contents
                    Nothing -> throwError $ "no such file: " ++ name file

write :: File -> [Int] -> Computer ()
write file contents = do verifyFileExists file
                         modify $ changeData file contents

clear :: File -> Computer ()
clear file = do verifyFileExists file
                modify $ changeData file []

destroy :: File -> Computer ()
destroy file = do verifyFileExists file
                  modify $ removeFile file

append :: File -> [Int] -> Computer ()
append file addition =
    do result <- lookup file <$> gets snd
       case result of
            Just contents -> modify $ changeData file $ contents ++ addition
            Nothing -> throwError $ "no such file: " ++ name file

prepend :: File -> [Int] -> Computer ()
prepend file addition =
    do result <- lookup file <$> gets snd
       case result of
            Just contents -> modify $ changeData file $ addition ++ contents
            Nothing -> throwError $ "no such file: " ++ name file

fork :: Program -> File -> File -> Computer ()
fork program input output = modify $ \(processes, filesystem) -> (processes ++ [Process program input output Nothing], filesystem)

step :: Process -> Computer (Maybe Process)
step (Process program input output _) =
    do inputData <- read input
       let (maybeState, newProgram, outputData) = runProgram program inputData
       append output outputData
       return $ (\state -> Process newProgram input output (Just state)) <$> maybeState

wait :: Computer () 
wait = do processes <- gets fst
          if length processes == 0
             then return ()
             else do let process:rest = processes
                     case state process of
                          Just state -> case state of
                                             WaitingForInput -> return ()
                                             Error message -> throwError message
                          Nothing -> return ()
                     maybeNextProcess <- step process
                     let nextProcesses = case maybeNextProcess of
                                              Just nextProcess -> rest ++ [nextProcess]
                                              Nothing -> rest
                     modify $ \(_, filesystem) -> (nextProcesses, filesystem)
                     wait

exec :: Program -> File -> Computer [Int]
exec program input = do inputData <- read input
                        let (finalState, finalProgram, outputData) = runProgram program inputData
                        case finalState of
                             Just state -> throwError $ case state of
                                                             WaitingForInput -> "insufficient input data"
                                                             Error message -> message
                             Nothing -> return outputData

evalComputer :: Computer a -> Either String a
evalComputer = flip evalState ([], empty) . runExceptT

runComputer :: Computer a -> (Either String a, ([Process], Map File [Int]))
runComputer = flip runState ([], empty) . runExceptT
