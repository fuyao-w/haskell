module Main where

import System.IO
import System.Directory
import Data.List
import System.Environment ( getArgs )


main = do
    (command:args) <- getArgs
    let (Just action ) = lookup command dispatch
    action args


dispatch :: [(String,[String] -> IO())]    
dispatch = [ ("add",add) ,("view",view),("remove",remove)]


add  :: [String] -> IO()
add [] = do putStrLn  "得写点东西~"    
add [_] = do putStrLn  "缺少事项"
add [fileName,todoItem]
    | fileName == "" || todoItem == ""  = do putStrLn  "缺少参数"
    | fileName /= "" = appendFile fileName (todoItem ++ "\n")







view  :: [String] -> IO()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show (n+1) ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks



remove :: [String ]-> IO()
remove []  = do putStrLn  "缺少参数"
remove [_]  = do putStrLn  "缺少参数"
remove [fileName,numberString] 
    |   fileName == "" || numberString == ""  = do putStrLn  "缺少参数"
    |   fileName /= "" && numberString /= "" = 
        do
            handle <- openFile fileName ReadMode 
            (tempName,tempHandle) <- openTempFile "." "temp"
            contents <- hGetContents handle
            let number = read numberString - 1
                todoTasks = lines contents
                newTodoTtems = delete (todoTasks !! number) todoTasks --删除谋一句话
            hPutStr tempHandle $ unlines  newTodoTtems 
            hClose handle
            hClose  tempHandle
            removeFile fileName
            renameFile tempName fileName






    
