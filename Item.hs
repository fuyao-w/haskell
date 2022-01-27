import System.IO
import System.Directory
import Data.List
import System.Environment
import Data.Sequence.Internal.Sorting (QList(Nil))
import Data.Sequence (Seq(Empty))
import qualified Data.Sequence.Internal.Sorting as Main

-- main do
--   todoItem <- getLine
--   appendFile "todo.txt" (todoItem ++ "\n")
--
-- main_1 = do
--   handle <- openFile "todo.txt" ReadMode
--   (tempName,tempHandle) <- openTempFile "." "temp"
--   contents <- hGetContents handle
--   let todoTasks = lines contents
--       numberedTasks = zipWith (\n line -> show (n+1) ++ " - " ++ line) [0..] todoTasks
--   putStrLn "这些是你的 TODO 清单："
--   putStr $ unlines numberedTasks
--   putStrLn "你想删除那一条内容？"
--   numberString <- getLine
--   let number = read numberString
--       newTodoItems = delete (todoTasks !! number) todoTasks
--   hPutStr tempHandle $ unlines newTodoItems
--   hClose handle
--   hClose tempHandle
--   removeFile "todo.txt"
--   renameFile tempName "todo.txt"
--
--
--
-- main = do
--   args <- getArgs
--   progName <- getProgName
--   putStrLn "参数是："
--   mapM putStrLn args
--   putStrLn "程序名称是："
--   putStrLn progName
--

dispatch :: [(String,[String] -> IO ())]
dispatch = [("add",add),("view",view),("remove",remove)]



main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show (n+1) ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks


remove :: [String] -> IO ()
remove [fileName,numberString] = do
  handle <- openFile fileName ReadMode
  (tempName,tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString - 1
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName



-- main = interact wordCount  where 
    -- wordCount input = show (length (lines input)) ++\n --  wordCount input = 是函数定义，后面是实现


myDrop n xs = if n <= 0 || null xs 
              then xs 
              else myDrop (n-1) (tail xs)


data Profile = ProductManager  {
    name :: String 
}| Chengxuyuan deriving (Show)


goodExapmle (x:xs) = x + goodExapmle xs
goodExapmle _ = 0
