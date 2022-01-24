-- main = putStrLn "hello world"
import Data.Char
import Control.Monad
import System.IO
-- main = do
--   putStrLn "Hello,what's your name?"
--   name <- getLine
--   putStrLn ("Hey " ++ name ++ ", you rock!")

--
-- main = do
--       putStrLn "What's your first name?"
--       firstName <- getLine
--       putStrLn "What's your last name?"
--       lastName <- getLine
--       let bigFirstName = map toUpper  firstName
--           bigLastName = map toUpper lastName
--       putStrLn $ "hey " ++ bigFirstName ++ " "  ++ bigLastName ++ ", how are you?"

--
-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--       putChar c
--       main

-- main = do
--   rs <- sequence [getLine,getLine,getLine]
--   print rs


-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

-- main = do
--   colors <- forM [1,2,3,4] (\a -> do
--       putStrLn $ "Which color do you associate with the number "++ show a ++ "?"
--       color <- getLine
--       return color
--     )
--   putStrLn "The colors that you associate witch 1,2,3 and 4 are : "
--   mapM putStrLn colors

--
-- main = do
--   contents <- getContents
--   putStr (map toUpper contents)


-- main = do
--   contents <- getContents
--   putStr (shortLinesOnly contents)


-- main = interact shortLinesOnly
-- main = interact $ unlines . filter ((<10) .length) .lines

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines  shortLines
    in result


respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs


-- main = interact $ respondPalindromes


-- main = do
--   handle <- openFile "Shape.hs" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

main =do
  contents <- readFile "Shape.hs"
  -- putStr contents
  writeFile "shape.hs.copy" (map toUpper contents)
