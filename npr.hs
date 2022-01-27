-- import Data.List
-- import Control.Applicative
import System.Environment (getArgs)
import Data.Char (digitToInt,toUpper)
import Data.List (isInfixOf,isPrefixOf)
-- slovePRN  :: String -> Float
-- slovePRN  = head . foldl foldingFucction [] .words
--     where foldingFucction (x:y:ys) "*" = (x * y):ys
--           foldingFucction (x:y:ys) "+" = (x + y):ys
--           foldingFucction (x:y:ys) "-" = (x - y):ys
--           foldingFucction (x:y:ys) "/" = (x / y):ys
--           foldingFucction (x:y:ys) "^" = (x ** y):ys
--           foldingFucction (x:xs) "ln" = log x :xs
--           foldingFucction xs "sum" = [sum xs]
--           foldingFucction xs numberString = read numberString: xs



-- f1 :: Int -> Int -> Int
-- f1 x y = 2 *x + y
-- main = do
--     print(show $ fmap f1  (Just 1) <*> (Just 2))




data MList a  =
              MCons a (MList a)
              | MNil deriving (Show)




-- fromList :: [a] -> MList a
-- fromList (x:xs) = MCons x (fromList xs)
-- fromList []     = MNil



pluralise word  = map plural
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"



interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith  function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = id





asInt = loop 0

loop :: Int -> String -> Int
loop = foldl (\ acc x -> acc * 10 + digitToInt x)


upperCase = map toUpper

isInAny needle = any (needle `isInfixOf`)





dlts :: String -> [String]
dlts = foldr step [] . lines
    where step l ds
            | "#define DLT_" `isPrefixOf` l = secondWord l : ds
            | otherwise                     = ds
          secondWord = head . tail . words