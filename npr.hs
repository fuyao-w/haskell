import Data.List
import Control.Applicative


slovePRN  :: String -> Float
slovePRN  = head . foldl foldingFucction [] .words
    where foldingFucction (x:y:ys) "*" = (x * y):ys
          foldingFucction (x:y:ys) "+" = (x + y):ys
          foldingFucction (x:y:ys) "-" = (x - y):ys
          foldingFucction (x:y:ys) "/" = (x / y):ys
          foldingFucction (x:y:ys) "^" = (x ** y):ys
          foldingFucction (x:xs) "ln" = log x :xs
          foldingFucction xs "sum" = [sum xs]
          foldingFucction xs numberString = read numberString: xs



f1 :: Int -> Int -> Int
f1 x y = 2 *x + y
main = do
    print(show $ fmap f1  (Just 1) <*> (Just 2))
