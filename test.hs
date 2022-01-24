
doubleMe  x = x * 2

doubleUs x y = (doubleMe x) + (doubleMe y)

doubleSmallNumber x = if x > 100
  then x
  else x * 2

hello = "你好 Haskell"

lostNumbers = [1,2,3,4]

compareList = [1,3,3] > [1,3,2]

len = length [1,2,3,4]

getList = [1..20]

getA = ['a'..'z']

repName = take 30 (repeat "wfy")

listCom =  [x*x | x <- [1..10],x * 2 > 12]

boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs,odd x]

clac = [x* y | x <- [2,5,10],y <- [8,10,11]]

length' xs = sum [1 | _ <- xs]

tupleList = [("wfy",24),("lijiaqi",26)]

compute = zip [1,2,3,4] [5,6,7,8]

rightTriangles' ::  [(Int,Int,Int)]

rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c],a <- [1..b], c *c == a*a + b*b,a+b+c == 24]

factorial :: Integer -> Integer

factorial n = product [1..n]

circumference :: Float -> Float

circumference r = r * pi * r

-- lucky :: (Integral a) => a -> String

lucky 4 = "lucky number four"

lucky x = "not foundlu"


newFactorial :: (Integral a) => a -> a

newFactorial 0 = 1
newFactorial n = n * newFactorial(n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)

head' :: [a] -> a

head' [] = error "can't call head on an empty list "
head' (x:_) = x

first :: (a,b,c) -> a
first (x, _, _)=x

tell :: (Show a) => [a] ->String
tell [] = "the list is empty"
tell (x:[]) = "the list has one element " ++ show x
tell (x:y:[]) = "the list has two elements: " ++ show x ++ " and  " ++ show y
tell (x:y:_) = "this list is long."

lengthNew :: (Num b) => [a] -> b
lengthNew [] = 0
lengthNew(_:xs) = 1+ length' xs

bmiTell :: (RealFloat a ) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "you are underweight ,you emo "
  | bmi <= normal = "you are supposedly normal"
  | bmi  <= fat = "you are fat"
  | otherwise = "you are a whale"
  where bmi = weight / height ^ 2
        (skinny, normal, fat)=(18.5,25.0,30.0)

max' ::(Ord a)=>a->a->a
max' a b
    | a > b     = a
    | otherwise = b


initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName


clacBmis :: (RealFloat a) => [(a,a)] -> [a]
clacBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea


clacBmis1 :: (RealFloat a) => [(a,a)] -> [a]
clacBmis1 xs = [bmi | (w,h) <- xs ,let bmi = w *h ^2,bmi > 25]


head1 :: [a] -> a
head1 xs = case xs of [] -> error "no head"
                      (x: _) -> x



maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs




replicate' :: (Num i, Ord i) => i -> a -> [a]

replicate' n x
      | n <= 0 = []
      | otherwise = x:replicate' (n-1) x



take' :: (Num i,Ord i) => i -> [a] -> [a]
take' n _
        | n <= 0 = []
take' _ []       = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] ->  [(a,b)]
zip'_ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs ,a <=x]
      biggerSorted = quicksort [a | a <- xs , a > x]
  in smallerSorted ++ [x] ++ biggerSorted



divideByTen ::(Floating a)=>a->a
divideByTen = (/10)

compareWithHundred ::(Num a,Ord a)=>a-> Ordering
compareWithHundred   = compare 100

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c ) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f x y = f y x



-- let notNull x = not (null x) in filter notNull [[1,2,3],[2],[3,4,5]]

quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort  (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..]) where p x=x`mod`3829==0


-- let a = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n :chain (n `div` 2)
    | odd n = n: chain (n*3 + 1)


numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--   where
--      isLong xs = length xs > 15
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f = \x y -> f y x

sum1 :: (Num a) => [a] -> a
-- sum1 xs = foldl (\acc x -> acc + x) 0 xs
sum1 = foldl (+) 0

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 y ys = foldl (\acc x -> if x == y then True else acc) False ys

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = foldr (\x acc -> f x : acc) [] xs


-- scanl1 (\acc x->if x>acc then x else acc) [3,4,5,3,7,9,2,1]

sqrtNums :: Int
sqrtNums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))

-- sum (filter (> 10) (map (*2) [2..10]))
--
-- sum $ filter (>10) $ map (*2) [2..10]
--
-- map ($3) [(4+),(10*),sqrt]
--
-- map (\x -> negate (abs x)) [5,-3,-1]
--
-- map (negate.abs) [5,-3,-4]


oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
