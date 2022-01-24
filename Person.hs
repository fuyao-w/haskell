module Person
    (
      Person(..),
      Car(..),
      Vector(..)
    ) where

--  record syntax
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int ,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)


me = let fuyao = Person {firstName = "扶摇", lastName = "王", age = 24 ,height = 175,phoneNumber = "17726037103",flavor ="xx"} in fuyao



data Car a b c = Car {
  company :: a,
  model :: b,
  year :: c
} deriving (Show , Eq ,Read)


tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company =c ,model = m ,year = y}) = "This " ++ c ++ " " ++ m ++ " " ++ show y

data Vector a = Vector a a a deriving (Show)


vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)


vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k ) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n ) = i *l + j * m + k * n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq ,Ord,Show ,Read,Bounded,Enum)



type PhoneNumber =String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]
