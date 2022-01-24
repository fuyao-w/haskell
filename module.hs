import qualified Data.List as List
-- import Data.List (nub,sort)
-- import Data.List hiding (nub)
import qualified Data.Map as M

import Geometry as G

numUniques :: (Eq a) => [a] -> Int
numUniques = length.(List.nub)


r =  List.intersperse '.' "ABCDEFG"

tr = List.transpose [[1,2,3],[4,5,6],[7,8,9]]

c = List.concat ["a","b"]
cm =  List.concatMap (replicate 4) [1..3]

andFunc =  List.and $ map (>4) [5,6]
iter = take 10 $ List.iterate (*2) 1

spli = List.splitAt 3 "heyman"

tw = sum $ List.takeWhile (<10000) $ map (^3) [1..]


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber lmap =
    case M.lookup lockerNumber lmap of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"



lockers :: LockerMap
lockers = M.fromList
    [
      (100,(Taken,"ZD39I"))
      ,(101,(Free,"JAH3I"))
      ,(103,(Free,"IQSA9"))
      ,(105,(Free,"QOTSA"))
      ,(109,(Taken,"893JJ"))
      ,(110,(Taken,"99292"))
    ]


-- data List a = Empty | Cons {listHead :: a ,listTail :: List a} deriving (Show,Read,Eq,Ord)


infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs ) .++ ys = x :-: (xs .++ ys)



data Tree a =  EmptyTree |  Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree  EmptyTree

treeInsert  :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums


data TrafficLight  = Red | Yellow | Green
instance  Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance  Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0  = False
    yesno _ = True


instance YesNo Bool where
    yesno = id

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
        yesno (Just _) = True
        yesno Nothing = False


instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) =
        Node (f x) (fmap f leftsub) (fmap f rightsub)
