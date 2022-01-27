
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

module PrettyJson(
    renderJValue
) where

import SimpleJson ( JValue(..) )
import Numeric (showHex)
import Data.Bits (shiftR , (.&.))
import Data.Char (ord)


data Doc = Empty
        | Char Char
        | Text String
        | Line
        | Concat Doc Doc
        | Union Doc Doc
 deriving (Show)






renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray  ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                            ? text ": "
                            ? renderJValue val


(?) :: Doc -> Doc -> Doc
Empty ? y = y
x ? Empty = x
x ? y = x `Concat` y



oneChar :: Char -> Doc
oneChar c = case lookup  c simpleEscapes of
                Just r -> text r
                Nothing |  mustEscape c -> hexEscape c
                        | otherwise     -> char c
            where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

hcat :: [Doc] -> Doc
hcat = fold (?)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left ? x ? char right


string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

simpleEscapes :: [(Char ,String)]
simpleEscapes = zipWith  ch  "\b\n\f\r\t\\\"/" "bnfrt\\\"/"  where ch  a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c
            | d < 0x10000 =  smallHex d
            | otherwise   = astral (d - 0x10000)
        where d = ord c


smallHex :: Int -> Doc
smallHex x = text "\\u"
            ? text (replicate  (4 - length h) '0')
            ? text h
        where h = showHex  x ""


astral :: Int -> Doc
astral n = smallHex (a + 0xd800) ? smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff



series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close  f = enclose open close .  fsep . punctuate  (char ',') .map f

fsep :: [Doc] -> Doc
fsep = fold (</>)



(</>) :: Doc -> Doc -> Doc
x </> y = x ? softline ? y



softline :: Doc
softline = group line

group :: Doc -> Doc 
group x = flatten x `Union` x


flatten :: Doc -> Doc
flatten  (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line             = Char ' '
flatten (x `Union` _)    = flatten x
flatten other            = other 



punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] =[]
punctuate p [d] = [d]
punctuate p (d :ds) = d ? p : punctuate p ds


empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)


line :: Doc
line = Line


compact :: Doc -> String 
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = 
              case d of
                  Empty           -> transform ds
                  Char c          -> c : transform ds
                  Text s          -> s ++ transform ds
                  Line            -> '\n' : transform ds
                  a `Concat` b    ->  transform (a:b:ds)
                  _ `Union` b     -> transform (b:ds)




pretty :: Int -> Doc -> String 
pretty  width x = best 0 [x]
    where 
        best col (d:ds) = 
            case d of
                Empty -> best col ds
                Char c -> c : best (col + 1) ds
                Text s -> s ++ best (col + length  s) ds
                Line   -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union`  b -> nicest col (best col (a:ds)) (best col (b:ds))                                            
        best _ _ = ""
        nicest col a b 
                        | (width - least) `fits` a = a 
                        | otherwise                = b
                        where least = min width col      

fits :: Int -> String -> Bool 
w `fits` _ | w < 0 = False 
w `fits` ""        = True 
w `fits` ('\n':_)  = True 
w `fits` (c:cs)    = (w - 1) `fits` cs

              


data BasketBallPlayer =  YaoMing
                    | Kobe                    
                    | James deriving (Show)


class Player a  where 
    play :: a -> String


instance Player BasketBallPlayer where
    play a = show a ++ "yao ming play very good" 

-- instance Player  Kobe where 
--     play = "科比会单打"



type JsonError = String
class JSON a where 
    toJvalue :: a -> JValue
    fromJValue :: JValue -> Either JsonError a

instance JSON String where
        toJvalue = JString
        fromJValue (JString s)  = Right s
        fromJValue _            = Left "not a JSON string"

        