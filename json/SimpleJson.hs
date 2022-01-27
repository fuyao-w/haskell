module SimpleJson
    (
        JValue(..)
        , getString
        , getInt
        , getObject
        , getBool
        , getArray
        , getDouble
        , isNull

    ) where

import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject [(String,JValue)]
            | JArray  [JValue]
                deriving (Eq,Ord,Show)


getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing





getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt  _           = Nothing


getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble  _           = Nothing




getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just  o
getObject  _           = Nothing



getArray :: JValue -> Maybe [JValue]
getArray  (JArray a) = Just  a
getArray  _           = Nothing


getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

isNull :: JValue -> Bool
isNull v  =   v == JNull





renderJValue :: JValue -> String
renderJValue (JString s) = show  s
renderJValue (JNumber s) = show  s
renderJValue (JBool True ) = "true"
renderJValue (JBool False  ) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where   pairs [] = ""
            pairs ps = intercalate  " ," (map renderPair ps)
            renderPair (k,v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where   values [] = ""
            values vs = intercalate  ", " (map renderJValue vs)





putJvalue :: JValue -> IO ()
putJvalue  v = putStrLn  (renderJValue v)



