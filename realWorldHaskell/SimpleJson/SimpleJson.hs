module SimpleJson
    (
        JValue(..)
      , getString
      , getNumber
      , getBool
      , getArray
      , getObject
      , isNull
        ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getNumber (JNumber s) = Just s
getNumber _           = Nothing

getBool (JBool s) = Just s
getBool _         = Nothing

getArray (JArray s) = Just s
getArray _          = Nothing

getObject (JObject s) = Just s
getObject _           = Nothing

isNull v = v == JNull
