module SimpleJson (
    JValue(..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject.
    getArray,
    isNull
) 
where
data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe String
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe String
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe String
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe String
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe String
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v            = v == JNull