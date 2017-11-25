module PutJson
    (
        renderValue
    )
    where

    import SimpleJson
    import Data.List (intercalate)

    renderValue :: JValue -> String

    renderValue (JString s) = show s

    renderValue (JNumber n) = show n

    renderValue (JBool b) = show b

    renderValue (JArray a) = "[ " ++ values a ++ " ]"
        where values [] = ""
              values vs = intercalate ", " $ map renderValue vs

    renderValue (JObject o) = "{ " ++ classStr o ++ " }"
        where classStr [] = ""
              classStr cls = intercalate ", " $ map renderPair cls
              renderPair (f, s) = "( " ++ show f ++ ": " ++ renderValue s ++ " )"