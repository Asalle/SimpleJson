module PrettyJson
    (
        renderJValue
    )
    where
        import SimpleJson
        import Prettify

        renderJValue :: JValue -> Doc
        renderJValue (JBool True)  = text "true"
        renderJValue (JBool False) = text "false"
        renderJValue (JNull) = text "null"
        renderJValue (JString s) = string s
        renderJValue (JNumber n) = double n
        renderJValue (JArray a) = series '[' ']' renderJValue a
        renderJValue (JObject o) = series '{' '}' field o
            where field (name, val) = string name <> text ": " <> renderJValue val

        series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
        series left right printFunc = enclose left right . fsep . punctuate (char ',') . map printFunc



