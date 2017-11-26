module PrettyJson
    (
        renderJValue
    )
    where
        import SimpleJson
        import PrettyStub

        renderJValue :: JValue -> Doc
        renderJValue (JBool True)  = text "true"
        renderJValue (JBool False) = text "false"
        renderJValue (JNull) = text "null"
        renderJValue (JString s) = string s
        renderJValue (JNumber n) = double n
