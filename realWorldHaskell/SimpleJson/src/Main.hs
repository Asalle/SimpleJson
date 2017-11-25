module Main (main) where

    import SimpleJson
    import PrettyJson

    main = do
        putStrLn ( renderJValue (JObject [("name", JString "Jake the Dog"), ("age", JNumber 34)]))

