module Main (main) where

    import SimpleJson
    import PutJson

    main = do
        putStrLn ( renderValue (JObject [("name", JString "Jake the Dog"), ("age", JNumber 34)]))

