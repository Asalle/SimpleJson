module Main (main) where

    import SimpleJson

    main = print (JObject[("name", JString "Jake the Dog"), ("age", JNumber 34)])
