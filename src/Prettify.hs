module Prettify
    (
        Doc
      , string
      , text
      , double
      , char
      , (<>)
      , enclose
      , fsep
      , punctuate
    )
    where

        import SimpleJson
        import Numeric (showHex)
        import Data.Char (ord)
        import Data.Bits (shiftR, (.&.))

        data Doc = Empty
                | Char Char
                | Text String
                | Line
                | Concat Doc Doc
                | Union Doc Doc
            deriving (Show, Eq)

        string :: String -> Doc
        string = enclose '"' '"' . hcat . map oneChar

        text :: String -> Doc
        text "" = Empty
        text str = Text str

        double :: Double -> Doc
        double num = text $ show num

        line :: Doc
        line = Line

        enclose :: Char -> Char -> Doc -> Doc
        enclose left right d = char left <> d <> char right

        (<>) :: Doc -> Doc -> Doc
        a <> Empty = a
        Empty <> b = b
        a <> b = a `Concat` b

        concat :: [[a]] -> [a]
        concat = foldr (++) []

        fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
        fold f = foldr f empty

        hcat :: [Doc] -> Doc
        hcat = fold (<>)

        char :: Char -> Doc
        char = Char

        oneChar :: Char -> Doc
        oneChar c = case lookup c simpleEscapes of
                    Just r -> text r
                    Nothing | mustEscape c -> hexEscape c
                            | otherwise -> char c
            where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

        simpleEscapes :: [(Char, String)]
        simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
            where ch a b = (a, ['\\', b])

        smallHex :: Int -> Doc
        smallHex x =    text "\\u"
                    <>  text (replicate (4 - length h) '0')
                    <>  text h
            where h = showHex x ""

        astral :: Int -> Doc
        astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
            where a = (n `shiftR` 10) .&. 0x3ff
                  b = n .&. 0x3ff

        hexEscape :: Char -> Doc
        hexEscape c | d < 0x10000 = smallHex d
                    | otherwise = astral $ d - 0x10000
            where d = ord c

        empty :: Doc
        empty = Empty

        fsep :: [Doc] -> Doc
        fsep = fold (</>)

        (</>) :: Doc -> Doc -> Doc
        x </> y = x <> softline <> y

        softline :: Doc
        softline = group line

        group :: Doc -> Doc
        group x = flatten x `Union` x

        flatten :: Doc -> Doc
        flatten (x `Concat` y) = flatten x `Concat` flatten y
        flatten Line           = Char ' '
        flatten (x `Union` _)  = flatten x
        flatten other          = other

        punctuate :: Doc -> [Doc] -> [Doc]
        punctuate _ [] = []
        punctuate _ [a] = [a]
        punctuate p (x:xs) = (x <> p) : punctuate p xs