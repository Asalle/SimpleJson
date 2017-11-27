{- |
Module      :  $Header$
Description :  Pretty printing JSON data
License     :  GPL v. 3

Maintainer  :  asalle.kim@gmail.com
Stability   :  unstable
Portability :  portable
-}

module Prettify
    (
        Doc
      , pretty
      , compact
      , string
      , text
      , double
      , char
      , (<>)
      , enclose
      , fsep
      , punctuate
      , empty
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

        pretty width x = best 0 [x]
            where best col (d:ds) =
                      case d of
                        Empty        -> best col ds
                        Char c       -> c :  best (col + 1) ds
                        Text s       -> s ++ best (col + length s) ds
                        Line         -> '\n' : best 0 ds
                        a `Concat` b -> best col (a:b:ds)
                        a `Union` b  -> nicest col (best col (a:ds))
                                                   (best col (b:ds))
                  best _ _ = ""

                  nicest col a b | (width - least) `fits` a = a
                                 | otherwise                = b
                                 where least = min width col

        compact :: Doc -> String
        compact x = transform [x]
            where transform [] = ""
                  transform (x:xs) =
                    case x of
                        Empty -> transform xs
                        Char c -> c : transform xs
                        Text s -> s ++ transform xs
                        Line   -> '\n' : transform xs
                        a `Concat` b -> transform (a:b:xs)
                        _ `Union` b -> transform (b:xs)

        fits :: Int -> String -> Bool
        w `fits` _ | w < 0 = False
        w `fits` ""        = True
        w `fits` ('\n':_)  = True
        w `fits` (c:cs)    = (w - 1) `fits` cs

        -- | 'string' pretty prints a string adding quotes
        string :: String -> Doc
        string = enclose '"' '"' . hcat . map oneChar

        -- | 'text' pretty prints a string with no quotes
        text :: String -> Doc
        text "" = Empty
        text str = Text str

        -- | 'double' converts a number to Doc
        double :: Double -> Doc
        double num = text $ show num

        line :: Doc
        line = Line

        -- | 'enclose' left right doc encloses the Doc with left and right
        enclose :: Char -> Char -> Doc -> Doc
        enclose left right d = char left <> d <> char right

        -- | Doc concatenation
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

        -- | Make a Doc out of Char
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

        -- | fold with softline
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

        -- | intercalate a list with punctuation or whatever
        punctuate :: Doc -> [Doc] -> [Doc]
        punctuate _ [] = []
        punctuate _ [a] = [a]
        punctuate p (x:xs) = (x <> p) : punctuate p xs