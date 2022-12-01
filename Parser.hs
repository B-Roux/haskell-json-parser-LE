module Parser where

-- Define the monadic recursive descent parser

type Parser a = [Char] -> Maybe (a, [Char])


-- -- Parser combinators -- --

-- Try alternative parser
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p p' stream = case p stream of
    Just a -> Just a
    Nothing -> (p' stream)

-- Combine two parsers and keep both results
(.>>.) :: Parser a -> Parser b -> Parser (a,b)
(.>>.) p p' stream = case p stream of
    Nothing -> Nothing
    Just (c, rem) -> case p' rem of
        Nothing -> Nothing
        Just (c', rem') -> Just ((c, c'), rem')

-- Combine two parsers and keep left
(.>>) :: Parser a -> Parser b -> Parser a
(.>>) p p' stream = case p stream of
    Nothing -> Nothing
    Just (c, rem) -> case p' rem of
        Nothing -> Nothing
        Just (_, rem') -> Just (c, rem')

-- Combine two parsers and keep right
(>>.) :: Parser a -> Parser b -> Parser b
(>>.) p p' stream = case p stream of
    Nothing -> Nothing
    Just (c, rem) -> case p' rem of
        Nothing -> Nothing
        Just (c', rem') -> Just (c', rem')

-- Substitute the parsed result
(|->) :: Parser a -> b -> Parser b
(|->) p x stream = case p stream of
    Nothing -> Nothing
    Just (_, rem) -> Just (x, rem)

-- Pass the parsed result
(|=>) :: Parser a -> (a -> b) -> Parser b
(|=>) p f stream = case p stream of
    Nothing -> Nothing
    Just (c, rem) -> Just ((f c), rem)


-- -- Atomic parsers -- --

-- Parse a single character
pChar :: Char -> Parser Char
pChar _ [] = Nothing
pChar x (y:ys) | y==x = Just (y, ys)
               | otherwise = Nothing

-- Parse a sequence of characters
pString :: [Char] -> Parser [Char]
pString x stream =
    case f x stream of
        Nothing -> Nothing
        Just (_, res) -> Just (x, res)
    where
        f [] ys = Just ([], ys)
        f (x:xs) ys = case pChar x ys of
            Nothing -> Nothing
            Just (_, res) -> f xs res


-- -- Higher order parsers -- --

-- Match any character
anyChar :: Parser Char
anyChar [] = Nothing
anyChar (x:xs) = Just (x, xs)

-- Match any character except x
anyCharBut :: Char -> Parser Char
anyCharBut _ [] = Nothing
anyCharBut x (y:ys) | y==x = Nothing
                    | otherwise = Just (y, ys)

-- Match any character from a list
anyCharOf :: [Char] -> Parser Char
anyCharOf xs stream = choice (map pChar xs) $ stream

-- Match between two parsers
between :: Parser a -> Parser b -> Parser c -> Parser c
between x y z stream = x >>. z .>> y $ stream

-- Match any of the parsers
choice :: [Parser a] -> Parser a
choice [] stream = Nothing
choice (p:ps) stream = foldr (<|>) p ps $ stream

-- Match an end-of-file (or stream)
eof :: Parser ()
eof stream = case stream of
    [] -> Just ((), [])
    othwerwise -> Nothing

-- Match a parser 0 or more times
manyOf :: Parser a -> Parser [a]
manyOf p stream = f stream [] where
    f xs acc = case p xs of
        Nothing -> Just (reverse acc, xs)
        Just (c, rem) -> f rem (c:acc)

-- Match a parser 1 or more times
oneOrMore :: Parser a -> Parser[a]
oneOrMore p stream = case p stream of
    Nothing -> Nothing
    Just (c, rem) -> case manyOf p rem of
        Nothing -> Just (c:[], rem)
        Just (cs, rem') -> Just (c:cs, rem')

-- Optionally match, otherwise don't consume input
optional :: Parser a -> Parser (Maybe a)
optional p stream = case p stream of
        Nothing -> Just (Nothing, stream)
        Just (c, rem) -> Just (Just c, rem)

-- Match parsers sequentially
sequenceOf :: [Parser a] -> Parser [a]
sequenceOf ps' stream = f ps' stream [] where
    f [] ys acc = Just (reverse acc, ys)
    f (p:ps) ys acc = case p ys of
        Nothing -> Nothing
        Just (c, rem) -> f ps rem (c:acc)

-- Match an array of parsers with separators
arrayOf :: Parser s -> Parser a -> Parser [a]
arrayOf psep pitem stream = case pitem stream of
    Nothing -> Just ([], stream)
    Just (c, rem) ->
        case (manyOf (psep >>. pitem) rem) of
            Nothing -> Just ([c], rem)
            Just (cs, rem') -> Just (c:cs, rem')
