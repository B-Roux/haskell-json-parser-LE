module JSONParser (pJSON) where

-- This parser is (loosely) based on the official JSON spec
-- (available at https://www.json.org/json-en.html)
-- and may differ from some more common conventions in JSON
-- parsing.
-- For convenience, the parsers are also laid out in a similar
-- order to how they appear there :-)
--
-- Some notes:
--     1. This parser cannot parse unicode characters. Escaped
--          unicode characters will result in Nothing.
--     2. This parser does not differentiate between integers
--          or floating-point values. Everhting is parsed
--          as a double.
--          Note: The JSON spec also does not differentiate
--          between these, even most parsers do.

import Parser

data JsonValue = JString String
               | JNumber Double
               | JObject [(String,JsonValue)]
               | JArray [JsonValue]
               | JBool Bool
               | JNull
               deriving (Show)

-- Convenience function
pJSON :: Parser JsonValue
pJSON stream = pJValue stream

-- Match a JSON object
pJObject :: Parser JsonValue
pJObject stream = between
    (pChar '{' .>> pJWhitespace)
    (pJWhitespace >>. pChar '}')
    (arrayOf (pChar ',') (
        (pJWhitespace >>. pJString' .>> pJWhitespace)
        .>>. (pChar ':' >>. pJValue)
    )) |=> JObject
    $ stream

-- Match a JSON array
pJArray :: Parser JsonValue
pJArray stream = between
    (pChar '[')
    (pChar ']')
    (arrayOf (pChar ',') pJValue) |=> JArray
    $ stream

-- Match any JSON value
pJValue :: Parser JsonValue
pJValue stream = between
    pJWhitespace
    pJWhitespace
    (choice [ pJString
            , pJNumber
            , pJObject
            , pJArray
            , pJBool
            , pJNull
            ])
    $ stream

-- Match a JSON string
pJString :: Parser JsonValue
pJString stream = pJString' |=> JString
    $ stream

-- Parse as a string
pJString' :: Parser String
pJString' stream = between
    (pChar '"')
    (pChar '"')
    (manyOf (pChar '\\' >>. choice
        [ pChar '\"' |-> '\"'
        , pChar '\\' |-> '\\'
        , pChar 'b' |-> '\b'
        , pChar 'f' |-> '\f'
        , pChar 'n' |-> '\n'
        , pChar 'r' |-> '\r'
        , pChar 't' |-> '\t'
        ] <|> anyCharBut '\"'))
    $ stream

-- Match a JSON number
pJNumber :: Parser JsonValue
pJNumber stream = sequenceOf
    [ sequenceOf [ optional (pChar '-') |=>
                     \i -> case i of Nothing -> ""
                                     Just _ -> "-"
                 , oneOrMore (anyCharOf ['0'..'9'])
                 ] |=> concat
    , optional (pChar '.' >>. oneOrMore (anyCharOf ['0'..'9'])) |=>
        \i -> case i of Nothing -> ""
                        Just s -> '.':s
    , optional ((pChar 'e' <|> pChar 'E')
        >>. sequenceOf [ optional (pChar '-' <|> pChar '+') |=>
                           \i -> case i of Nothing -> "+"
                                           Just c -> [c]
                       , oneOrMore (anyCharOf ['0'..'9'])
                       ]) |=>
        \i -> case i of Nothing -> ""
                        Just ss -> 'E':(concat ss)
    ] |=> concat |=> (read :: [Char] -> Double) |=> JNumber
    $ stream

-- Match a JSON boolean
pJBool :: Parser JsonValue
pJBool stream = (pString "true" |-> JBool True)
    <|> (pString "false" |-> JBool False)
    $ stream

-- Match a JSON null
pJNull :: Parser JsonValue
pJNull stream = pString "null" |-> JNull
    $ stream

-- Match any allowed JSON whitespace
pJWhitespace :: Parser [Char]
pJWhitespace stream = manyOf (anyCharOf
    [ ' '
    , '\n'
    , '\r'
    , '\t'
    ])
    $ stream
