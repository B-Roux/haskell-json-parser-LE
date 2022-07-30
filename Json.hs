import Parser

data JsonValue = JString String
               | JNumber Double
               | JObject [(String,JsonValue)]
               | JArray [JsonValue]
               | JBool Bool
               | JNull

-- Match a JSON object
pJObject :: Parser JsonValue
pJObject stream = undefined

-- Match a JSON array
pJArray :: Parser JsonValue
pJArray stream = undefined

-- Match a string
pJString :: Parser JsonValue
pJString stream = undefined

-- Match a number
pJNumber :: Parser JsonValue
pJNumber stream = sequenceOf
    [ sequenceOf [ optional (pChar '-') |=>
                     \i -> case i of Nothing -> ""
                                     Just _ -> "-"
                 , manyOf (anyCharOf ['0'..'9'])
                 ] |=> concat
    , optional (pChar '.' >>. manyOf (anyCharOf ['0'..'9'])) |=>
        \i -> case i of Nothing -> ""
                        Just s -> '.':s
    , optional ((pChar 'e' <|> pChar 'E')
        >>. sequenceOf [ optional (pChar '-' <|> pChar '+') |=>
                           \i -> case i of Nothing -> "+"
                                           Just c -> [c]
                       , manyOf (anyCharOf ['0'..'9'])
                       ]) |=>
        \i -> case i of Nothing -> ""
                        Just ss -> 'E':(concat ss)
    ] |=> concat |=> read |=> JNumber
    $ stream

-- Match a boolean
pJBool :: Parser JsonValue
pJBool stream = (pString "true" |-> JBool True)
    <|> (pString "false" |-> JBool False)
    $ stream

-- Match a null
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

