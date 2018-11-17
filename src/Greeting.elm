module Greeting exposing (greet, Entry(..))

type Entry 
    = None
    | Only String
    | Multiple (List String)



greet: Entry -> String
greet entry =
    case entry of
        Only name -> 
            if isUpper name then
                "HELLO, " ++ name ++ "."
            else
                "Hello, " ++ name ++ "."

        None -> "Hello, my friend."
        Multiple entries ->
                let
                    splittedNames = entries
                                    |> List.map splitUnquotedString
                                    |> List.concat
                                    |> List.map (String.trim) 
                    (uppercaseNames, lowercaseNames) = List.partition isUpper splittedNames

                in
                    (lowerCaseGreeting lowercaseNames) ++ (upperCaseGreeting uppercaseNames)

splitUnquotedString: String -> List String
splitUnquotedString string =
    let
        isQuoted = (String.startsWith "\"" string) && (String.endsWith "\"" string)
    in
    if isQuoted then
        [ string 
            |> String.dropLeft 1
            |> String.dropRight 1
        ]

    else
        String.split "," string


upperCaseGreeting: List String -> String
upperCaseGreeting entries =
    entries
        |> List.map (\name -> " AND HELLO " ++ name ++ "!")
        |> String.concat

lowerCaseGreeting: List String -> String
lowerCaseGreeting entries =
    let
        firstPartLength = (List.length entries) - 1

        firstPart = entries
                    |> List.take firstPartLength
                    |> String.join ", "
                    
        lastPart = entries 
                    |> List.drop firstPartLength
                    |> List.head
                    |> Maybe.withDefault ""
    in
        "Hello, " ++ firstPart ++ " and " ++ lastPart ++ "."



isUpper: String -> Bool
isUpper string =
    (String.toUpper string) == string