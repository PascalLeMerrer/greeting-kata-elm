module GreetingTest exposing (suite)

import Greeting exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Greeting module"
        [ describe "greet should return greeting" 
            [ test "when name is provided" <|
                \_ ->
                    Expect.equal "Hello, Bob." <| greet (Only "Bob")
            , test "when no name is provided" <|
                \_ ->
                    Expect.equal "Hello, my friend." <| greet (None)                    
            , test "when 2 names are provided" <|
                \_ ->
                    Expect.equal "Hello, Jill and Jane." <| greet (Multiple ["Jill", "Jane"])
            , test "when more than 2 names are provided" <|
                \_ ->
                    Expect.equal "Hello, Amy, Brian and Charlotte." 
                                <| greet (Multiple ["Amy", "Brian", "Charlotte"])
            ]
        , describe "greet should should shout greeting" 
            [ test "when an uppercase name is provided" <|
                \_ ->
                    Expect.equal "HELLO, JERRY." <| greet (Only "JERRY")
            , test "when a mix of uppercase and lowercase names is provided" <|
                \_ ->
                    Expect.equal "Hello, Amy and Charlotte. AND HELLO BRIAN!"
                               <| greet (Multiple ["Amy", "BRIAN", "Charlotte"])
          
            ]
        , describe "greet should should split names" 
            [ test "when an entry contains a comma" <|
                \_ ->
                    Expect.equal "Hello, Bob, Charlie and Dianne."
                                 <| greet (Multiple ["Bob", "Charlie, Dianne"])
            , test "except when an entry is quoted" <|
                \_ ->
                    Expect.equal "Hello, Bob and Charlie, Dianne."
                                 <| greet (Multiple ["Bob", "\"Charlie, Dianne\""])

            ]
        ]