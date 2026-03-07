module FormatTests exposing (suite)

import Expect
import Format
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "Format"
        [ test "escapes and folds long description" <|
            \() ->
                ( "DESCRIPTION", "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop" )
                    |> Format.normalizeField
                    |> Expect.equal "DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed\u{000D}\n  diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam era\u{000D}\n t\\, sed diam voluptua.\\nbeep boop"
        , test "double quotes are not escaped" <|
            \() ->
                Format.formatValue "She said \"hello\""
                    |> Expect.equal "She said \"hello\""
        , test "backslashes are escaped" <|
            \() ->
                Format.formatValue "path\\to\\file"
                    |> Expect.equal "path\\\\to\\\\file"
        , test "semicolons are escaped" <|
            \() ->
                Format.formatValue "a;b;c"
                    |> Expect.equal "a\\;b\\;c"
        , test "commas are escaped" <|
            \() ->
                Format.formatValue "a,b,c"
                    |> Expect.equal "a\\,b\\,c"
        , test "newlines become literal \\n" <|
            \() ->
                Format.formatValue "line1\nline2"
                    |> Expect.equal "line1\\nline2"
        , test "carriage return + newline becomes literal \\n" <|
            \() ->
                Format.formatValue "line1\u{000D}\nline2"
                    |> Expect.equal "line1\\nline2"
        , test "string at 74-char limit does not fold" <|
            \() ->
                let
                    value : String
                    value =
                        String.repeat 70 "x"
                in
                ( "KEY", value )
                    |> Format.normalizeField
                    |> String.contains "\u{000D}\n "
                    |> Expect.equal False
        , test "string one char over 74-char limit folds" <|
            \() ->
                let
                    value : String
                    value =
                        String.repeat 71 "x"
                in
                ( "KEY", value )
                    |> Format.normalizeField
                    |> String.contains "\u{000D}\n "
                    |> Expect.equal True
        , test "very long single word folds correctly" <|
            \() ->
                let
                    value : String
                    value =
                        String.repeat 200 "a"

                    result : String
                    result =
                        ( "KEY", value ) |> Format.normalizeField

                    lines : List String
                    lines =
                        String.split "\u{000D}\n" result
                in
                lines
                    |> List.all (\line -> String.length line <= 75)
                    |> Expect.equal True
        , fuzz Fuzz.string "no unescaped backslashes in output" <|
            \input ->
                let
                    output : String
                    output =
                        Format.formatValue input

                    withoutEscapes : String
                    withoutEscapes =
                        output
                            |> String.replace "\\n" ""
                            |> String.replace "\\\\" ""
                            |> String.replace "\\," ""
                            |> String.replace "\\;" ""
                in
                withoutEscapes
                    |> String.contains "\\"
                    |> Expect.equal False
        , fuzz Fuzz.string "no unescaped commas or semicolons in output" <|
            \input ->
                let
                    output : String
                    output =
                        Format.formatValue input
                in
                Expect.all
                    [ \o ->
                        o
                            |> String.replace "\\," ""
                            |> String.contains ","
                            |> Expect.equal False
                    , \o ->
                        o
                            |> String.replace "\\;" ""
                            |> String.contains ";"
                            |> Expect.equal False
                    ]
                    output
        , fuzz Fuzz.string "no literal newlines in output" <|
            \input ->
                let
                    output : String
                    output =
                        Format.formatValue input
                in
                output
                    |> String.contains "\n"
                    |> Expect.equal False
        ]
