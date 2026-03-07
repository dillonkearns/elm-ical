module FormatTests exposing (suite)

import Expect
import Format
import Fuzz
import Property
import Test exposing (..)


suite : Test
suite =
    describe "Format"
        [ test "escapes and folds long description" <|
            \() ->
                ( "DESCRIPTION"
                , Property.Text "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.\nbeep boop"
                , []
                )
                    |> Property.encodeProperty
                    |> Expect.equal
                        (String.join "\u{000D}\n"
                            [ "DESCRIPTION:Lorem ipsum dolor sit amet\\, consetetur sadipscing elitr\\, sed "
                            , " diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"
                            , " \\, sed diam voluptua.\\nbeep boop"
                            ]
                        )
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
        , test "ASCII string at 75-byte limit does not fold" <|
            \() ->
                let
                    input : String
                    input =
                        String.repeat 75 "x"
                in
                Format.splitOverflowingLines input
                    |> String.contains "\u{000D}\n "
                    |> Expect.equal False
        , test "ASCII string one byte over 75-byte limit folds" <|
            \() ->
                let
                    input : String
                    input =
                        String.repeat 76 "x"
                in
                Format.splitOverflowingLines input
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
                        Format.splitOverflowingLines ("KEY:" ++ value)

                    lines : List String
                    lines =
                        String.split "\u{000D}\n" result
                in
                lines
                    |> List.all (\line -> utf8ByteLength line <= 75)
                    |> Expect.equal True
        , test "line folding respects byte length for multi-byte characters" <|
            \() ->
                let
                    -- "X:" (2 bytes) + 70 ASCII (70 bytes) + "🎉" (4 bytes) = 76 bytes total
                    -- Should fold before the emoji to keep line <= 75 bytes.
                    input : String
                    input =
                        "X:" ++ String.repeat 70 "a" ++ "🎉"

                    result : String
                    result =
                        Format.splitOverflowingLines input

                    lines : List String
                    lines =
                        String.split "\u{000D}\n" result
                in
                lines
                    |> List.all (\line -> utf8ByteLength line <= 75)
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


utf8ByteLength : String -> Int
utf8ByteLength str =
    str
        |> String.toList
        |> List.map charByteLength
        |> List.sum


charByteLength : Char -> Int
charByteLength c =
    let
        code : Int
        code =
            Char.toCode c
    in
    if code < 0x80 then
        1

    else if code < 0x0800 then
        2

    else if code < 0x00010000 then
        3

    else
        4
