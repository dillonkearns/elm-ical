module Format exposing (formatValue, splitOverflowingLines)

import Regex


formatValue : String -> String
formatValue value =
    value
        |> Regex.replace (reg "[\\\\;,]") (\{ match } -> "\\" ++ match)
        |> Regex.replace (reg "(?:\u{000D}\n|\u{000D}|\n)") (\_ -> "\\n")


{-| Fold lines per RFC 5545 Section 3.1: no line may exceed 75 octets
excluding the line break. The first line gets 75 bytes; continuation lines
get a leading space (1 byte) plus up to 74 bytes of content = 75 total.
-}
splitOverflowingLines : String -> String
splitOverflowingLines string =
    let
        chars : List Char
        chars =
            String.toList string

        ( firstLine, rest ) =
            takeBytesUpTo 75 chars
    in
    (firstLine :: foldByBytes 74 [] rest)
        |> List.map String.fromList
        |> String.join "\u{000D}\n "


foldByBytes : Int -> List (List Char) -> List Char -> List (List Char)
foldByBytes maxBytes acc chars =
    case chars of
        [] ->
            List.reverse acc

        _ ->
            let
                ( line, rest ) =
                    takeBytesUpTo maxBytes chars
            in
            foldByBytes maxBytes (line :: acc) rest


takeBytesUpTo : Int -> List Char -> ( List Char, List Char )
takeBytesUpTo maxBytes chars =
    takeBytesUpToHelp maxBytes 0 [] chars


takeBytesUpToHelp : Int -> Int -> List Char -> List Char -> ( List Char, List Char )
takeBytesUpToHelp maxBytes currentBytes taken remaining =
    case remaining of
        [] ->
            ( List.reverse taken, [] )

        c :: rest ->
            let
                cBytes : Int
                cBytes =
                    charByteLength c

                newTotal : Int
                newTotal =
                    currentBytes + cBytes
            in
            if newTotal > maxBytes then
                ( List.reverse taken, remaining )

            else
                takeBytesUpToHelp maxBytes newTotal (c :: taken) rest


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


reg : String -> Regex.Regex
reg string =
    Regex.fromString string
        |> Maybe.withDefault Regex.never
