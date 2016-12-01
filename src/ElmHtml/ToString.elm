module ElmHtml.ToString exposing (nodeRecordToString, nodeTypeToString, prettyPrintNodeRecord)

{-| Convert ElmHtml to string

@docs nodeRecordToString, nodeTypeToString, prettyPrintNodeRecord

-}

import String
import Dict exposing (Dict)
import ElmHtml.InternalTypes exposing (..)


{-| Convert a node record to a string. This basically takes the tag name, then
    pulls all the facts into tag declaration, then goes through the children and
    nests them under this one
-}
nodeRecordToString : NodeRecord -> String
nodeRecordToString =
    nodeRecordToStringHelp False


{-| Pretty print a node record.
-}
prettyPrintNodeRecord : NodeRecord -> String
prettyPrintNodeRecord =
    nodeRecordToStringHelp True


nodeRecordToStringHelp : Bool -> NodeRecord -> String
nodeRecordToStringHelp prettyPrint { tag, children, facts } =
    let
        openTag : List (Maybe String) -> String
        openTag extras =
            let
                trimmedExtras =
                    List.filterMap (\x -> x) extras
                        |> List.map String.trim
                        |> List.filter ((/=) "")

                filling =
                    case trimmedExtras of
                        [] ->
                            ""

                        more ->
                            " " ++ (String.join " " more)

                closingBracket =
                    if List.isEmpty children then
                        "/>"
                    else
                        ">"
            in
                "<" ++ tag ++ filling ++ closingBracket

        closeTag =
            "</" ++ tag ++ ">"

        styles =
            case Dict.toList facts.styles of
                [] ->
                    Nothing

                styles ->
                    styles
                        |> List.map (\( key, value ) -> key ++ ":" ++ value)
                        |> String.join ""
                        |> (\styleString -> "style=\"" ++ styleString ++ "\"")
                        |> Just

        classes =
            Dict.get "className" facts.stringAttributes
                |> Maybe.map (\name -> "class=\"" ++ name ++ "\"")

        stringAttributes =
            Dict.filter (\k v -> k /= "className") facts.stringAttributes
                |> Dict.toList
                |> List.map (\( k, v ) -> k ++ "=\"" ++ v ++ "\"")
                |> String.join " "
                |> Just

        boolAttributes =
            Dict.toList facts.boolAttributes
                |> List.map (\( k, v ) -> k ++ "=" ++ (String.toLower <| toString v))
                |> String.join " "
                |> Just

        joinChar =
            if prettyPrint then
                "\n"
            else
                ""

        childToString child =
            if prettyPrint then
                "  " ++ nodeTypeToString child
            else
                nodeTypeToString child

        opener =
            openTag [ classes, styles, stringAttributes, boolAttributes ]
    in
        if List.isEmpty children then
            -- this will be printed like <foo/> because it has no children,
            -- so no closing tag necessary
            opener
        else
            let
                childrenStrings =
                    children
                        |> List.map childToString
                        |> String.join joinChar
            in
                [ opener, childrenStrings, closeTag ]
                    |> String.join joinChar


{-| Convert a given html node to a string based on the type
-}
nodeTypeToString : ElmHtml -> String
nodeTypeToString nodeType =
    case nodeType of
        TextTag { text } ->
            text

        NodeEntry record ->
            nodeRecordToString record

        CustomNode record ->
            ""

        MarkdownNode record ->
            record.model.markdown

        NoOp ->
            ""
