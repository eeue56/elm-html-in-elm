module ElmHtml.ToString exposing (..)

{-| Convert ElmHtml to string

@docs nodeRecordToString, nodeTypeToString

-}

import String
import Dict exposing (Dict)
import ElmHtml.InternalTypes exposing (..)


{-| Convert a node record to a string. This basically takes the tag name, then
    pulls all the facts into tag declaration, then goes through the children and
    nests them under this one
-}
nodeRecordToString : NodeRecord -> String
nodeRecordToString { tag, children, facts } =
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
            in
                "<" ++ tag ++ filling ++ ">"

        closeTag =
            "</" ++ tag ++ ">"

        childrenStrings =
            List.map nodeTypeToString children
                |> String.join ""

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
    in
        String.join ""
            [ openTag [ classes, styles, stringAttributes, boolAttributes ]
            , childrenStrings
            , closeTag
            ]


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
