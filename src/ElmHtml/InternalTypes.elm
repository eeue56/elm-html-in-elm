module ElmHtml.InternalTypes
    exposing
        ( ElmHtml(..)
        , TextTagRecord
        , NodeRecord
        , CustomNodeRecord
        , MarkdownNodeRecord
        , Facts
        , Tagger
        , decodeElmHtml
        , emptyFacts
        , ElementKind(..)
        , toElementKind
        )

{-| Internal types used to represent Elm Html in pure Elm

@docs ElmHtml, TextTagRecord, NodeRecord, CustomNodeRecord, MarkdownNodeRecord, Facts, Tagger, ElementKind

@docs decodeElmHtml, emptyFacts, toElementKind
-}

import Dict exposing (Dict)
import Json.Encode
import Json.Decode exposing (field)
import ElmHtml.Markdown exposing (..)
import ElmHtml.Constants exposing (..)
import ElmHtml.Helpers exposing (..)


{-| Type tree for representing Elm's Html
- TextTag is just a plain old bit of text.
- NodeEntry is an actual HTML node, e.g a div
- CustomNode are nodes defined to work with the renderer in some way, e.g webgl/markdown
- MarkdownNode is just a wrapper for CustomNode designed just for markdown
-}
type ElmHtml
    = TextTag TextTagRecord
    | NodeEntry NodeRecord
    | CustomNode CustomNodeRecord
    | MarkdownNode MarkdownNodeRecord
    | NoOp


{-| Text tags just contain text
-}
type alias TextTagRecord =
    { text : String }


{-| A node contains the `tag` as a string, the children, the facts (e.g attributes) and descendantsCount
-}
type alias NodeRecord =
    { tag : String
    , children : List ElmHtml
    , facts :
        Facts
        --, namespace : String
    , descendantsCount : Int
    }


{-| A markdown node contains facts (e.g attributes) and the model used by markdown
-}
type alias MarkdownNodeRecord =
    { facts : Facts
    , model : MarkdownModel
    }


{-| Custom nodes contain facts (e.g attributes) and a json value for the model
-}
type alias CustomNodeRecord =
    { facts : Facts
    , model : Json.Decode.Value
    }


{-| Tagger holds the map function when Html.Map is used, the tagger
should then be applied to events comming from descendant nodes
-}
type alias Tagger =
    Json.Decode.Value


{-| Facts contain various dictionaries and values for a node
- styles are a mapping of rules
- events may be a json object containing event handlers
- attributes are pulled out into stringAttributes and boolAttributes - things with string values go into
  stringAttributes, things with bool values go into boolAttributes
-}
type alias Facts =
    { styles : Dict String String
    , events : Dict String Json.Decode.Value
    , taggers : List Tagger
    , attributeNamespace : Maybe Json.Decode.Value
    , stringAttributes : Dict String String
    , boolAttributes : Dict String Bool
    }


{-| Type for representing the five kinds of elements according to HTML 5
[spec](https://html.spec.whatwg.org/multipage/syntax.html#elements-2).
Used to handle different rendering behavior depending on the type of element.
-}
type ElementKind
    = VoidElements
    | RawTextElements
    | EscapableRawTextElements
    | ForeignElements
    | NormalElements


{-| decode a json object into ElmHtml
-}
decodeElmHtml : Json.Decode.Decoder ElmHtml
decodeElmHtml =
    decodeElmHtmlWithTaggers []


decodeElmHtmlWithTaggers : List Tagger -> Json.Decode.Decoder ElmHtml
decodeElmHtmlWithTaggers taggers =
    field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\typeString ->
                case typeString of
                    "text" ->
                        Json.Decode.map TextTag (decodeTextTag)

                    "keyed-node" ->
                        Json.Decode.map NodeEntry (decodeKeyedNode taggers)

                    "node" ->
                        Json.Decode.map NodeEntry (decodeNode taggers)

                    "custom" ->
                        decodeCustomNode taggers

                    "tagger" ->
                        decodeTagger taggers

                    "thunk" ->
                        field "node" (decodeElmHtmlWithTaggers taggers)

                    _ ->
                        Json.Decode.fail ("No such type as " ++ typeString)
            )


{-| decode text tag
-}
decodeTextTag : Json.Decode.Decoder TextTagRecord
decodeTextTag =
    field "text" (Json.Decode.andThen (\text -> Json.Decode.succeed { text = text }) Json.Decode.string)


{-| encode text tag
-}
encodeTextTag : TextTagRecord -> Json.Encode.Value
encodeTextTag { text } =
    Json.Encode.object [ ( "text", Json.Encode.string text ) ]


{-| decode a tagger
-}
decodeTagger : List Tagger -> Json.Decode.Decoder ElmHtml
decodeTagger taggers =
    Json.Decode.field "tagger" (Json.Decode.value)
        |> Json.Decode.andThen
            (\tagger ->
                Json.Decode.oneOf
                    [ Json.Decode.at [ "node" ] (decodeElmHtmlWithTaggers (tagger :: taggers))
                    , Json.Decode.at [ "text" ] (decodeElmHtmlWithTaggers (tagger :: taggers))
                    , Json.Decode.at [ "custom" ] (decodeElmHtmlWithTaggers (tagger :: taggers))
                    ]
            )


decodeKeyedNode : List Tagger -> Json.Decode.Decoder NodeRecord
decodeKeyedNode taggers =
    let
        -- elm stores keyed nodes as tuples
        -- we only want to decode the html, in the second property
        decodeSecondNode =
            Json.Decode.field "_1" (decodeElmHtmlWithTaggers taggers)
    in
        Json.Decode.map4 NodeRecord
            (Json.Decode.field "tag" Json.Decode.string)
            (Json.Decode.field "children" (Json.Decode.list decodeSecondNode))
            (Json.Decode.field "facts" (decodeFacts taggers))
            (Json.Decode.field "descendantsCount" Json.Decode.int)


{-| decode a node record
-}
decodeNode : List Tagger -> Json.Decode.Decoder NodeRecord
decodeNode taggers =
    Json.Decode.map4 NodeRecord
        (field "tag" Json.Decode.string)
        (field "children" (Json.Decode.list (decodeElmHtmlWithTaggers taggers)))
        (field "facts" (decodeFacts taggers))
        (field "descendantsCount" Json.Decode.int)


{-| encode a node record: currently does not support facts or children
-}
encodeNodeRecord : NodeRecord -> Json.Encode.Value
encodeNodeRecord record =
    Json.Encode.object
        [ ( "tag", Json.Encode.string record.tag )
          --, ( "children", Json.Encode.list encodeElmHtml)
          --, ( "facts", encodeFacts)
        , ( "descendantsCount", Json.Encode.int record.descendantsCount )
        ]


{-| decode custom node into either markdown or custom
-}
decodeCustomNode : List Tagger -> Json.Decode.Decoder ElmHtml
decodeCustomNode taggers =
    Json.Decode.oneOf
        [ Json.Decode.map MarkdownNode (decodeMarkdownNodeRecord taggers)
        , Json.Decode.map CustomNode (decodeCustomNodeRecord taggers)
        ]


{-| decode custom node record
-}
decodeCustomNodeRecord : List Tagger -> Json.Decode.Decoder CustomNodeRecord
decodeCustomNodeRecord taggers =
    Json.Decode.map2 CustomNodeRecord
        (field "facts" (decodeFacts taggers))
        (field "model" Json.Decode.value)


{-| decode markdown node record
-}
decodeMarkdownNodeRecord : List Tagger -> Json.Decode.Decoder MarkdownNodeRecord
decodeMarkdownNodeRecord taggers =
    Json.Decode.map2 MarkdownNodeRecord
        (field "facts" (decodeFacts taggers))
        (field "model" decodeMarkdownModel)


{-| decode the styles
-}
decodeStyles : Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.oneOf
        [ field styleKey (Json.Decode.dict Json.Decode.string)
        , Json.Decode.succeed Dict.empty
        ]


{-| encode styles
-}
encodeStyles : Dict String String -> Json.Encode.Value
encodeStyles stylesDict =
    let
        encodedDict =
            stylesDict
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, Json.Encode.string v ))
    in
        Json.Encode.object [ ( styleKey, Json.Encode.object encodedDict ) ]


{-| grab things from attributes via a decoder, then anything that isn't filtered on
    the object
-}
decodeOthers : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeOthers otherDecoder =
    decodeAttributes otherDecoder
        |> Json.Decode.andThen
            (\attributes ->
                decodeDictFilterMap otherDecoder
                    |> Json.Decode.map (filterKnownKeys >> Dict.union attributes)
            )


{-| For a given decoder, keep the values from a dict that pass the decoder
-}
decodeDictFilterMap : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeDictFilterMap decoder =
    Json.Decode.dict Json.Decode.value
        |> Json.Decode.map
            (Dict.toList
                >> List.filterMap
                    (\( key, value ) ->
                        case Json.Decode.decodeValue decoder value of
                            Err _ ->
                                Nothing

                            Ok v ->
                                Just ( key, v )
                    )
                >> Dict.fromList
            )


decodeAttributes : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeAttributes decoder =
    Json.Decode.oneOf
        [ Json.Decode.field attributeKey (decodeDictFilterMap decoder)
        , Json.Decode.succeed Dict.empty
        ]


decodeEvents : Json.Decode.Decoder (Dict String Json.Decode.Value)
decodeEvents =
    Json.Decode.oneOf
        [ Json.Decode.field eventKey (Json.Decode.dict Json.Decode.value)
        , Json.Decode.succeed Dict.empty
        ]


{-| decode fact
-}
decodeFacts : List Tagger -> Json.Decode.Decoder Facts
decodeFacts taggers =
    Json.Decode.map6 Facts
        (decodeStyles)
        (decodeEvents)
        (Json.Decode.succeed taggers)
        (Json.Decode.maybe (Json.Decode.field attributeNamespaceKey Json.Decode.value))
        (decodeOthers Json.Decode.string)
        (decodeOthers Json.Decode.bool)


{-| Just empty facts
-}
emptyFacts : Facts
emptyFacts =
    { styles = Dict.empty
    , events = Dict.empty
    , taggers = []
    , attributeNamespace = Nothing
    , stringAttributes = Dict.empty
    , boolAttributes = Dict.empty
    }


{-| A list of Void elements as defined by the HTML5 specification. These
   elements must not have closing tags and most not be written as self closing
   either
-}
voidElements : List String
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]


{-| A list of all Raw Text Elements as defined by the HTML5 specification. They
   can contain only text and have restrictions on which characters can appear
   within its innerHTML
-}
rawTextElements : List String
rawTextElements =
    [ "script", "style" ]


{-| A list of all Escapable Raw Text Elements as defined by the HTML5
   specification. They can have text and character references, but the text must
   not contain an ambiguous ampersand along with addional restrictions:
   https://html.spec.whatwg.org/multipage/syntax.html#cdata-rcdata-restrictions
-}
escapableRawTextElements : List String
escapableRawTextElements =
    [ "textarea", "title" ]



{- Foreign elements are elements from the MathML namespace and the
   SVG namespace. TODO: detect these nodes and handle them correctly. Right
   now they will just be treated as Normal elements.
-}


{-| Identify the kind of element. Helper to convert an tag name into a type for
pattern matching.
-}
toElementKind : String -> ElementKind
toElementKind element =
    if List.member element voidElements then
        VoidElements
    else if List.member element rawTextElements then
        RawTextElements
    else if List.member element escapableRawTextElements then
        EscapableRawTextElements
    else
        -- All other allowed HTML elements are normal elements
        NormalElements
