module Tests exposing (..)

import Dict
import ElmHtml.InternalTypes exposing (Attribute(..), decodeAttribute, ElmHtml, ElmHtml(..), Facts, NodeRecord, Tagger, EventHandler, decodeElmHtml)
import Expect
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, disabled, value, colspan, style)
import Html.Events exposing (onCheck, onClick, onInput)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode exposing (decodeValue)
import Json.Encode
import Native.HtmlAsJson
import Test exposing (..)


elmHtml : Test
elmHtml =
    describe "ElmHtml parsing"
        [ test "parsing a node" <|
            \() ->
                div [] []
                    |> fromHtml
                    |> Expect.equal (Ok (NodeEntry decodedNode))
        , test "parsing a text" <|
            \() ->
                text "foo"
                    |> fromHtml
                    |> Expect.equal (Ok (TextTag { text = "foo" }))
        , test "parsing attributes" <|
            \() ->
                let
                    facts =
                        { decodedFacts
                            | stringAttributes = Dict.fromList [ ( "className", "foo" ), ( "value", "bar" ) ]
                            , boolAttributes = Dict.fromList [ ( "disabled", True ) ]
                        }

                    expected =
                        { decodedNode | tag = "button", facts = facts }
                in
                    button [ class "foo", value "bar", disabled True ] []
                        |> fromHtml
                        |> Expect.equal (Ok (NodeEntry expected))
        , test "parsing children" <|
            \() ->
                let
                    expected =
                        { decodedNode
                            | children = [ NodeEntry decodedNode, TextTag { text = "foo" } ]
                            , descendantsCount = 2
                        }
                in
                    div []
                        [ div [] []
                        , text "foo"
                        ]
                        |> fromHtml
                        |> Expect.equal (Ok (NodeEntry expected))
        , describe "parsing events"
            [ testParsingEvent "click" (onClick SomeMsg)
            , testParsingEvent "input" (onInput InputMsg)
            , testParsingEvent "change" (onCheck CheckMsg)
            ]
        , describe "parsing Html.map"
            [ test "adds the correct tagger to a mapped button" <|
                \() ->
                    let
                        taggedNode =
                            input [ onInput identity ] []
                                |> Html.map (\msg -> msg ++ "bar")
                                |> fromHtml
                    in
                        taggedNode
                            |> Result.andThen (simulate "input" "{\"target\": {\"value\": \"foo\"}}")
                            |> Expect.equal (Ok "foobar")
            , test "adds two taggers to a double mapped button with changing types" <|
                \() ->
                    let
                        taggedNode =
                            input [ onInput identity ] []
                                |> Html.map (\str -> [ str ] ++ [ "bar" ])
                                |> Html.map (\list -> ( list, "baz" ))
                                |> fromHtml
                    in
                        taggedNode
                            |> Result.andThen (simulate "input" "{\"target\": {\"value\": \"foo\"}}")
                            |> Expect.equal (Ok ( [ "foo", "bar" ], "baz" ))
            ]
        ]


attributes : Test
attributes =
    describe "Attribute parsing"
        [ test "parsing Attribute" <|
            \() ->
                colspan 1
                    |> fromAttribute
                    |> Expect.equal (Ok (Attribute { key = "colspan", value = "1" }))
        , test "parsing NamespacedAttribute" <|
            \() ->
                xlinkHref "#id"
                    |> fromAttribute
                    |> Expect.equal
                        (Ok (NamespacedAttribute { key = "xlink:href", value = "#id", namespace = "http://www.w3.org/1999/xlink" }))
        , test "parsing Property" <|
            \() ->
                disabled True
                    |> fromAttribute
                    |> Expect.equal (Ok (Property { key = "disabled", value = Json.Encode.bool True }))
        , test "parsing Event" <|
            \() ->
                onClick ()
                    |> fromAttribute
                    |> Expect.equal (Ok (Event { key = "click", decoder = Json.Decode.succeed (), options = Html.Events.defaultOptions }))
        , test "parsing Styles" <|
            \() ->
                style [ ( "margin", "0" ) ]
                    |> fromAttribute
                    |> Expect.equal (Ok (Styles [ ( "margin", "0" ) ]))
        ]


type Msg
    = SomeMsg
    | InputMsg String
    | CheckMsg Bool


toJson : a -> Json.Decode.Value
toJson =
    Native.HtmlAsJson.unsafeCoerce


eventDecoder : EventHandler -> Json.Decode.Decoder msg
eventDecoder eventHandler =
    Native.HtmlAsJson.eventDecoder eventHandler


eventHandler : String -> Html a -> Json.Decode.Value
eventHandler eventName node =
    Native.HtmlAsJson.eventHandler eventName node


taggerFunction : Tagger -> (a -> msg)
taggerFunction tagger =
    Native.HtmlAsJson.taggerFunction tagger


taggedEventDecoder : List Tagger -> EventHandler -> Json.Decode.Decoder msg
taggedEventDecoder taggers eventHandler =
    case taggers of
        [] ->
            (eventDecoder eventHandler)

        [ tagger ] ->
            Json.Decode.map (taggerFunction tagger) (eventDecoder eventHandler)

        tagger :: taggers ->
            Json.Decode.map (taggerFunction tagger) (taggedEventDecoder taggers eventHandler)


eventAttributeDecoder : EventHandler -> Json.Decode.Decoder msg
eventAttributeDecoder =
    Native.HtmlAsJson.unsafeCoerce


fromAttribute : Html.Attribute a -> Result String (Attribute a)
fromAttribute attribute =
    toJson attribute
        |> decodeValue (decodeAttribute eventAttributeDecoder)


decodedNode : NodeRecord msg
decodedNode =
    { tag = "div"
    , children = []
    , facts = decodedFacts
    , descendantsCount = 0
    }


decodedFacts : Facts msg
decodedFacts =
    { styles = Dict.fromList []
    , events = Dict.fromList []
    , attributeNamespace = Nothing
    , stringAttributes = Dict.fromList []
    , boolAttributes = Dict.fromList []
    }


fromHtml : Html a -> Result String (ElmHtml msg)
fromHtml html =
    toJson html
        |> decodeValue (decodeElmHtml taggedEventDecoder)


simulate : String -> String -> ElmHtml msg -> Result String msg
simulate eventName event parsedHtml =
    case parsedHtml of
        NodeEntry node ->
            Dict.get eventName node.facts.events
                |> Result.fromMaybe "Tried to trigger event on something other than a NodeEntry"
                |> Result.andThen (\eventDecoder -> Json.Decode.decodeString eventDecoder event)

        _ ->
            Err "Tried to trigger event on something other than a NodeEntry"


testParsingEvent : String -> Html.Attribute a -> Test
testParsingEvent eventName eventAttribute =
    test ("parsing " ++ eventName) <|
        \() ->
            let
                node =
                    button [ eventAttribute ] []

                facts =
                    { decodedFacts
                        | events = Dict.fromList [ ( eventName, eventDecoder (eventHandler eventName node) ) ]
                    }

                expected =
                    { decodedNode | tag = "button", facts = facts }
            in
                node
                    |> fromHtml
                    |> Expect.equal (Ok (NodeEntry expected))
