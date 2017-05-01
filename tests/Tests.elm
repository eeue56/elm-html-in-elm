module Tests exposing (..)

import Dict
import ElmHtml.InternalTypes exposing (ElmHtml, ElmHtml(..), Facts, NodeRecord, Tagger, decodeElmHtml)
import Expect
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (decodeValue)
import Native.HtmlAsJson
import Test exposing (..)


toJson : Html a -> Json.Decode.Value
toJson node =
    Native.HtmlAsJson.toJson node


eventHandler : String -> Html a -> Json.Decode.Value
eventHandler eventName node =
    Native.HtmlAsJson.eventHandler eventName node


taggerFunction : Json.Decode.Value -> (a -> msg)
taggerFunction tagger =
    Native.HtmlAsJson.taggerFunction tagger


decodedNode : NodeRecord
decodedNode =
    { tag = "div"
    , children = []
    , facts = decodedFacts
    , descendantsCount = 0
    }


decodedFacts : Facts
decodedFacts =
    { styles = Dict.fromList []
    , events = Dict.fromList []
    , taggers = []
    , attributeNamespace = Nothing
    , stringAttributes = Dict.fromList []
    , boolAttributes = Dict.fromList []
    }


fromHtml : Html a -> Result String ElmHtml
fromHtml =
    decodeValue decodeElmHtml << toJson


type Msg
    = SomeMsg
    | InputMsg String
    | CheckMsg Bool


all : Test
all =
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
                            div [] []
                                |> Html.map (\msg -> msg ++ "bar")
                    in
                        Expect.equal (simulateMsg taggedNode "foo") "foobar"
            , test "adds two taggers to a double mapped button with changing types" <|
                \() ->
                    let
                        taggedNode =
                            div [ onClick "somestring" ] []
                                |> Html.map (\str -> [ str ] ++ [ "bar" ])
                                |> Html.map (\list -> ( list, "baz" ))
                    in
                        Expect.equal (simulateMsg taggedNode "foo") ( [ "foo", "bar" ], "baz" )
            ]
        ]


simulateMsg : Html msg -> a -> msg
simulateMsg parsedHtml =
    case (fromHtml parsedHtml) of
        Ok (NodeEntry node) ->
            applyTaggers node.facts.taggers

        _ ->
            Debug.crash "taggers not found"


applyTaggers : List Tagger -> a -> msg
applyTaggers taggers msg =
    case taggers of
        [] ->
            Debug.crash "could not apply empty taggers"

        [ tagger ] ->
            (taggerFunction tagger msg)

        tagger :: taggers ->
            applyTaggers taggers (taggerFunction tagger msg)


testParsingEvent : String -> Html.Attribute a -> Test
testParsingEvent eventName eventAttribute =
    test ("parsing " ++ eventName) <|
        \() ->
            let
                node =
                    button [ eventAttribute ] []

                facts =
                    { decodedFacts
                        | events = Dict.fromList [ ( eventName, eventHandler eventName node ) ]
                    }

                expected =
                    { decodedNode | tag = "button", facts = facts }
            in
                node
                    |> fromHtml
                    |> Expect.equal (Ok (NodeEntry expected))
