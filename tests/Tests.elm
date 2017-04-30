module Tests exposing (..)

import Dict
import ElmHtml.InternalTypes exposing (ElmHtml, ElmHtml(..), Facts, NodeRecord, decodeElmHtml)
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
        ]


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
