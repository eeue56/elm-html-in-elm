module Tests exposing (..)

import Dict
import ElmHtml.InternalTypes exposing (ElmHtml, ElmHtml(..), Facts, NodeRecord, decodeElmHtml)
import Expect
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, value)
import Json.Decode exposing (decodeValue)
import Native.HtmlAsJson
import Test exposing (..)


{-| Convert a Html node to a Json
-}
toJson : Html a -> Json.Decode.Value
toJson node =
    Native.HtmlAsJson.toJson node


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
    , events = Nothing
    , attributeNamespace = Nothing
    , stringAttributes = Dict.fromList []
    , boolAttributes = Dict.fromList []
    }


fromHtml : Html a -> Result String ElmHtml
fromHtml =
    decodeValue decodeElmHtml << toJson


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
                    expectedFacts =
                        { decodedFacts
                            | stringAttributes = Dict.fromList [ ( "className", "foo" ), ( "value", "bar" ) ]
                            , boolAttributes = Dict.fromList [ ( "disabled", True ) ]
                        }

                    expectedButton =
                        { decodedNode | tag = "button", facts = expectedFacts }
                in
                    button [ class "foo", value "bar", disabled True ] []
                        |> fromHtml
                        |> Expect.equal (Ok (NodeEntry expectedButton))
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
        ]
