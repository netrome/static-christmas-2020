module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main =
    Browser.sandbox { init = PromptName "", update = update, view = view }


type Model
    = PromptName String
    | Action Content
    | Done
    | Failure


type alias Message =
    { author : String, time : String, text : String }


type alias Content =
    { name : String
    , messages : List Message
    , responses : List Message
    , messageBuffer : List String
    , currentInput : String
    , currentRemainder : String
    }


type Msg
    = NameUpdate String
    | NameSubmit String
    | ChatInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameUpdate name ->
            PromptName name

        NameSubmit name ->
            Action
                { name = name
                , messages = initialMessages
                , responses = responses
                , messageBuffer = messageBuffer
                , currentInput = ""
                , currentRemainder = "!oY"
                }

        ChatInput text ->
            case model of
                Action content ->
                    chatStep content text

                Done ->
                    Done

                _ ->
                    Failure


chatStep : Content -> String -> Model
chatStep content text =
    case content.currentRemainder of
        "" ->
            sendMessage content text

        _ ->
            forceMessageStep content text


forceMessageStep : Content -> String -> Model
forceMessageStep content text =
    let
        unconsed =
            String.uncons content.currentRemainder
    in
    case unconsed of
        Just ( c, rem ) ->
            Action
                { content
                    | currentInput = String.cons c content.currentInput
                    , currentRemainder = rem
                }

        _ ->
            Failure


sendMessage : Content -> String -> Model
sendMessage content text =
    case content.messageBuffer of
        newRemainder :: newMessageBuffer ->
            sendMessage2 content text newRemainder newMessageBuffer

        _ ->
            Done


sendMessage2 : Content -> String -> String -> List String -> Model
sendMessage2 content text newRemainder newMessageBuffer =
    let
        ( response, newResponses ) =
            newResponsesF content
    in
    Action
        { content
            | messages =
                content.messages
                    ++ [ { author = content.name, time = "13:37", text = String.dropRight 1 text }
                       , response
                       ]
            , currentInput = ""
            , currentRemainder = newRemainder
            , messageBuffer = newMessageBuffer
        }


newResponsesF : Content -> ( Message, List Message )
newResponsesF content =
    case content.responses of
        response :: remainder ->
            ( response, remainder )

        [] ->
            ( { author = "prince", time = "13:37", text = "Jag har inget mer att säga" }, [] )


sampleChannels : List String
sampleChannels =
    [ "ellie"
    , "elm-dev"
    , "elm-discuss"
    , "elm-format"
    , "elm-ui"
    , "general"
    , "news-and-links"
    ]


sampleActiveChannel : String
sampleActiveChannel =
    "elm-ui"


messageBuffer : List String
messageBuffer =
    List.map String.reverse
        [ "Tja!"
        , "Läget?"
        , "Jag är awesome"
        , "Och bäst i världen?"
        , "Du är cool"
        , "Jag är awesome"
        ]


responses : List Message
responses =
    [ { author = "prince", time = "13:37", text = "Hello, I am your crown prince" }
    , { author = "prince", time = "13:37", text = "Derp in tha sherp" }
    , { author = "prince", time = "13:37", text = "Flerpy ferpy" }
    ]


initialMessages : List Message
initialMessages =
    [ { author = "prince", time = "13:37", text = "Welcome to my empire." }
    ]


channelPanel : List String -> String -> Element msg
channelPanel channels activeChannel =
    let
        activeChannelAttrs =
            [ Background.color <| rgb255 117 179 201, Font.bold ]

        channelAttrs =
            [ paddingXY 15 5, width fill ]

        channelEl channel =
            el
                (if channel == activeChannel then
                    activeChannelAttrs ++ channelAttrs

                 else
                    channelAttrs
                )
            <|
                text ("# " ++ channel)
    in
    column
        [ height fill
        , width <| fillPortion 1
        , paddingXY 0 10
        , scrollbars
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map channelEl channels


chatPanel : Content -> String -> Element Msg
chatPanel content channel =
    let
        header =
            row
                [ width fill
                , paddingXY 20 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 200 200 200
                ]
                [ el [] <| text ("#" ++ channel)
                , Input.button
                    [ padding 5
                    , alignRight
                    , Border.width 1
                    , Border.rounded 3
                    , Border.color <| rgb255 200 200 200
                    ]
                    { onPress = Nothing
                    , label = text "Search"
                    }
                ]

        messageEntry message =
            column [ width fill, spacingXY 0 5 ]
                [ row [ spacingXY 10 0 ]
                    [ el [ Font.bold ] <| text message.author, text message.time ]
                , paragraph [] [ text message.text ]
                ]

        messagePanel =
            column [ padding 10, spacingXY 0 20, scrollbarY, height fill ] <|
                List.map messageEntry content.messages

        footer =
            el [ alignBottom, padding 20, width fill ] <|
                row
                    [ spacingXY 2 0
                    , width fill
                    , Border.width 2
                    , Border.rounded 4
                    , Border.color <| rgb255 200 200 200
                    ]
                    [ el
                        [ padding 5
                        , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
                        , Border.color <| rgb255 200 200 200
                        , mouseOver [ Background.color <| rgb255 86 182 139 ]
                        ]
                      <|
                        text "+"
                    , Input.text [ Background.color <| rgb255 255 255 255 ]
                        { label = Input.labelHidden "chat input"
                        , onChange = ChatInput
                        , placeholder = Nothing
                        , text = content.currentInput
                        }
                    ]
    in
    column [ height fill, width <| fillPortion 5 ]
        [ header
        , messagePanel
        , footer
        ]


view : Model -> Html Msg
view model =
    layout [] <| mainView model


mainView : Model -> Element.Element Msg
mainView model =
    case model of
        Action content ->
            viewAction content

        PromptName name ->
            promptName name

        Failure ->
            text "Impossibru!!!"

        Done ->
            text "Mission accomplished"


promptName : String -> Element.Element Msg
promptName name =
    column []
        [ text "Ditt namn"
        , Input.text []
            { label = Input.labelHidden "name input"
            , onChange = NameUpdate
            , placeholder = Nothing
            , text = name
            }
        , Input.button []
            { label =
                text "Chatta med prinsen"
            , onPress = Just (NameSubmit name)
            }
        ]


viewAction : Content -> Element.Element Msg
viewAction content =
    row [ height fill, width fill ]
        [ channelPanel sampleChannels sampleActiveChannel
        , chatPanel content sampleActiveChannel
        ]
