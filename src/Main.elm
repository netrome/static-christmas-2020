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
    | Hacking
    | Hacked
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
    | DoHack


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
                , currentRemainder = "Hej herr prins, vad är det du behöver hjälp med?"
                }

        ChatInput text ->
            case model of
                Action content ->
                    chatStep content text

                Hacking ->
                    Hacking

                _ ->
                    Failure

        DoHack ->
            Hacked


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
                    | currentInput = content.currentInput ++ String.fromChar c
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
            Hacking


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
            , responses = newResponses
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
    [ "Ännu bättre familjen over 9000"
    , "das-famij"
    , "Askungen"
    , "kPrins_93"
    , "Kalle och hans vänner"
    ]


sampleActiveChannel : String
sampleActiveChannel =
    "kPrins_93"


messageBuffer : List String
messageBuffer =
    [ "Ja, och du behövde hjälp med pengarna?"
    , "Jaha. Så mycket för att lita på främlingar."
    , "Vänta, är du Batman?"
    , "Nananananananana nananananananana..."
    , "BATMAN!"
    , "Så...?"
    , "Vadå för något?"
    , "Ja duh, det vet ju alla!"
    , "Va? Så vad är månen gjord av då?"
    , "Choklad!"
    , "Choklaaaad choklaaaad a choko choko choko choklaaaad! ^^"
    , "Neutralisera jultomten? Men då blir ju julen inställd!"
    , "NEEEEEEEEEEEEEEEEJ!!!"
    , "Ehm, kanske lite..."
    , "Men hur ska jag?"
    , "Okej jag kan väl försöka då."
    ]


responses : List Message
responses =
    [ { author = "kPrins_93", time = "13:37", text = "Tack för frågan - som jag sa har jag nyligen blivit väldigt rik." }
    , { author = "kPrins_93", time = "13:37", text = "Inte riktigt, det sa jag bara för att få din uppmärksamhet." }
    , { author = "kPrins_93", time = "13:37", text = "Du kan lita på mig. Jag har redan spenderat alla mina pengar på att bli Batman." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Ja det är jag." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Är du klar?" }
    , { author = "kPrins_93/Batman", time = "13:37", text = "ಠ_ಠ" }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Ja - jag har infiltrerat NASA och upptäckt något hemskt..." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Du vet att månen är gjort av ost eller hur?" }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Det är en bluff! NASA har hittat på det." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Choklad." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Choklad!!!" }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Eller hur? Men det är inte vilken choklad som helst. I fel händer kan denna choklad användas för att neutralisera jultomten!" }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Exakt. NASA har precis lyckats extrahera ett prov av denna choklad, som de tänker använda för att stoppa julen för all framtid." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Än är det inte för sent dock, det är har DU kommer in. Jag har hört att du kan HTML." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Du behöver använda dina kunskaper för att hacka NASA och göra dig av med chokladen så den inte faller i fel händer." }
    , { author = "kPrins_93/Batman", time = "13:37", text = "Tack så mycket! Chokladen är i omloppsbana rakt över Norsbol. Hela världen räknar med dig nu!" }
    ]


initialMessages : List Message
initialMessages =
    [ { author = "kPrins_93", time = "13:37", text = "Hej! Vad bra att du loggat in." }
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

        Hacking ->
            hackingView

        Hacked ->
            hackedView


hackingView : Element.Element Msg
hackingView =
    column
        [ Background.image "https://media1.tenor.com/images/6ebf94800dd0d71ed77b6fea40821c43/tenor.gif?itemid=17954501"
        , height fill
        , width fill
        , padding 10
        , Font.alignLeft
        , Font.color <| rgb255 20 255 20
        ]
        [ column
            [ centerX
            , centerY
            , Background.color <| rgba255 40 40 40 0.5
            , padding 10
            ]
            [ Input.button [ Font.size 40, centerX, padding 40 ] { onPress = Just DoHack, label = text "Hacka Nasa med HTML" }
            ]
        ]


hackedView : Element.Element Msg
hackedView =
    column
        [ Background.image "https://cutewallpaper.org/21/matrix-gif-background/4K-1000-Min.-Green-Matrix-Glowing-Motion-Background-2160p-Efect-GIF.gif"
        , height fill
        , width fill
        , padding 10
        , Font.alignLeft
        , Font.color <| rgb255 20 255 20
        ]
        [ column
            [ centerX
            , centerY
            , Background.color <| rgba255 40 40 40 0.5
            , padding 10
            ]
            [ el [ Font.size 40, centerX, padding 40 ] (text "SUCCESS!")
            , paragraph [] [ text "Du har precis hackat NASA med HTML. Deras satellit med det hemliga chokladprovet har kraschlandat vid den östra väggen till Norsbol 103. Det är nu ditt ansvar att göra dig av med innehållet." ]
            ]
        ]


promptName : String -> Element.Element Msg
promptName name =
    el
        [ height fill
        , width fill
        , padding 10
        , Background.color <| rgb255 92 99 118
        ]
    <|
        column
            [ Background.color <| rgb255 250 250 250
            , centerX
            , centerY
            , padding 20
            , Border.rounded 10
            , spacing 10
            ]
            [ text "Ditt namn"
            , Input.text []
                { label = Input.labelHidden "name input"
                , onChange = NameUpdate
                , placeholder = Nothing
                , text = name
                }
            , Input.button
                [ padding 10
                , Font.color <| rgb255 250 250 250
                , Background.color <| rgb255 112 189 148
                , Border.rounded 3
                ]
                { label =
                    text "Logga in"
                , onPress = Just (NameSubmit name)
                }
            ]


viewAction : Content -> Element.Element Msg
viewAction content =
    row [ height fill, width fill ]
        [ channelPanel sampleChannels sampleActiveChannel
        , chatPanel content sampleActiveChannel
        ]
