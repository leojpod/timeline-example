module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, h1, main_, span, text)
import Html.Attributes exposing (class, style, id)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events.Extra.Mouse exposing (onMove)
import Round
import Task
import Browser.Events
import Browser.Dom 

import Time exposing (Posix, Zone)
import Time.Extra as Time
import Process
import Html exposing (nav)
import Html exposing (label)
import Html exposing (input)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Events exposing (onCheck)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Genre
    = Talk
    | Movie
    | Concert


type alias Event =
    { start : Posix
    , end : Posix
    , title : String
    , genre : Genre
    , track : Int
    }


type alias Model =
    { zone : Zone
    , now : Posix
    , cursorTime : Posix
    , timelineSpecs: {xPosition: Float, 
    width: Float}
    , granularity : Int
    , program : List Event
    }


fakeProgram : Zone -> Posix -> List Event
fakeProgram zone now =
    let
        today : Time.Parts
        today =
            now
                |> Time.floor Time.Day zone
                |> Time.posixToParts zone
    in
    [ { start = Time.partsToPosix zone { today | hour = 0, minute = 30 }
      , end = Time.partsToPosix zone { today | hour = 4, minute = 15 }
      , title = "Impossible state impossible"
      , genre = Talk
      , track = 1
      }
    , { start = Time.partsToPosix zone { today | hour = 8, minute = 15 }
      , end = Time.partsToPosix zone { today | hour = 12, minute = 30 }
      , title = "Diablo Swing Orchestra"
      , genre = Concert
      , track = 3
      }
    , { start = Time.partsToPosix zone { today | hour = 9 }
      , end = Time.partsToPosix zone { today | hour = 11, minute = 45 }
      , title = "Titanic, a react story"
      , genre = Movie
      , track = 2
      }
    , { start = Time.partsToPosix zone { today | hour = 14, minute = 33 }
      , end = Time.partsToPosix zone { today | hour = 18, minute = 55 }
      , title = "F# was not that sharp..."
      , genre = Movie
      , track = 1
      }
    ]


type alias Flags =
    { millisecNow : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { millisecNow } =
    ( { zone = Time.utc
      , now = Time.millisToPosix millisecNow
      , cursorTime = Time.millisToPosix millisecNow
      -- NOTE in a real stuff I'd find a way to now have this weird initialization
      , timelineSpecs = {xPosition = 0 , width  = 0}
      , granularity = 1
      , program = []
      }
    , Cmd.batch
        [ Time.here |> Task.perform UpdateTimeZone
        , Task.succeed () |> Task.perform (\() -> RefreshTimelineSpecs)
        ]
    )


type Msg
    = UpdateTimeZone Zone
    | UpdateNow Posix
    | UpdateCursor Float
    | NoOp
    | RefreshTimelineSpecs 
    | UpdateTimelineSpecs {xPosition : Float, width: Float} 
    | UpdateGranularity Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RefreshTimelineSpecs ->
            ( model, Browser.Dom.getElement "timeline" 
            |> Task.map (\{element} -> 
              UpdateTimelineSpecs {xPosition = element.x, width = element.width})
              |> Task.onError (\_ -> Process.sleep 10 
                |> Task.map (\_ -> RefreshTimelineSpecs ))
              |> Task.perform identity )
            
        UpdateTimeZone zone ->
            ( { model
                | zone = zone
                , program = fakeProgram zone model.now
              }
            , Cmd.none
            )

        UpdateNow now ->
            ( { model | now = now }, Cmd.none )

        UpdateCursor clientX ->
          let 
              {now, zone, timelineSpecs} = model

              timeRatio: Float 
              timeRatio = ((clientX - timelineSpecs.xPosition) / timelineSpecs.width )
                    |> clamp 0 1

              today: Posix
              today = now 
                  |> Time.floor Time.Day zone 

              cursorMinutes: Int
              cursorMinutes = 
                round <| timeRatio * 24 * 60

          in
          ({model | cursorTime = Time.add Time.Minute cursorMinutes model.zone today}, Cmd.none)


        UpdateTimelineSpecs specs ->
            ( { model | timelineSpecs = specs }, Cmd.none )


        UpdateGranularity granularity ->
            ( { model | granularity = granularity }, Cmd.none )


seconds : number -> number
seconds sec =
    sec * 1000


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [
    Time.every (seconds 30) UpdateNow
    , Browser.Events.onResize  (\_ _ -> RefreshTimelineSpecs)

    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Timeline example"
    , body =
        [ div [ class "flex flex-col justify-start w-full p-4 overflow-x-hidden space-y-4" ]
        [ nav [ class "flex items-center" ] [
          label [ class "flex items-center space-x-2" ] [
            span [] [ text "snap" ]
            , input [ type_ "checkbox", checked (model.granularity == 10), onCheck  (\bool -> 
              if bool then UpdateGranularity 10 
              else UpdateGranularity 1) ] []
            ]
          ]
              , main_ [ class "relative" ]
                [ timeline model
                , nowTracker model
                , pointerTracker model
                ]
            ]
        ]
    }


timeTracker : { hour : Int, minute : Int, barAttr : Attribute Msg, pillAttr : Attribute Msg } -> Html Msg
timeTracker { hour, minute, barAttr, pillAttr } =
    let
        percent : Float
        percent =
            100 * toFloat (hour * 60 + minute) / (24 * 60)
    in
    div
        [ class "absolute top-0 bottom-0 flex flex-col items-center"
        , class "pointer-events-none"

        -- WARNING: magic number incoming!
        -- there are nicer ways to correct the placement of the marker
        -- but since the display is rather "fixed" in width, it's almost (emphasis on "Almost") ok to check with calc and substract half of the width from our timeTracker ... yeah not the nicest but good enough to start
        , style "left" <| "calc(" ++ Round.round 2 percent ++ "% - 27px)"
        ]
        [ div
            [ class "flex-grow w-0 border-2 border-opacity-50"
            , barAttr
            ]
            []
        , span [ class "p-2 text-sm font-bold rounded-full", pillAttr ]
            [ text <| twoDigitsStr hour ++ ":" ++ twoDigitsStr minute
            ]
        ]


nowTracker : Model -> Html Msg
nowTracker { now, zone } =
    let
        { hour, minute } =
            Time.posixToParts zone now
    in
    timeTracker
        { hour = hour
        , minute = minute
        , barAttr = class "border-blue-400"
        , pillAttr = class "text-blue-100 bg-blue-400"
        }


pointerTracker : Model -> Html Msg
pointerTracker { zone, cursorTime, granularity } =
    let
        { hour, minute } =
            Time.posixToParts zone cursorTime

        roundedMinute: Int
        roundedMinute = 
          (round (toFloat minute / toFloat granularity)) * granularity


        (finalHour, finalMinute) = 
          if roundedMinute == 60 then 
            (hour +1, 0)
            else (hour, roundedMinute)
    in
    timeTracker
        { hour = finalHour
        , minute = finalMinute
        , barAttr = class "border-red-400"
        , pillAttr = class "text-red-100 bg-red-400"
        }


twoDigitsStr : Int -> String
twoDigitsStr =
    String.fromInt >> String.padLeft 2 '0'


genrePill : Genre -> Html Msg
genrePill genre =
    (case genre of
        Talk ->
            ( class "text-green-600 bg-green-200 border-green-600", "talk" )

        Movie ->
            ( class "text-blue-600 bg-blue-200 border-blue-600", "movie" )

        Concert ->
            ( class "text-red-600 bg-red-200 border-red-600", "talk" )
    )
        |> (\( extraAttr, genreName ) ->
                span [ class "p-1 border-2 rounded-md", extraAttr ] [ text genreName ]
           )


fromPosixToGridColumn : Zone -> Posix -> String
fromPosixToGridColumn zone time =
    let
        { hour, minute } =
            Time.posixToParts zone time

        roundedMinute : Int
        roundedMinute =
            round (toFloat minute / 15) * 15
    in
    "time-"
        ++ (if roundedMinute == 60 then
                twoDigitsStr (hour + 1) ++ "00"

            else
                twoDigitsStr hour ++ twoDigitsStr roundedMinute
           )


showEvent : Zone -> Event -> Html Msg
showEvent zone { start, end, title, genre, track } =
    div
        [ class "flex flex-col justify-start border-2 shadow-xl bg-gray-50 rounded-md"
        , style "grid-column" <| fromPosixToGridColumn zone start ++ " / " ++ ( fromPosixToGridColumn zone end)
        , style "grid-row" <| "track-" ++ String.fromInt track
        ]
        [ h1 [] [ text title ]
        , div [ class "self-end" ]
            [ genrePill genre
            ]
        ]


timeline : Model -> Html Msg
timeline { zone, program } =
    let
        -- hours
        hours : List Int
        hours =
            List.range 0 23

        minutes : List Int
        minutes =
            [ 0, 15, 30, 45 ]

        timeIndicator : Bool -> Int -> Html Msg
        timeIndicator isTop hour =
            let
                hourStr: String
                hourStr =
                    twoDigitsStr hour
            in
            div
                [ class "p-1 text-xs bg-blue-200 rounded-lg"
                , class "absolute origin-bottom-left -rotate-45"
                , attributeIf (not isTop) <| class "translate-y-4"
                , style "grid-row" <|
                    if isTop then
                        "top-time"

                    else
                        "bottom-time"
                , style "grid-column" <| "time-" ++ hourStr ++ "00"
                ]
                [ text <| hourStr ++ ":00" ]

        timeIndicators : Bool -> List (Html Msg)
        timeIndicators isTop =
            List.map (timeIndicator isTop) hours

        timelineSlots : List { hour : String, minute : String }
        timelineSlots =
            hours
                |> List.concatMap
                    (\hour ->
                        minutes
                            |> List.map (\minute -> { hour = hour, minute = minute })
                    )
                |> List.map
                    (\{ hour, minute } ->
                        { hour = twoDigitsStr hour
                        , minute = twoDigitsStr minute
                        }
                    )

        tracks : List Int
        tracks =
            List.range 1 3

        timeMarkers : List (Html Msg)
        timeMarkers =
            hours
                |> List.map
                    (\hour ->
                        div
                            [ class "w-0 border-2 border-gray-400"
                            , style "grid-column" <| "time-" ++ twoDigitsStr hour ++ "00"
                            , style "grid-row" "track-1 / bottom-time"
                            ]
                            []
                    )
    in
    div
        [ class "relative my-6 grid gap-y-2"
        , onMove (.clientPos >> Tuple.first >> (UpdateCursor))
        , style "grid-template-rows" <|
            String.join "\u{000D}"
                ("[top-time] 1.5em"
                    :: List.map
                        (\track ->
                            "[track-" ++ String.fromInt track ++ "] 1fr"
                        )
                        tracks
                    ++ [ "[bottom-time] 1.5em" ]
                )
        , style "grid-template-columns" <|
            String.join "\u{000D}"
                (List.map
                    (\{ hour, minute } ->
                        "[time-" ++ hour ++ minute ++ "] 1fr"
                    )
                    timelineSlots
                )
        ]
        (div
            [ class "bg-gray-100"
            , style "grid-column" "time-0000 / time"
            , style "grid-row" "track-1 / bottom-time"
            , id "timeline"
            ]
            []
            :: timeIndicators True
            ++ timeMarkers
            ++ List.map (showEvent zone) program
            ++ timeIndicators False
        )
