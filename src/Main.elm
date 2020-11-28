module Main exposing (Distance, HeartRateInfo, Model, Msg(..), Time(..), ZoneInfo, aerobicZone, formatTime, hrStats, hrView, init, lactateTreshold, longRunZone, main, marathonZone, mp, mul, mulHR, pace, parseHR, parseTime, recovery, statsHelper, timeView, update, view, zoneView)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, input, main_, node, section, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (map)
import String exposing (pad, split)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { timeStr : String
    , restingHR : String
    , maxHR : String
    , criticalPower: String
    }


init : Model
init =
    { timeStr = "2:25:00", restingHR = "45", maxHR = "200", criticalPower = "378" }


type Time
    = Seconds Int


type alias HeartRateInfo =
    { resting : Int, max : Int, cp : Int }


parseHR : Model -> Maybe HeartRateInfo
parseHR model =
    case ( String.toInt model.restingHR, String.toInt model.maxHR, String.toInt model.criticalPower ) of
        ( Just r, Just m, Just cp ) ->
            Just { resting = r, max = m, cp = cp }

        _ ->
            Nothing


parseTime : String -> Maybe Time
parseTime time =
    let
        comps =
            split ":" time |> map String.toInt
    in
    case comps of
        [ Just h, Just m ] ->
            Just (Seconds (h * 3600 + m * 60))

        [ Just h, Just m, Just s ] ->
            Just (Seconds (h * 3600 + m * 60 + s))

        _ ->
            Nothing


formatTime : Time -> String
formatTime (Seconds totalSeconds) =
    let
        minutes =
            floor (toFloat totalSeconds / 60)

        remainingSeconds =
            modBy 60 totalSeconds
    in
    String.fromInt minutes ++ ":" ++ pad 2 '0' (String.fromInt remainingSeconds)


type alias Distance =
    Float


pace : Time -> Distance -> Time
pace (Seconds seconds) distance =
    Seconds (round (toFloat seconds / distance))



-- UPDATE


type Msg
    = ChangeTime String
    | ChangeRestingHR String
    | ChangeMaxHR String
    | ChangeCP String


mul : Time -> Float -> Time
mul (Seconds s) d =
    Seconds (round (toFloat s * d))


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTime str ->
            { model | timeStr = str }

        ChangeRestingHR str ->
            { model | restingHR = str }

        ChangeMaxHR str ->
            { model | maxHR = str }

        ChangeCP str ->
            { model | criticalPower = str }

-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "wrapper" ]
        [ section [ class "container" ]
            [ h1 [] [ text "Pfitzinger Marathon Training Calculator" ]
            , h2 [] [ text "Pacing" ]
            , timeView model
            , hrView model
            , h2 [] [ text "Marathon" ]
            , zoneView model marathonZone
            , h2 [] [ text "Long Run" ]
            , zoneView model longRunZone
            , h2 [] [ text "Lactate Treshold Run" ]
            , zoneView model lactateTreshold
            , h2 [] [ text "Aerobic Run" ]
            , zoneView model aerobicZone
            , h2 [] [ text "Recovery Run" ]
            , zoneView model recovery
            ]
        ]


timeView : Model -> Html Msg
timeView model =
    div []
        [ text "Marathon Time"
        , input [ placeholder "Marathon Goal Time", value model.timeStr, onInput ChangeTime ] []
        ]


type alias ZoneInfo =
    { minPace : Float, maxPace : Float, minHr : Float, maxHr : Float, minPower: Float, maxPower: Float }


aerobicZone : ZoneInfo
aerobicZone =
    { minPace = 1.15, maxPace = 1.25, minHr = 0.62, maxHr = 0.75, minPower = 0.75, maxPower = 0.85 }


longRunZone : ZoneInfo
longRunZone =
    { minPace = 1.1, maxPace = 1.2, minHr = 0.65, maxHr = 0.78, minPower = 0.8, maxPower = 0.9 }


marathonZone : ZoneInfo
marathonZone =
    { minPace = 1, maxPace = 1, minHr = 0.73, maxHr = 0.84, minPower = 0.92, maxPower = 1 }


lactateTreshold : ZoneInfo
lactateTreshold =
    { minPace = 0.85, maxPace = 0.95, minHr = 0.77, maxHr = 0.88, minPower = 0.95, maxPower = 1.05 }


recovery : ZoneInfo
recovery =
    { minPace = 1.15, maxPace = 1.25, minHr = 0.6, maxHr = 0.7, minPower = 0, maxPower = 0.8 }


mp : Time -> Time
mp time =
    pace time 42.195


statsHelper : Time -> ZoneInfo -> Html Msg
statsHelper marathonPace m =
    div []
        [ text
            (" Pace: " ++ formatTime (mul marathonPace m.maxPace)
                ++ (if m.maxPace == m.minPace then
                        ""
                    else
                        "-" ++ formatTime (mul marathonPace m.minPace)
                   )
                ++ "/km"
            )
        ]


hrView : Model -> Html Msg
hrView model =
    div []
        [ div []
            [ text "Resting Heart Rate"
            , input [ placeholder "Resting Heart Rate", value model.restingHR, onInput ChangeRestingHR ] []
            ]
        , div []
            [ text "Max Heart Rate"
            , input [ placeholder "Max Heart Rate", value model.maxHR, onInput ChangeMaxHR ] []
            ]
        , div []
            [ text "Critical Power"
            , input [ placeholder "CriticalPower", value model.criticalPower, onInput ChangeCP ] []
            ]
        ]


zoneView : Model -> ZoneInfo -> Html Msg
zoneView model zone =
    div []
        [ Maybe.withDefault (div [] [ text "No Goal Pace" ]) (Maybe.map (\time -> statsHelper (mp time) zone) (parseTime model.timeStr))
        , Maybe.withDefault (div [] [ text "No HR" ]) (Maybe.map (\hr -> hrStats hr zone) (parseHR model))
        , Maybe.withDefault (div [] [ text "No CP" ]) (Maybe.map (\hr -> powerStats hr zone) (parseHR model))
        ]


hrStats : HeartRateInfo -> ZoneInfo -> Html Msg
hrStats hr zone =
    div [] [ text (String.fromInt (mulHR hr zone.minHr) ++ "-" ++ String.fromInt (mulHR hr zone.maxHr) ++ " bpm") ]

powerStats : HeartRateInfo -> ZoneInfo -> Html Msg
powerStats hr zone =
    div [] [ text (String.fromInt (round ((toFloat hr.cp) * zone.minPower)) ++ "-" ++ String.fromInt (round ((toFloat hr.cp) * zone.maxPower)) ++ " W") ]


mulHR : HeartRateInfo -> Float -> Int
mulHR hr m =
    hr.resting + round (toFloat (hr.max - hr.resting) * m)
