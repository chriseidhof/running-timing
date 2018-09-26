import Browser
import Html exposing (Html, button, div, text, input, node, h1, section, main_, h2, h3)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import String exposing (split, pad)
import List exposing (map)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = {
        timeStr : String,
        restingHR: String,
        maxHR: String
    }

init : Model
init = { timeStr = "2:55:00", restingHR = "55", maxHR = "205" }

type Time = Seconds Int
type alias HeartRateInfo = { resting: Int, max: Int }

parseHR : Model -> Maybe HeartRateInfo
parseHR model = case (String.toInt model.restingHR, String.toInt model.maxHR) of
                          (Just r, Just m) -> Just { resting = r, max = m}
                          _ -> Nothing

parseTime : String -> Maybe Time
parseTime time = let comps = split ":" time |> map (String.toInt) 
          in
    case comps of
              [Just h, Just m] -> Just (Seconds (h*3600 + m*60))
              [Just h, Just m, Just s] -> Just (Seconds (h*3600 + m*60 + s))
              _ -> Nothing

formatTime : Time -> String
formatTime (Seconds totalSeconds) =
    let minutes = floor ((toFloat totalSeconds) / 60)
        remainingSeconds = modBy 60 totalSeconds
    in ((String.fromInt minutes)) ++ ":" ++ (pad 2 '0' (String.fromInt remainingSeconds))

type alias Distance = Float

pace: Time -> Distance -> Time
pace (Seconds seconds) distance = Seconds (round ((toFloat seconds) / distance))


-- UPDATE

type Msg = ChangeTime String
         | ChangeRestingHR String
         | ChangeMaxHR String

mul : Time -> Float -> Time
mul (Seconds s) d = Seconds (round ((toFloat s) * d))

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeTime str -> { model | timeStr = str }
    ChangeRestingHR str -> { model | restingHR = str }
    ChangeMaxHR str -> { model | maxHR = str }


-- VIEW


view : Model -> Html Msg
view model = main_ [class "wrapper"] [
          section [class "container"] ([
             h1 [] [text "Pfitzinger Marathon Training Calculator"],
             h2 [] [text "Pacing"],
             timeView model,
             h2 [] [text "Heart Rate"],
             hrView model,
             h2 [] [text "Aerobic Run"],
             zoneView model aerobicZone,
             h2 [] [text "Long Run"],
             zoneView model longRunZone,
             h2 [] [text "Marathon"],
             zoneView model marathonZone,
             h2 [] [text "Lactate Treshold Run"],
             zoneView model lactateTreshold,
             h2 [] [text "Recovery Run"],
             zoneView model recovery
             ] ++ (map stylesheet milligram))
          ]

timeView : Model -> Html Msg
timeView model =
  div []
    ([ text "Marathon Time", input [ placeholder "Marathon Goal Time", value model.timeStr, onInput ChangeTime ] []
    ] ++ info model)

info : Model -> List (Html Msg)
info model = [Maybe.withDefault (div [] [text "Can't parse"]) (Maybe.map stats (parseTime model.timeStr))]

type alias ZoneInfo = { minPace : Float, maxPace : Float, minHr : Float, maxHr : Float }

aerobicZone : ZoneInfo
aerobicZone = { minPace = 1.15, maxPace = 1.25, minHr = 0.62, maxHr = 0.75 }

longRunZone : ZoneInfo
longRunZone = { minPace = 1.1, maxPace = 1.2, minHr = 0.65, maxHr = 0.78 }

marathonZone : ZoneInfo
marathonZone = { minPace = 1, maxPace = 1, minHr = 0.73, maxHr = 0.84 }

lactateTreshold : ZoneInfo
lactateTreshold = { minPace = 0.85, maxPace = 0.95, minHr = 0.77, maxHr = 0.88 }

recovery : ZoneInfo
recovery = { minPace = 1.25, maxPace = 1.15, minHr = 0.6, maxHr = 0.7 }

mp : Time -> Time
mp time = pace time 42.195

stats : Time -> Html Msg
stats time = let marathonPace = pace time 42.195
  in div [] [ text ("Pace: " ++ (formatTime marathonPace) ++ "/km") ]

statsHelper : Time -> ZoneInfo -> Html Msg
statsHelper marathonPace m = div [] [ text (" Pace: " ++ (formatTime (mul marathonPace m.maxPace)) ++ "-" ++  (formatTime (mul marathonPace m.minPace))++ "/km") ]


hrView : Model -> Html Msg
hrView model =
  div []
   ([ div []
      [ text "Resting Heart Rate"
      , input [ placeholder "Resting Heart Rate", value model.restingHR, onInput ChangeRestingHR ] []
      ]
    , div []
      [ text "Max Heart Rate"
      , input [ placeholder "Max Heart Rate", value model.maxHR, onInput ChangeMaxHR ] []
      ]
    ])

zoneView : Model -> ZoneInfo -> Html Msg
zoneView model zone = div [] [
   Maybe.withDefault (div [] [text "No Goal Pace"]) (Maybe.map (\time -> statsHelper (mp time) zone) (parseTime model.timeStr)),
   Maybe.withDefault (div [] [text "No HR"]) (Maybe.map (\hr -> hrStatsHelper2 hr zone) (parseHR model))
   ]

hrStatsHelper2 : HeartRateInfo -> ZoneInfo -> Html Msg
hrStatsHelper2 hr zone = div [] [text ((String.fromInt (mulHR hr zone.minHr)) ++ "-" ++ (String.fromInt (mulHR hr zone.maxHr)) ++ " bpm")]


mulHR : HeartRateInfo -> Float -> Int
mulHR hr m = hr.resting + (round (toFloat (hr.max - hr.resting) * m))

-- hrStats : HeartRateInfo -> Html Msg
-- hrStats hr = div [] (map (hrStatsHelper hr) hrZones)
-- 
-- hrStatsHelper : HeartRateInfo -> (String, Float, Float) -> Html Msg
-- hrStatsHelper hr (str, min, max) = div [] [text (str ++ ": " ++ (String.fromInt (mulHR hr min)) ++ "-" ++ (String.fromInt (mulHR hr max)) ++ " bpm")]

stylesheet url =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      url
            ]
        children = []
    in 
        node tag attrs children

milligram : List String
milligram = 
   [ "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
   , "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
   , "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css"
   ]
