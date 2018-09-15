import Browser
import Html exposing (Html, button, div, text, input, node, h1)
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
view model = div [] ([
             h1 [] [text "Pfitzinger Marathon Training Calculator"],
             timeView model,
             hrView model
             ] ++ (map stylesheet milligram))

timeView : Model -> Html Msg
timeView model =
  div []
    ([ text "Marathon Time", input [ placeholder "Marathon Goal Time", value model.timeStr, onInput ChangeTime ] []
    ] ++ info model)

info : Model -> List (Html Msg)
info model = [Maybe.withDefault (div [] [text "Can't parse"]) (Maybe.map stats (parseTime model.timeStr))]

paces : List ((String, Float))
paces =
  [ ("Marathon", 1)
  , ("Long Run Max", 1.1)
  , ("Long Run Min", 1.2)
  , ("Aerobic Max", 1.15)
  , ("Aerobic Min", 1.25)
  ]

stats : Time -> Html Msg
stats time = let marathonPace = pace time 42.195
  in
    div []
    (map (\(s,m) -> div [] [ text (s ++ " Pace: " ++ (formatTime (mul marathonPace m)) ++ "/km") ]) paces)


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
    ] ++ hrInfo model)

hrInfo : Model -> List (Html Msg)
hrInfo model = [Maybe.withDefault (div [] [text "Can't parse"]) (Maybe.map hrStats (parseHR model))]

hrZones : List (String, Float, Float)
hrZones =
    [ ("Long Run", 0.65, 0.78)
    , ("Aerobic", 0.62, 0.75)
    , ("Lactate Treshold", 0.77, 0.88)
    , ("Marathon Pace", 0.73, 0.84)
    , ("Recovery", 0.6, 0.7)
    ]

mulHR : HeartRateInfo -> Float -> Int
mulHR hr m = hr.resting + (round (toFloat (hr.max - hr.resting) * m))

hrStats : HeartRateInfo -> Html Msg
hrStats hr = div []
  (map (\(str, min, max) ->
           div [] [text (str ++ ": " ++ (String.fromInt (mulHR hr min)) ++ "-" ++ (String.fromInt (mulHR hr max)) ++ " bpm")]
      ) hrZones)

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
