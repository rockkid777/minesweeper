module Visuals exposing (runningFace, wonFace, lostFace)
import Svg exposing (Svg, svg, circle, g, line, path)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import List

withFace: List (Svg msg) -> Html msg
withFace nodes = svg [ viewBox "0 0 300 300" ]
  <| List.append
    [ circle [ cx "150", cy "150", r "145", fill "yellow"  ] []]
    nodes

deadEyes: Svg msg
deadEyes =
  g [ class "eyes", stroke "black", fill "black"
    , style "stroke-width: 10; stroke-linecap: round" ]
    [ g [ class "eye" ] [ line [ x1 "70", y1 "70", x2 "120", y2 "120" ] []
    , line [ x1 "70", y1 "120", x2 "120", y2 "70" ] [] ]
    , g [ class "eye" ] [ line [ x1 "180", y1 "70", x2 "230", y2 "120" ] []
    , line [ x1 "180", y1 "120", x2 "230", y2 "70" ] [] ] ]

runningEyes: Svg msg
runningEyes =
  g [ class "eyes", stroke "black", fill "black" ]
    [ g [ class "eye" ] [ circle [ cx "90", cy "100", r "30" ] [] ]
    , g [ class "eye" ] [ circle [ cx "210", cy "100", r "30" ] [] ] ]

sunglasses: Svg msg
sunglasses =
  g [ class "eyes", stroke "black", fill "black" ]
    [ g [ class "sunglass" ]
    [ Svg.path [ d "M 40 90 q 50 80 100 0 " ] []
    , Svg.path [ d "M 160 90 q 50 80 100 0 " ] []
    , line [ x1 "120", y1 "100", x2 "180", y2 "100"
      , style "stroke-width: 5" ] [] ] ]

sadMouth: Svg msg
sadMouth =
  g [ class "mouth", stroke "black", fill "black"
    , style "stroke-width: 10; stroke-linecap: round" ]
    [ Svg.path [ d "M 90 220 s 60 -120 120 0", fill "none" ] [] ]

neutralMouth: Svg msg
neutralMouth =
  g [ class "mouth", stroke "black", fill "black"
    , style "stroke-width: 10; stroke-linecap: round" ]
    [ line [ x1 "100", y1 "200", x2 "200", y2 "200" ] [] ]

lostFace: Html msg
lostFace = withFace [ deadEyes, sadMouth ]

wonFace: Html msg
wonFace = withFace [ sunglasses, neutralMouth ]

runningFace: Html msg
runningFace = withFace [ runningEyes, neutralMouth ]
