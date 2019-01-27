module Visuals exposing (runningFace, wonFace, lostFace, displayForWithSize)
import Svg exposing (Svg, svg, circle, g, line, path, rect)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import List

withFace: List (Svg msg) -> Html msg
withFace nodes = svg [ viewBox "0 0 300 300" ]
  <| List.append
    [ circle [ cx "150", cy "150", r "145", fill "yellow", stroke "black" ] []]
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

withBackground: List (Svg msg) -> Html msg
withBackground nodes = svg [ viewBox "0 0 300 600" ]
  [ rect [ x "0", y "0", width "300", height "600", fill "black" ] []
  , g [ class "digit-background", fill "maroon" ]
    [ topLeft, bottomLeft, topRight, bottomRight, top, center, bottom ]
  , g [ class "digit-foreground", fill "red" ] nodes
  ]

topLeft: Svg msg
topLeft = Svg.path [ d "M 10 20 l 60 60 l 0 150 l -60 60 Z" ] []

bottomLeft: Svg msg
bottomLeft = Svg.path [ d "M 10 310 l 60 60 l 0 150 l -60 60 Z" ] []

topRight: Svg msg
topRight = Svg.path [ d "M 290 20 l -60 60 l 0 150 l 60 60 Z" ] []

bottomRight: Svg msg
bottomRight = Svg.path [ d "M 290 310 l -60 60 l 0 150 l 60 60 Z" ] []

top: Svg msg
top = Svg.path [ d "M 20 10 l 60 60 l 140 0 l 60 -60 Z" ] []

center: Svg msg
center =
  Svg.path [ d "M 30 300 l 50 -50 l 140 0 l 50 50 l -50 50 l -140 0 Z" ] []

bottom: Svg msg
bottom = Svg.path [ d "M 20 590 l 60 -60 l 140 0 l 60 60 Z" ] []

digitFor: Int -> Html msg
digitFor n =
  case n of
    1 -> withBackground [ topRight, bottomRight ]
    2 -> withBackground [ top, center, bottom, topRight, bottomLeft ]
    3 -> withBackground [ top, center, bottom, topRight, bottomRight ]
    4 -> withBackground [ topLeft, center, topRight, bottomRight ]
    5 -> withBackground [ topLeft, top, center, bottomRight, bottom ]
    6 -> withBackground [ topLeft, top, center, bottomLeft, bottomRight, bottom ]
    7 -> withBackground [ top, topRight, bottomRight ]
    8 -> withBackground
        [ topLeft, bottomLeft, topRight, bottomRight, top, center, bottom ]
    9 -> withBackground [ topLeft, top, center, topRight, bottomRight, bottom ]
    _ -> withBackground
        [ topLeft, bottomLeft, topRight, bottomRight, top, bottom ]

displayForWithSize: Int -> Int -> List (Html msg)
displayForWithSize n s =
  if n >= (10 ^ (s + 1)) then
    displayFor <| (10 ^ (s + 1)) - 1
  else
    let
      digits = displayFor n
      d = s - (List.length digits)
      padding = List.repeat d 0 |> List.map (digitFor)
    in
    padding ++ digits


displayFor: Int -> List (Html msg)
displayFor n = displayForHelper n []

displayForHelper: Int -> List (Html msg) -> List (Html msg)
displayForHelper n prev =
  case n of
    0 -> digitFor 0 :: prev
    _ -> displayForHelper (n // 10) ( digitFor (modBy 10 n) :: prev)
