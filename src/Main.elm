module Main exposing (init)
import Browser
import Array exposing (Array)
import Maybe exposing (andThen)
import Html exposing (Html, button, div, text, table, td, tr, i, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class, src)
import Time exposing (every)
import Random exposing (list, int, pair)
import Json.Decode as Decode
import Visuals exposing (..)

type Field = Hidden | Blank | Mine | Flag | Exploded | Number Int

type Mode = Normal | Marker

type alias Board = Array (Array Field)

type GameStatus = Stopped | Running | Won | Lost

type alias Model = { size: (Int, Int)
                  , discovered: Int
                  , board: Board
                  , bombs: List (Int, Int)
                  , mode: Mode
                  , flags: List (Int, Int)
                  , status: GameStatus
                  , playTime: Int
                }

type Msg = Start
          | NewGame (Int, Int)
          | ToggleMode
          | Click Int Int
          | ToggleFlag Int Int
          | Tick Time.Posix

onRightClick: msg -> Html.Attribute msg
onRightClick m =
  Html.Events.custom
    "contextmenu"
    (Decode.succeed {
        message = m
      , preventDefault = True
      , stopPropagation = True
      })

genBoard: (Int, Int) -> Board
genBoard (x, y) = Array.repeat x (Array.repeat y Hidden)

init: Int -> (Model, Cmd msg)
init _ = (
  { board = (genBoard (8, 8))
    , bombs = [], discovered = 0, size = (8, 8), mode = Normal
    , flags = [], status = Stopped, playTime = 0
  }
  , Cmd.none
  )

main = Browser.element { init = init
                        ,subscriptions = (\model -> Time.every 1000 Tick)
                        ,update = update, view = view}

isBomb: (Int, Int) -> List (Int, Int) -> Bool
isBomb f l = List.member f l

neighbours: (Int, Int) -> List (Int, Int)
neighbours (x, y) = [(x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y + 1)
    , (x, y - 1), (x - 1, y), (x - 1, y + 1), (x - 1, y - 1)]

directNeighbours: (Int, Int) -> List (Int, Int)
directNeighbours (x, y) = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

nearBombsCount: (Int, Int) -> List (Int, Int) -> Int
nearBombsCount (x, y) bombs =
  List.length (List.filter (\f -> List.member f bombs) (neighbours (x, y)))

nearFieldsOf: Field -> (Int, Int) -> Array (Array Field) -> List (Int, Int)
nearFieldsOf field (x, y) board =
  let
    fn = (\(x1, y1) ->
      (Array.get x1 board |> andThen (\r -> Array.get y1 r)) == Just field)
  in
  List.filter fn (neighbours (x, y))

nearFlagsCount: (Int, Int) -> Array (Array Field) -> Int
nearFlagsCount (x, y) board =
  List.length (nearFieldsOf Flag (x, y) board)

first: (a -> Bool) -> List a -> Maybe a
first f l = List.head <| List.filter f l

discoverable: (Int, Int) -> Model -> Bool
discoverable (x, y) model =
  let field = Array.get x model.board |> andThen (Array.get y) in
  field == (Just Hidden)

setField: (Int, Int) -> Field -> Board -> Board
setField (x, y) f board =
  case (Array.get x board) of
    (Just row) -> Array.set x (Array.set y f row) board
    _ -> board

discoverNeighbours: (Int, Int) -> Bool -> Model -> (Model, Cmd Msg)
discoverNeighbours (x, y) restricted model =
  let
    nl = if restricted
      then nearFieldsOf Hidden (x, y) model.board
      else neighbours (x, y)
  in
  List.foldl (\p (m, _) -> discover p m) (model, Cmd.none) nl

toggleFlagField: (Int, Int) -> Bool -> Model -> (Model, Cmd Msg)
toggleFlagField (x, y) toggle model =
  let
    board = setField (x, y) (if toggle then Flag else Hidden) model.board
    flags =
      if toggle then
        List.sort ((x, y) :: model.flags)
      else
        List.filter (\p -> not (p == (x, y))) model.flags
  in
  ({ model | flags = flags, board = board }
    , Cmd.none)

statFor: (Int, Int) -> Int -> Int -> GameStatus -> GameStatus
statFor (sx, sy) bombCount discovered status =
  case status of
    Running ->
      if discovered == (sx * sy - bombCount) then Won else status
    _ -> status

discover: (Int, Int) -> Model -> (Model, Cmd Msg)
discover (x, y) model =
  if not (model.status == Running) then
      (model, Cmd.none)
    else
      let
        row = Array.get x model.board
        field = andThen (Array.get y) row
      in
      case field of
        (Just Flag) ->
          if model.mode == Normal then
            (model, Cmd.none)
          else
            toggleFlagField (x, y) False model
        (Just Hidden) ->
          if model.mode == Marker then
            toggleFlagField (x, y) True model
          else if (isBomb (x,y) model.bombs) then
            let board = setField (x, y) Exploded model.board in
            ({ model | board = board, status = Lost }
              , Cmd.none)
          else
            let c = nearBombsCount (x, y) model.bombs in
            case c of
              0 ->
                if (discoverable (x, y) model) then
                  let
                    b = setField (x, y) Blank model.board
                    discovered = model.discovered + 1
                  in
                  discoverNeighbours (x, y) False {model | board = b
                                                , discovered = discovered}
                else
                  (model, Cmd.none)
              _ ->
                let
                  board = setField (x, y) (Number c) model.board
                  discovered = model.discovered + 1
                in
                ({ model | board = board, discovered = discovered }
                , Cmd.none)
        Just (Number n) ->
          if (nearFlagsCount (x, y) model.board) == n then
            discoverNeighbours (x, y) True model
          else
            (model, Cmd.none)
        _ -> (model, Cmd.none)

genBomb: Cmd Msg
genBomb = Random.generate NewGame (pair (int 0 7) (int 0 7))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      ({ model | board = (genBoard model.size)
        , discovered = 0, bombs = [], flags = [], status = Running
        , playTime = 0 }
      , genBomb)
    NewGame b ->
      if (List.member b model.bombs) then
        (model, genBomb)
      else
        let
          bombs = List.sort (b :: model.bombs)
          cmd = if (List.length bombs) == 10 then Cmd.none else genBomb
        in
        ({ model | bombs = bombs, status = Running }, cmd)
    ToggleMode ->
      let mode = if model.mode == Normal then Marker else Normal in
      ({ model | mode = mode }, Cmd.none)
    Click x y ->
      let (m, c) = discover (x, y) model in
      (
        { m |
          status = statFor m.size (List.length m.bombs) m.discovered m.status
        }, c
      )
    ToggleFlag x y ->
      if model.status == Running then
        let field = Array.get x model.board |> andThen (Array.get y) in
        case field of
          Just Flag -> toggleFlagField (x, y) False model
          Just Hidden -> toggleFlagField (x, y) True model
          _ -> (model, Cmd.none)
      else
        (model, Cmd.none)
    Tick _ ->
      case model.status of
        Running -> ({ model | playTime = model.playTime + 1 }, Cmd.none)
        _ -> (model, Cmd.none)

toHtmlField: Int -> Int -> Field -> Html Msg
toHtmlField x y f =
  let
    borders =
      [ style "border" "4px solid"
      , style "border-left-color" "lightgrey"
      , style "border-top-color" "lightgrey"
      , style "border-right-color" "#404040"
      , style "border-bottom-color" "#404040"
      ]
    attributes = List.append
      [ onClick (Click x y)
      , onRightClick (ToggleFlag x y)
      , style "text-align" "center"
      , style "size" "36px"
      , style "height" "28px"
      , style "width" "28px"
      , style "background-color" "grey"
      , style "background-color"
                    (if (f == Hidden || f == Flag)
                      then "darkgrey"
                      else "lightgrey")
      ]
      (if (f == Hidden || f == Flag)
        then borders
        else
          [ style "border" "solid 4px", style "border-color" "lightgrey" ])

  in
  case f of
    Hidden -> td attributes [text ""]
    Blank -> td attributes [text ""]
    Mine -> td attributes [i [class "fas", class "fa-bomb"] []]
    Number n -> td attributes [text (String.fromInt n)]
    Flag -> td attributes [i [class "far", class "fa-flag"] []]
    Exploded -> td attributes [i [class "fas", class "fa-bomb"
                                  , style "color" "red"] []]

toHtmlRow: Int -> Array Field -> Html Msg
toHtmlRow x arr =
  tr [] (Array.toList (Array.indexedMap (toHtmlField x) arr))

textForMode: Mode -> String
textForMode m =
  case m of
    Normal -> "Normal"
    _ -> "Marker"

faceFor: GameStatus -> Html msg
faceFor status =
  case status of
    Lost -> lostFace
    Won -> wonFace
    _ -> runningFace

view: Model -> Html Msg
view model =
  let
    flagCount = (List.length model.bombs) - (List.length model.flags)
    boardWidth = String.fromInt (2 + (Tuple.first model.size) * 40) ++ "px"
    digitWrapper = (\d -> div [ class "digit", style "margin" "0"
      , style "width" "25px", style "height" "50px", style "padding" "0"
      , style "float" "left" ] [ d ])
    bombsDisplay = Visuals.displayForWithSize flagCount 3
      |> List.map digitWrapper
    timeDisplay = Visuals.displayForWithSize model.playTime 3
      |> List.map digitWrapper
  in
  div [ style "width" boardWidth ]
    [ div [ class "statContainer", style "display" "flex" ] 
      [ div [ style "float" "left", style "width" "75px" ]
        [ div [ class "display", style "height" "50px", style "width" "75px" ]
          bombsDisplay
        ]
      , div
        [ class "faceholder", onClick Start, style "float" "left", style "margin-left" "auto", style "margin-right" "auto"
        , style "height" "50px", style "width" "50px" ]
        [ faceFor model.status ]
      , div [ style "float" "right", style "width" "75px" ]
        [ div [ class "display", style "height" "50px", style "width" "75px" ]
          timeDisplay
        ]
      ]
    , table [ style "border-collapse" "separate", style "clear" "both" ]
        (Array.toList (Array.indexedMap toHtmlRow model.board))
    , div
      [ onClick ToggleMode , style "width" boardWidth, style "height" "36px"
      , style "text-align" "center", style "vertical-align" "middle"
      , style "background-color" "lightgrey", style "line-height" "36px"
      ]
      [ text (textForMode model.mode) ]
    ]
