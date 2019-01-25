import Browser
import Array exposing (Array)
import Maybe exposing (andThen)
import Html exposing (Html, button, div, text, table, td, tr, i)
import Html.Events exposing(onClick)
import Html.Attributes exposing(style, class)
import Random exposing (list, int, pair)
import Json.Decode as Decode

type Field = Hidden | Blank | Mine | Flag | Exploded | Number Int

type Mode = Normal | Marker

type alias Board = Array (Array Field)

type alias Model = { board: Board
                  , bombs: List (Int, Int)
                  , mode: Mode
                  , flags: List (Int, Int)
                  , running: Bool
                }

type Msg = Start
          | NewGame (Int, Int)
          | ToggleMode
          | Click Int Int
          | ToggleFlag Int Int

onRightClick: msg -> Html.Attribute msg
onRightClick m =
  Html.Events.custom
    "contextmenu"
    (Decode.succeed {
        message = m
      , preventDefault = True
      , stopPropagation = True
      })

genBoard: Int -> Int -> Board
genBoard x y = Array.repeat x (Array.repeat y Hidden)

init: Int -> (Model, Cmd msg)
init _ = (
  { board = (genBoard 8 8), bombs = [], mode = Normal , flags = [], running = False}
  , Cmd.none
  )

main = Browser.element { init = init
                        ,subscriptions = (\model -> Sub.none)
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

populatedBlanks: (Int, Int) -> Model -> (Model, Cmd Msg)
populatedBlanks (x, y) model =
  List.foldl (\p (m, _) -> discover p m) (model, Cmd.none) (neighbours (x, y))

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

discover: (Int, Int) -> Model -> (Model, Cmd Msg)
discover (x, y) model =
  if not model.running then
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
            ({ model | board = board, running = False }
              , Cmd.none)
          else
            let c = nearBombsCount (x, y) model.bombs in
            case c of
              0 ->
                if (discoverable (x, y) model) then
                  let b = setField (x, y) Blank model.board in
                  populatedBlanks (x, y) {model | board = b}
                else
                  (model, Cmd.none)
              _ ->
                let board = setField (x, y) (Number c) model.board in
                ({ model | board = board }
                , Cmd.none)
        _ -> (model, Cmd.none)

genBomb: Cmd Msg
genBomb = Random.generate NewGame (pair (int 0 7) (int 0 7))

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      ({ model | board = (genBoard 8 8), bombs = [], flags = [], running = False }, genBomb)
    NewGame b ->
      if (List.member b model.bombs) then
        (model, genBomb)
      else
        let
          bombs = List.sort (b :: model.bombs)
          cmd = if (List.length bombs) == 10 then Cmd.none else genBomb
        in
        ({ model | bombs = bombs, running = True }, cmd)
    ToggleMode ->
      let mode = if model.mode == Normal then Marker else Normal in
      ({ model | mode = mode }, Cmd.none)
    Click x y -> discover (x, y) model
    ToggleFlag x y ->
      if model.running then
        let field = Array.get x model.board |> andThen (Array.get y) in
        case field of
          Just Flag -> toggleFlagField (x, y) False model
          Just Hidden -> toggleFlagField (x, y) True model
          _ -> (model, Cmd.none)
      else
        (model, Cmd.none)


toHtmlField: Int -> Int -> Field -> Html Msg
toHtmlField x y f =
  let
    attributes = [ onClick (Click x y)
                  ,onRightClick (ToggleFlag x y)
                  ,style "height" "20px"
                  ,style "width" "20px"
                  ,style "background-color"
                    (if f == Blank then "lightgrey" else "darkgrey")
                  ]
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

view: Model -> Html Msg
view model =
  let flagCount = (List.length model.bombs) - (List.length model.flags) in
  div [] [ button [onClick Start] [text "Start!"]
        , div [] [text (String.fromInt flagCount)]
        , button [onClick ToggleMode] [text (textForMode model.mode)]
        , table []
            (Array.toList (Array.indexedMap toHtmlRow model.board))
        ]
