import Html
import Html.App
import Result
import Maze
import MazeRunner
import MazeView
import Keyboard
import Html


type alias Model =
  { maze : Maze.Maze
  , runner : MazeRunner.MazeRunner
  , finished : Bool
  }


type Msg
  = KeyMsg Keyboard.KeyCode


main =
  Html.App.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


init : (Model, Cmd Msg)
init =
  let
    maze = Result.withDefault Maze.emptyMaze Maze.sampleMaze
    runner = MazeRunner.makeMazeRunner (Maze.getStart maze)
    finished = False
  in
    (Model maze runner finished, Cmd.none)


view : Model -> Html.Html Msg
view model =
  if model.finished then
    Html.text "~congrats! you won the game~"
  else
    MazeView.view (mazeViewModel model)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyMsg keycode ->
      let
        updatedModel = case keycode of
          75 ->
            move MazeRunner.Up model
          74 ->
            move MazeRunner.Down model
          72 ->
            move MazeRunner.Left model
          76 ->
            move MazeRunner.Right model
          _ ->
            model
      in
        (updatedModel, Cmd.none)


move : MazeRunner.Direction -> Model -> Model
move dir model =
  let
    maze = model.maze
    runner = model.runner
    (row, col) = (MazeRunner.getLoc runner)
    nextTile = case dir of
      MazeRunner.Up ->
        Maze.tileAt maze (row - 1, col)
      MazeRunner.Down ->
        Maze.tileAt maze (row + 1, col)
      MazeRunner.Left ->
        Maze.tileAt maze (row, col - 1)
      MazeRunner.Right ->
        Maze.tileAt maze (row, col + 1)
  in
    if not model.finished then
      case nextTile of
        Maze.Wall ->
          model
        Maze.End ->
          {model | finished = True}
        _ ->
          {model | runner = MazeRunner.move dir runner}
    else
      model


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs KeyMsg


mazeViewModel : Model -> MazeView.Model
mazeViewModel model =
  { maze = model.maze
  , runner = model.runner
  }
