import Html
import Html.App
import Result
import Maze
import MazeRunner
import MazeView
import Keyboard


type alias Model =
  { maze : Maze.Maze
  , runner : MazeRunner.MazeRunner
  }


type Msg
  = KeyMsg Keyboard.KeyCode


main =
  Html.App.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = mazeViewModel >> MazeView.view
  }


init : (Model, Cmd Msg)
init =
  let
    maze = Result.withDefault Maze.emptyMaze Maze.sampleMaze
    runner = MazeRunner.makeMazeRunner (Maze.getStart maze)
  in
    (Model maze runner, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg {maze, runner} =
  case msg of
    KeyMsg keycode ->
      let
        updatedRunner = case keycode of
          75 ->
            MazeRunner.moveUp runner
          74 ->
            MazeRunner.moveDown runner
          72 ->
            MazeRunner.moveLeft runner
          76 ->
            MazeRunner.moveRight runner
          _ ->
            runner
      in
        ({maze = maze, runner = updatedRunner}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Keyboard.downs KeyMsg


mazeViewModel : Model -> MazeView.Model
mazeViewModel model = model
