module MazeView exposing
  ( Model
  , view
  )


import Html
import String
import Maze
import MazeRunner


type alias Model =
  { maze : Maze.Maze
  , runner : MazeRunner.MazeRunner
  }


type DisplayTile
  = MazeTile Maze.Tile
  | RunnerTile


view : Model -> Html.Html msg
view model =
  Html.pre [] [
    Html.text <| formatMaze model
  ]


formatMaze : Model -> String
formatMaze {maze, runner} =
  let
    addRunnerTile mazeList runner =
      listSet2d (\_ -> RunnerTile) (MazeRunner.getRow runner, MazeRunner.getCol runner) mazeList
    tileList = addRunnerTile (listMap2d MazeTile (Maze.toList maze)) runner
    formatRow list =
      String.fromList <| List.map tileToChar list
  in
    String.join "\n" (List.map formatRow tileList)


tileToChar : DisplayTile -> Char
tileToChar tile =
  case tile of
    MazeTile Maze.Wall ->
      '#'
    MazeTile Maze.Space ->
      '.'
    MazeTile Maze.Start ->
      'S'
    MazeTile Maze.End ->
      'E'
    RunnerTile ->
      '@'


listMap2d : (a -> b) -> List (List a) -> List (List b)
listMap2d func list =
  List.map (\subList -> List.map func subList) list

listSet2d : (a -> a) -> (Int, Int) -> List (List a) -> List (List a)
listSet2d val (y, x) list =
  listSet (\row -> listSet val x row) y list


listSet : (a -> a) -> Int -> List a -> List a
listSet func idx list =
  let
    listSet' i searched remaining =
      case remaining of
        [] ->
          List.reverse searched
        (x :: xs) ->
          if i == idx then
            List.reverse searched ++ (func x :: xs)
          else
            listSet' (i + 1) (x :: searched) xs
  in
    listSet' 0 [] list
