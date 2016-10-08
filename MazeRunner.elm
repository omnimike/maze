module MazeRunner exposing
  ( MazeRunner
  , Direction(..)
  , makeMazeRunner
  , getRow
  , getCol
  , move
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  )


import Maze


type MazeRunner
  = BasicMazeRunner Maze.Location


type Direction
  = Up
  | Down
  | Left
  | Right


makeMazeRunner : Maze.Location -> MazeRunner
makeMazeRunner loc =
  BasicMazeRunner loc


getRow : MazeRunner -> Int
getRow (BasicMazeRunner (row, col)) =
  row


getCol : MazeRunner -> Int
getCol (BasicMazeRunner (row, col)) =
  col


move : Direction -> MazeRunner -> MazeRunner
move dir (BasicMazeRunner (row, col)) =
  case dir of
    Up ->
      BasicMazeRunner (row - 1, col)
    Down ->
      BasicMazeRunner (row + 1, col)
    Left ->
      BasicMazeRunner (row, col - 1)
    Right ->
      BasicMazeRunner (row, col + 1)


moveUp : MazeRunner -> MazeRunner
moveUp =
  move Up


moveDown : MazeRunner -> MazeRunner
moveDown =
  move Down


moveLeft : MazeRunner -> MazeRunner
moveLeft =
  move Left


moveRight : MazeRunner -> MazeRunner
moveRight =
  move Right
