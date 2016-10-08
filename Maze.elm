module Maze exposing
  ( Maze
  , Tile(..)
  , ParseError
  , ParseResult
  , Dimensions
  , Location
  , toList
  , getDims
  , getStart
  , tileAt
  , emptyMaze
  , sampleMaze
  , parse
  )


import Array
import String
import List
import Result


type alias Location = (Int, Int)


type alias Dimensions = (Int, Int)


type Maze
  = Maze Dimensions (Array.Array Tile) Location


type Tile
  = Wall
  | Space
  | Start
  | End


type ParseError
  = InvalidChar Location Char
  | MultipleStartPoints Location
  | NoStartPoint


type alias ParseResult =
  Result ParseError Maze


toList : Maze -> List (List Tile)
toList (Maze (height, width) arr _) =
  let
    rowToList row =
      let
        startIndex = width * row
        endIndex = startIndex + width
        slice = Array.slice startIndex endIndex arr
      in
        Array.toList slice

    toList' row list =
      if row < 0 then
        list
      else
        toList' (row - 1) <| (rowToList row) :: list
  in
    toList' (height - 1) []


getDims : Maze -> Dimensions
getDims (Maze dims _ _) =
  dims


getStart : Maze -> Location
getStart (Maze _ _ start) =
  start


tileAt : Maze -> Location -> Tile
tileAt (Maze (height, width) arr _) (row, col) =
  let
    rowOffset = width * row
    tile = Array.get (rowOffset + col) arr
  in
    case tile of
      Just tile ->
        tile
      Nothing ->
        Wall


parse : String -> ParseResult
parse str =
  let
    charList = String.toList (String.trim str)

    findDims height width lineLength chars =
      case chars of
        [] ->
          (height, width)
        (c :: cs) ->
          case c of
            '\n' ->
              findDims (height + 1) (max width lineLength) 0 cs
            _ ->
              findDims height width (lineLength + 1) cs

    (height, width) = findDims 1 0 0 charList

    fillInLine currentWidth =
      List.repeat (width - currentWidth) Wall

    parseChars row col start reverseTiles chars =
      case chars of
        [] ->
          case start of
            Just startLoc ->
              Ok <| (List.reverse (fillInLine col ++ reverseTiles), startLoc)
            Nothing ->
              Err <| NoStartPoint
        (c :: cs) ->
          case c of
            ' ' ->
              parseChars row (col + 1) start (Wall :: reverseTiles) cs
            '#' ->
              parseChars row (col + 1) start (Wall :: reverseTiles) cs
            '.' ->
              parseChars row (col + 1) start (Space :: reverseTiles) cs
            'S' ->
              case start of
                Nothing ->
                  parseChars row (col + 1) (Just (row, col)) (Start :: reverseTiles) cs
                Just _ ->
                  Err <| MultipleStartPoints (row, col)
            'E' ->
              parseChars row (col + 1) start (End :: reverseTiles) cs
            '\n' ->
              parseChars (row + 1) 0 start (fillInLine col ++ reverseTiles) cs
            _ ->
              Err <| InvalidChar (row, col) c

    resultList = parseChars 0 0 Nothing [] charList

  in
    case resultList of
      Ok (tileList, start) ->
        Ok <| Maze (height, width) (Array.fromList tileList) start
      Err err ->
        Err err


emptyMaze : Maze
emptyMaze =
  Maze (1, 2) (Array.fromList [Start, End]) (0, 0)


sampleMaze : ParseResult
sampleMaze =
  parse """
###########################################
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#................S...E....................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
#.........................................#
###########################################
"""
