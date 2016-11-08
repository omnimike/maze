{-# LANGUAGE OverloadedStrings #-}

module MazeGen
    ( Maze
    , MazeSize
    , Seed
    , serialize
    , generate
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified System.Random as Random
import qualified Debug.Trace as Trace

type Seed =
    Random.StdGen

type MazeSize =
    (Int, Int)

type RoomPos =
    (Int, Int)

type TileMap =
    Map.Map TilePos Tile

type TilePos =
    (Int, Int)

type TileMapSize =
    (Int, Int)

data Tile
    = Wall
    | Space
    deriving (Show)

-- up, right, down, left
type RoomBoundries = (Tile, Tile, Tile, Tile)

data Maze
    = Maze
        { getTiles :: TileMap
        , getSize :: MazeSize
        , getStart :: RoomPos
        , getEnd :: RoomPos
        }
    deriving (Show)

tileAt :: TileMap -> TilePos -> Tile
tileAt tileMap loc =
    Map.findWithDefault Wall loc tileMap

isSpace :: Tile -> Bool
isSpace Space = True
isSpace _ = False

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

mazeSizeToTileMapSize :: MazeSize -> TileMapSize
mazeSizeToTileMapSize (mazeHeight, mazeWidth) =
    (2*mazeHeight + 1, 2*mazeWidth + 1)

roomPosToTilePos :: RoomPos -> TilePos
roomPosToTilePos (roomRow, roomCol) =
    (2*roomRow + 1, 2*roomCol + 1)

serialize :: Maze -> Text.Text
serialize maze =
    Text.pack (unlines [[charAt (row, col) | col <- colNums] | row <- rowNums])
    where
        (tileMapHeight, tileMapWidth) = mazeSizeToTileMapSize (getSize maze)
        rowNums = [0 .. (tileMapHeight - 1)]
        colNums = [0 .. (tileMapWidth - 1)]
        charAt loc
            | loc == roomPosToTilePos (getStart maze) =
                'S'
            | loc == roomPosToTilePos (getEnd maze) =
                'E'
            | otherwise =
                case tileAt (getTiles maze) loc of
                    Wall ->
                        '#'
                    Space ->
                        '.'

generate :: Seed -> MazeSize -> Maze
generate seed (mazeHeight, mazeWidth) =
    Maze tileMap (mazeHeight, mazeWidth) start end
    where
        (startRow, seed') = Random.randomR (0, mazeHeight - 1) seed
        (startCol, seed'') = Random.randomR (0, mazeWidth - 1) seed'
        start = (startRow, startCol)
        protoMap = protoTileMap (mazeHeight, mazeWidth)
        (tileMap, end) = step seed'' protoMap [start] 1 1 start
        step seed tileMap path pathLength longestLength end =
            if null path then
                (tileMap, end)
            else
                let
                    currentRoom : previousRooms = path
                    potentialNextRooms = availableRooms tileMap currentRoom
                in
                    if null potentialNextRooms then
                        step seed tileMap previousRooms (pathLength - 1) longestLength end
                    else
                        let
                            (nextRoom, seed') = choose seed potentialNextRooms
                            path' = nextRoom : path
                            pathLength' = pathLength + 1
                            newLongestPath = pathLength > longestLength
                            longestLength' =
                                if newLongestPath then
                                    pathLength
                                else
                                    longestLength
                            end' =
                                if (newLongestPath) then
                                    nextRoom
                                else
                                    end
                            wallBetween =
                                (wallRow, wallCol)
                                where
                                    (cRow, cCol) = roomPosToTilePos currentRoom
                                    (nRow, nCol) = roomPosToTilePos nextRoom
                                    wallRow = (cRow + nRow) `div` 2
                                    wallCol = (cCol + nCol) `div` 2
                            tileMap' = Map.update (\_ -> Just Space) wallBetween tileMap
                        in
                            step seed' tileMap' path' pathLength' longestLength' end'

choose seed list =
    (list !! idx, seed')
    where
        len = length list
        (idx, seed') = Random.randomR (0, len - 1) seed

availableRooms :: TileMap -> RoomPos -> [RoomPos]
availableRooms maze (roomRow, roomCol) =
    filter look adjacentRooms
    where
        adjacentRooms = adjacentSquares (roomRow, roomCol)
        look (r, c) =
            case tile of
                Wall ->
                    False
                Space ->
                   notVisited
            where
                tilePos = roomPosToTilePos (r, c)
                tile = tileAt maze tilePos
                adjacentTiles = map (tileAt maze) (adjacentSquares tilePos)
                notVisited = null (filter isSpace adjacentTiles)

adjacentSquares :: (Int, Int) -> [(Int, Int)]
adjacentSquares (row, col) =
    [up, right, down, left]
    where
        up = (row - 1, col)
        right = (row, col + 1)
        down = (row + 1, col)
        left = (row, col - 1)

protoTileMap :: MazeSize -> TileMap
protoTileMap (mazeHeight, mazeWidth) =
    make2DMap mapTile (mazeSizeToTileMapSize (mazeHeight, mazeWidth))
    where
        isEven n = n `rem` 2 == 0
        isOdd n = not (isEven n)
        mapTile (row, col) =
            if isOdd row && isOdd col then Space else Wall
        make2DMap func (height, width) =
            Map.fromList
                [ ((row, col), func (row, col))
                | row <- [0..(height-1)]
                , col <- [0..(width-1)]
                ]
