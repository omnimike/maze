{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MazeGen
import qualified Web.Scotty as Scotty
import qualified Data.Text.Lazy as LText
import qualified System.Random as Random

main :: IO ()
main = do
    Scotty.scotty 8080 $
        Scotty.get "/" $ do
            height <- Scotty.param "h"
            width <- Scotty.param "w"
            seed <- Scotty.param "s"
            Scotty.text $ LText.fromStrict $
                MazeGen.serialize (MazeGen.generate (Random.mkStdGen seed) (height, width))
