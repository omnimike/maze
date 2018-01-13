{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MazeGen
import qualified Web.Scotty as Scotty
import qualified Data.Text.Lazy as LText
import qualified System.Random as Random
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    Scotty.scotty 8080 $
        Scotty.get "/" $ do
            height <- Scotty.param "h"
            width <- Scotty.param "w"
            -- seed <- Scotty.param "s"
            seed <- liftIO Random.getStdGen
            Scotty.text $ LText.fromStrict $
                MazeGen.serialize (MazeGen.generate seed (height, width))
