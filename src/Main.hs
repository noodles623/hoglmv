{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import SDL.Time               (time)
import SDL                    (glSwapWindow)
import System.Environment     (getArgs)

import Input
import Common
import Mesh

runCube :: [String] -> IO ()
runCube (ws:hs:mod:texs) =
  withSDLVideo $ withGLWindow "Mesh Viewer" (read ws :: Int, read hs :: Int) $
  \w -> do
    draw <- setup mod texs (read ws :: Float) (read hs :: Float)
    let loop ot acc r = do
          ct <- time
          let dt = ct - ot
          acc' <- frameCount (acc + dt) $
                  do render draw r
                     SDL.glSwapWindow w
          checkInput >>=
            flip when (animate r ct >>= loop ct (acc'+dt))
    time >>= \t -> loop t 0.0 $ startState (read ws :: Int, read hs :: Int) t

main :: IO ()
main = getArgs >>= runCube