module Common where

import qualified SDL

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)
import SDL                    (($=))
import Graphics.GL as R
import If

withSDLVideo :: (MonadIO m) => m a -> m ()
withSDLVideo op = do
  SDL.initialize [SDL.InitVideo]
  void op 
  SDL.quit

withGLWindow :: (MonadIO m) => Text -> (Int,Int) -> (SDL.Window -> m a) -> m ()
withGLWindow title (x,y) op = do
  w <- SDL.createWindow title $
       SDL.defaultWindow { SDL.windowInitialSize =
                             SDL.V2 (fromIntegral x) (fromIntegral y)
                         , SDL.windowGraphicsContext =
                             SDL.OpenGLContext SDL.defaultOpenGL }
  SDL.glCreateContext w
  SDL.swapInterval $= SDL.SynchronizedUpdates
--  R.glEnable GL_CULL_FACE
  R.glEnable GL_DEPTH_TEST
  void $ op w
  SDL.destroyWindow w


frameCount :: Float -> IO () -> IO Float
frameCount acc op = 
  if (acc > 1.0/62.0) then do
    let acc' = acc - 1.0/60.0
    op
    return $ acc' < 0 ? 0 $ acc'
  else return acc


