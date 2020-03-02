module Main where

import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Control.Wire
import FRP.Netwire
import Prelude hiding ((.), id)
import Control.Wire.Unsafe.Event
import Control.Monad.IO.Class

import Structure
import Rendering
import Rasterizer

param = Paramaters 1000 500

main = do
  run clockSession_ program

myTime :: (HasTime t s, Monad m) => Wire s () m a Double
myTime = integral 0 . pure 1

generateWorld :: (HasTime t s, Monad m) => Wire s () m a World
generateWorld = fmap getWorld myTime

generateRenderer :: (HasTime t s, Monad m) => Wire s () m a (IO ())
generateRenderer = fmap (sketchWith param) generateWorld

program :: (HasTime t s, Monad m, Show t) => Wire s () m () (Event (IO ()))
program = periodic 1 . generateRenderer



run :: (HasTime t s, MonadIO m) => Session m s -> Wire s e m () (Event (IO ())) -> m e
run = go
  where
    go session wire = do
      (dt, session') <- stepSession session
      (wt', wire') <- stepWire wire dt (Right ())
      case wt' of
        Left a -> return a
        Right bEvent -> do
          case bEvent of
            Event b -> liftIO $ b
            _ -> return ()
          go session' wire'
