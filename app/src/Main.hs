module Main where

import Data.IORef
import Graphics.Rendering.Cairo
import Control.Monad.Reader

import Structure
import Rendering
import Rasterizer

main = do
  -- globalState <- newIORef $ Point (1.0) 0 circle
  -- print $ eval circleMetric x (getTangent $ clockFlow x) 1.0
  let param = Paramaters 1000 500
  surface <- createImageSurfaceFromParam param
  renderWith surface $ runReader (sketch $  ocToColl . inCircle) param
  surfaceWriteToPNG surface "/data/out.png"
  -- print $ (runReader fromPixel param) 750 0
  -- print $ runReader makePixelList param
  where
    x = Point (1.0) 0 circle
