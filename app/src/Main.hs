module Main where

import Data.IORef
import Graphics.Rendering.Cairo
import Control.Monad.Reader

import Structure
import Rendering
import Rasterizer

main = do
  let param = Paramaters 1000 500
  let world = World (pi/2)
  surface <- createImageSurfaceFromParam param
  renderWith surface $ runReader (sketch world inCircle) param
  surfaceWriteToPNG surface "/data/out.png"
