module Main where

import Data.IORef
import Graphics.Rendering.Cairo
import Control.Monad.Reader
import FRP.Elerea.Simple

import Structure
import Rendering
import Rasterizer

main = do
  let param = Paramaters 1000 500
  let world = World (0.0) 0
  let iterations = 20
  stateList <- (test iterations $ stateful world (updateWorld))
  foldr1 (>>) $ fmap (sketchWith param) stateList

test :: Int -> SignalGen (Signal a) -> IO [a]
test i g = replicateM i =<< start g
