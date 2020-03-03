{-# LANGUAGE MultiWayIf #-}
module Rendering where

import Graphics.Rendering.Cairo
import Control.Monad.Reader

import Structure

data Paramaters = Paramaters {
  horizontal :: Int,
  vertical :: Int
}

data RGBAColor = RGBAColor {
  r :: Double,
  g :: Double,
  b :: Double,
  a :: Double
}

createImageSurfaceFromParam (Paramaters h v) = createImageSurface FormatARGB32 h v

type Generate a = Reader Paramaters a

getAspectRatio :: Int -> Int -> Double
getAspectRatio h v = (fromIntegral h) / (fromIntegral v)

inCircle :: World -> (Double, Double) -> RGBAColor
inCircle world (x,y)
  | ((x-getX(world))^2)+((y-0)^2) <= (1/8)^2 = RGBAColor 1 1 1 1
  | otherwise = RGBAColor 0 0 0 1

setColour :: RGBAColor -> Render ()
setColour (RGBAColor r g b a) = setSourceRGBA r g b a
