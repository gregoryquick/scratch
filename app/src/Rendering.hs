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

blankScreen :: Generate (Render ())
blankScreen = do
  (Paramaters w h) <- ask
  return $ do
    setSourceRGBA 0 0 0 1
    rectangle 0 0 (fromIntegral w) (fromIntegral h)
    fill

createNewBlank :: Paramaters -> IO ()
createNewBlank param  = do
  surface <- createImageSurfaceFromParam param
  renderWith surface $ runReader (blankScreen) param
  surfaceWriteToPNG surface $ (++) "/data/" $ "out" ++ ".png"

getAspectRatio :: Int -> Int -> Double
getAspectRatio h v = (fromIntegral h) / (fromIntegral v)

inCircle :: World -> (Double, Double) -> RGBAColor
inCircle (World x0 y0) (x,y)
  | ((x-x0)^2)+((y-y0)^2) <= (1/24)^2 = RGBAColor 1 0 0 1
  | otherwise = RGBAColor 0 0 0 0

setColour :: RGBAColor -> Render ()
setColour (RGBAColor r g b a) = setSourceRGBA r g b a
