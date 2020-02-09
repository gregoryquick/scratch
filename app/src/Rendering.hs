{-# LANGUAGE MultiWayIf #-}
module Rendering where

import Graphics.Rendering.Cairo
import Control.Monad.Reader

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

-- getAspectRatio :: Generate (Double)
-- getAspectRatio = do
--   (Paramaters h v) <- ask
--   return $ (fromIntegral h) / (fromIntegral v)
getAspectRatio :: Int -> Int -> Double
getAspectRatio h v = (fromIntegral h) / (fromIntegral v)
-- h : v

-- fillBackround :: Generate (Render ())
-- fillBackround = do
--   (Paramaters h v) <- ask
--   return $ do
--     setSourceRGBA 0 0 0 1
--     rectangle 0 0 (fromIntegral h) (fromIntegral v)
--     fill

-- sketch :: Generate (Render ())
-- sketch = do
--   renderSequence <- sequence [fillBackround]
--   return $ foldr1 (>>) renderSequence

inCircle :: (Double, Double) -> Bool
inCircle (x,y)
  | (x^2)+(y^2) <= (1/2)^2 = True
  | otherwise = False

ocToColl :: Bool -> RGBAColor
ocToColl False = RGBAColor 0 0 0 1
ocToColl True = RGBAColor 1 1 1 1

setColour :: RGBAColor -> Render ()
setColour (RGBAColor r g b a) = setSourceRGBA r g b a
