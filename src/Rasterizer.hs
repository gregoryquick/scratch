module Rasterizer where

import Graphics.Rendering.Cairo
import Control.Monad.Reader

import Rendering
import Structure

makePixelList :: Generate [(Int,Int)]
makePixelList = do
  (Paramaters h v) <- ask
  let h' = fromIntegral(h)
  return $ take (h*v) $ map (\x-> (floor x `mod` h ,floor $ x / h')) [0.0..]

fromPixel :: Generate (Int -> Int -> (Double,Double))
fromPixel = do
  (Paramaters h v) <- ask
  let aspect = getAspectRatio h v
  let h' = fromIntegral(h)/2
  let v' = fromIntegral(v)/2
  return $ \x -> \y -> (aspect*(fromIntegral(x)-h')/h', -(fromIntegral(y)-v')/v')

fillPixel :: (Int, Int) -> ((Double,Double) -> RGBAColor) -> Generate (Render ())
fillPixel (x,y) f = do
  (Paramaters h v) <- ask
  q <- ask
  return $ do
    let coord = (runReader fromPixel q) x y
    setColour $ f coord
    rectangle (fromIntegral x) (fromIntegral y) 1 1
    fill

sketch :: World -> (World -> (Double,Double) -> RGBAColor) -> Generate (Render ())
sketch world f = do
  param <- ask
  renderSequence <- sequence (fmap ($ (f world)) (fmap fillPixel $ runReader makePixelList param))
  return $ foldr1 (>>) renderSequence

sketchWith :: Paramaters -> World -> IO ()
sketchWith param world = do
  -- surface <- createImageSurfaceFromParam param
  surface <- imageSurfaceCreateFromPNG $ (++) "/data/" $ "out" ++ ".png"
  renderWith surface $ runReader (sketch world dot) param
  surfaceWriteToPNG surface $ (++) "/data/" $ "out" ++ ".png"
