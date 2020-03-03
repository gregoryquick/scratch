-- {-# LANGUAGE MultiWayIf #-}
module Structure where

--Continous world structure
data World = World Double Double
getX :: World -> Double
getX (World x _) = x
getP :: World -> Double
getP (World _ p) = p

-- Quantized state version of the world
data QWorld = QWorld Int Int
getXq :: QWorld -> Int
getXq (QWorld x _) = x
getPq :: QWorld -> Int
getPq (QWorld _ p) = p

-- Quantization function
scaleFactor :: Double
scaleFactor = 1/100

quantize :: World -> QWorld
quantize world = QWorld (floor(scaleFactor*(getX(world)))) (floor(scaleFactor*(getP(world))))

--Dequantization function as quantization is just for behind the scenes stuff
inject :: QWorld -> World
inject qworld = World (fromIntegral(getXq(qworld))) (fromIntegral(getPq(qworld)))

--Function to get quantized version of world at a given time.
getQWorld :: Double -> QWorld
getQWorld = \x -> (quantize . getWorld) x

--Function to get the world at a given time.
getWorld :: Double -> World
getWorld t = World ((1/2)*cos((1/20)*t*2*pi)) (0.0)

--TODO Make it so that getWorld is deqantization of getQWorld instead of getQWorld being quantization of getWorld
