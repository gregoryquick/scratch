-- {-# LANGUAGE MultiWayIf #-}
module Structure where

import Data.List
import Data.Ord

--Continous world structure
data World = World Double Double deriving Show
getX :: World -> Double
getX (World x _) = x
getP :: World -> Double
getP (World _ p) = p

-- Quantized state version of the world
data QWorld = QWorld Int Int deriving Show
getXq :: QWorld -> Int
getXq (QWorld x _) = x
getPq :: QWorld -> Int
getPq (QWorld _ p) = p

-- Quantization function
quantums = World (1/10) (1/10)

quantize :: World -> QWorld
quantize world = QWorld (floor((getX(world)/getX(quantums)))) (floor((getP(world)/getP(quantums))))

--Dequantization function as quantization is just for behind the scenes stuff
inject :: QWorld -> World
inject qworld = World (fromIntegral(getXq(qworld))*getX(quantums)) (fromIntegral(getPq(qworld))*getP(quantums))

--Function to get quantized version of world at a given time.
getQWorld :: Double -> QWorld
getQWorld = \x -> (quantize . getWorld) x

--Function to get the world at a given time.
getWorld :: Double -> World
getWorld t = World ((1/2)*cos((1/20)*t*2*pi)) (0.0)

testFunction = \t -> inject $ quantize $ getWorld t

--This in general, with curl and divergence will have a different type signature, but here where there is just time
--derivitive and no spacial dependence I can be lazy and fake it should, should do it properly at some point
springConstant = (pi^2)/100
objectMass = 1.0

-- qfindWUI :: World -> [QWorld]
-- qfindWUI world = fmap (\x -> snd x) loop
--   where
--     qworld = quantize world
--     loop = (0.0, qworld) : (fmap (findChange . snd) loop)

findWorldUpdateInfo :: World -> [(Double, World)]
findWorldUpdateInfo world = fmap (\x -> (abs $ fst x, inject $ snd x)) loop
  where
    qworld = quantize world
    loop = (0.0, qworld) : (fmap (findChange . snd) loop)

derivitive :: World -> World
derivitive (World x p) = World (p/objectMass) (-1.0*springConstant*x)

findChange :: QWorld -> (Double, QWorld)
findChange state = (abs $ fst changeInfo, uncurry (changeQ' state) $ convertHelper $ changeInfo)
 where
  changeInfo = findChangeTime state

findChangeTime :: QWorld -> (Double, Int)
findChangeTime state = (timeTillChange !! (snd minimumTime),snd minimumTime)
  where
    properties = [getX, getP]
    cState = inject state
    distance = fmap ($ quantums) properties
    speed = fmap ($ (derivitive cState)) properties
    timeTillChange = fmap (uncurry (/)) $ zip distance speed
    time = fmap (abs) timeTillChange
    minimumTime = minimumBy (comparing fst) (zip time [0..])

convertHelper :: (Double, Int) -> (Maybe Bool, Int)
convertHelper x
 | fst x == 0 = (Nothing, snd x)
 | fst x > 0 = (Just True, snd x)
 | fst x < 0 = (Just False, snd x)

changeQ :: QWorld -> Maybe Bool -> Int -> Maybe QWorld
changeQ _ Nothing _ = Nothing
changeQ (QWorld a b) (Just True) c
  | c == 0 = Just $ QWorld (a+1) b
  | c == 1 = Just $ QWorld a (b+1)
changeQ (QWorld a b) (Just False) c
  | c == 0 = Just $ QWorld (a-1) b
  | c == 1 = Just $ QWorld a (b-1)

changeQ' :: QWorld -> Maybe Bool -> Int -> QWorld
changeQ' a b c = helper $ changeQ a b c
  where
    helper Nothing = a
    helper (Just x) = x
--TODO Make it so that getWorld is deqantization of getQWorld instead of getQWorld being quantization of getWorld
