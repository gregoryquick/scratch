-- {-# LANGUAGE MultiWayIf #-}
module Structure where

import Data.List
import Data.Ord
import Data.Semigroup

--Continous world structure
data World = World Double Double deriving Show
instance Semigroup World where
  (<>) (World x0 p0) (World x1 p1) = World (x0+x1) (p0+p1)
getX :: World -> Double
getX (World x _) = x
setX :: World -> Double -> World
setX (World _ p) x = World x p
getP :: World -> Double
getP (World _ p) = p
setP :: World -> Double -> World
setP (World x _) p = World x p
multiply :: Double -> World -> World
multiply s (World x p) = World (s*x) (s*p)
-- Quantized state version of the world
data QWorld = QWorld Int Int deriving Show
instance Semigroup QWorld where
  (<>) (QWorld x0 p0) (QWorld x1 p1) = QWorld (x0+x1) (p0+p1)
getXq :: QWorld -> Int
getXq (QWorld x _) = x
setXq :: QWorld -> Int -> QWorld
setXq (QWorld _ p) x = QWorld x p
getPq :: QWorld -> Int
getPq (QWorld _ p) = p
setPq :: QWorld -> Int -> QWorld
setPq (QWorld x _) p = QWorld x p


-- Quantization function
quantums = World (1/100) (1/100)

quantize :: World -> QWorld
quantize world = QWorld (floor((getX(world)/getX(quantums)))) (floor((getP(world)/getP(quantums))))

--Dequantization function as quantization is just for behind the scenes stuff
inject :: QWorld -> World
inject qworld = World (fromIntegral(getXq(qworld))*getX(quantums)) (fromIntegral(getPq(qworld))*getP(quantums))

--This in general, with curl and divergence will have a different type signature, but here where there is just time
--derivitive and no spacial dependence I can be lazy and fake it should, should do it properly at some point
springConstant = (pi^2)/100
objectMass = 1.0

derivitive :: World -> World
derivitive (World x p) = World (p/objectMass) (-1.0*springConstant*x)

--Integrator for now asumed to be of form y = y0 + h * z
timeStep :: Double
timeStep = 0.5

getList = [getX, getP]
setList = [setX, setP]

worlds :: World -> [(Double,World)]
worlds x0 = zip (tail timeDeltas) worldValues
  where
    timeDeltas = fmap (fst) loop
    worldValues = fmap (snd) loop
    startingWorld = x0
    loop :: [(Double,World)]
    loop = (0.0, startingWorld) : fmap calc historisisTerms
    historisisTerms :: [[World]]
    historisisTerms = transpose [fmap (snd) loop]

calc :: [World] -> (Double, World)
calc past = (abs timeDelta,setter x0 newData)
  where
    x0 = past !! 0
    zTerm = zCalc past
    -- newWorld = (<>) x0 $ multiply timeStep $ zCalc past
    deltaInfo = findChangeInfo zTerm
    timeDelta = fst deltaInfo
    index = snd deltaInfo
    getter = getList !! index
    setter = setList !! index
    oldData = getter x0
    changeofData :: Double -> Double
    changeofData t
      | t >= 0 = getter quantums
      | otherwise = (-1.0) * getter quantums
    newData = oldData + (changeofData timeDelta)


zCalc :: [World] -> World
zCalc past = x0'
  where
    x0 = past !! 0
    x0' = derivitive x0

findChangeInfo :: World -> (Double,Int)
findChangeInfo zTerm = minimumBy (comparing (abs . fst)) (zip timeList [0..])
  where
    derivitiveList = fmap ($ zTerm) getList
    quantumsList = fmap ($ quantums) getList
    timeList = fmap (uncurry (/)) $ zip quantumsList derivitiveList
