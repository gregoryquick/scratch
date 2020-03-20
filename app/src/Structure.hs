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
-- Length of string in meters
-- length = pi
-- unitMass = c*objectMass
-- tension = ((l^2)/(pi^2))*c*springConstant

-- Simple pendulum differential equation
-- g = -9.8
-- l = 1.0
-- derivitive :: World -> World
-- derivitive (World x p) = World (p) ((g/l)*sin((pi/180)*x))

--Integrator for now asumed to be of form y = y0 + h * z
timeStep :: Double
timeStep = 0.5

getList = [getX, getP]
setList = [setX, setP]

nodes :: Int
nodes = 2


--So that compile does not break with main.hs
worlds :: World -> [(Double,World)]
worlds w = [(0,w)]
