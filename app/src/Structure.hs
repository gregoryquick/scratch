-- {-# LANGUAGE MultiWayIf #-}
module Structure where

import Data.List
import Data.Ord
import Data.Semigroup

--Continous world structure
data World = World (Double -> Double)
instance Semigroup World where
  (<>) (World f) (World g) = World (\x -> (f x)+(g x))
-- Length of string in meters
-- length = pi
-- unitMass = c*objectMass
-- tension = ((l^2)/(pi^2))*c*springConstant

startingWorld :: World
startingWorld = World (\x -> 0.0)

--Integrator for now asumed to be of form y = y0 + h * z
timeStep :: Double
timeStep = 0.5

nodes :: Int
nodes = 2


--So that compile does not break with main.hs
worlds :: World -> [(Double,World)]
worlds w = [(0,w)]
