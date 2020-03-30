{-# LANGUAGE TypeFamilies #-}
module Structure where

import Data.List
import Data.Ord
import Data.Semigroup
import Data.VectorSpace
--Model code

-- World structure
--World the currennt possition and momentum
data World = World Double Double
instance Show World where
  show (World a b) = "World (" ++ (show a) ++ ") (" ++ (show b) ++ ")"
instance AdditiveGroup World where
  zeroV = World (0.0) (0.0)
  (^+^) (World a0 b0) (World a1 b1) = World (a0 + a1) (b0 + b1)
  negateV (World a b) = World (-1.0*a) (-1.0*b)
instance VectorSpace World where
  type Scalar World = Double
  (*^) s (World a b) = World (s*a) (s*b)

--A type to avoid confusing World and the timer derivive of world
type World' = World

-- Hookian spring constannt
springConstant = (pi^2)/100
--Mas of object
objectMass = 1.0

--the intial configeration
startingWorld :: World
startingWorld = World (0.5) (0.0)

actualValue :: Double -> World
actualValue t = World (0.5*cos(sqrt(springConstant/objectMass)*t)) (-0.5*sqrt(springConstant/objectMass)*sin(sqrt(springConstant/objectMass)*t))

--And error that gives difference from actual value
errorFunction :: Double -> World -> World
errorFunction time testWorld = (actualValue time) ^+^ (negateV testWorld)

--We know that we have diffential equations:
-- x' = p/m
-- p' = -kx
-- aka dX/dt = f(X)

--So jacobian df/dX is
-- 0.0 , 1/m
-- -k  , 0.0
--aka
--[d_x(X') , d_p(X')]
jacobianAtPoint :: World -> World -> World
jacobianAtPoint (World x p) (World xVelIn pVelIn) = World ((0.0)*xVelIn + (1/objectMass)*pVelIn) ((-1.0*springConstant)*xVelIn + (0.0)*pVelIn)

--We know that the given jacobian J_x0 of f about stable point x0 means that the system of equations
--is well aproximated by X' = J_x0(X) about x0

-- But the starting point is not neccisary near a stable point :( so I cannot neccisary use this as an aproximation,
-- Plus if I were to try using this I would have to find stable points, which seams a bit of a pain.
-- Is there some other set of equations where I can have my point be a stable point
-- If I could construct some sort of "path flow" moves towards the integral curve then integral curves

-- All center manifolds about x0 are invarient under X' = J_x0(X)
-- This just means that all all path lines of this system flow towards the center mannifolds (or other invarient spaces)
-- This should mean that the flow of a path that aproximates the center manifold sufficently will under the flow of
--  X' = J_x0(X') converge towards the center manold

--Inverse jacobian needed to say, take quantization of output changes, and make it changes in input changes

--Solver code

--We have constant jacobian so we will only needed
jacobian :: World -> World
jacobian = jacobianAtPoint (World 0.0 0.0)

--Simple euler method for now
h = 1.0;
eulerMethod :: World -> [World]
eulerMethod w = loop
  where
    loop :: [World]
    loop = w : fmap computeNext loop
    computeNext :: World -> World
    computeNext world = (^+^) world $ (*^) h $ jacobian world
--So that compile does not break with main.hs
worlds :: World -> [(Double,World)]
worlds w = fmap (\x -> (0.0000001,x)) $ eulerMethod w

--Another model for testing
-- --World structure
-- data World = World (Double -> Double)
-- instance Semigroup World where
--   (<>) (World f) (World g) = World (\x -> (f x)+(g x))
--
-- -- Length of string in meters
-- -- stringLength = pi
-- -- -- Mass density in kg per meter
-- -- unitMass = 1
-- -- -- Tension in newton
-- -- stringTension = (pi^2)/100
--
-- --the intial configeration
-- startingWorld :: World
-- startingWorld = World (\x -> 0.0)
