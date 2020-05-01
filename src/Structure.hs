{-# LANGUAGE TypeFamilies #-}
module Structure where

import Data.List
import Data.Ord
import Data.Semigroup
import Data.VectorSpace
import Data.Fixed
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
instance InnerSpace World where
  (<.>) (World a0 b0) (World a1 b1) = (a0*a1)+(b0*b1)
--Probably should look at chaning this to the Affine space type

--A type to avoid confusing World and the derivives of world
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
timeDifferential :: World -> World'
timeDifferential (World x p) = World (p/objectMass) (-1.0*springConstant*x)

--So jacobian df/dX is
-- 0.0 , 1/m
-- -k  , 0.0
--aka
--[d_x(X') , d_p(X')]
jacobianAtPoint :: World -> World' -> World'
jacobianAtPoint (World x p) (World xVelIn pVelIn) = World (pVelIn/objectMass) (-1.0*springConstant*xVelIn)
--This only looks like the system equation becouse the system equation is linear.

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

-- Linear multistep

nodes :: Int
nodes = 2
h :: Double
h = 0.1

integrator :: World -> [World]
integrator w = loop
  where
    loop :: [World]
    loop = w : ramp w : fmap computeNext historisisTerms

    ramp :: World -> World
    ramp world = (^+^) world $ (*^) h $ timeDifferential world

    historisisTerms :: [[World]]
    historisisTerms = transpose $ reverse $ take nodes $ iterate (tail) loop

    computeNext :: [(World)] -> World
    computeNext past = (^+^) x0 $ d0 ^+^ d1
      where
        d0 = (*^) ((3/2)*h) $ timeDifferential x0
        d1 = (*^) ((-1/2)*h) $ timeDifferential x1
        x0 = past!!0
        x1 = past!!1

-- Fixed phase distance

-- phaseDistance :: Double
-- phaseDistance = 0.01
--
-- integrator :: World -> [World]
-- integrator w = loop
--   where
--     loop :: [World]
--     loop = w : fmap computeNext loop
--
--     computeNext :: World -> World
--     computeNext oldWorld = (^+^) oldWorld $ deltaVector
--       where
--         diffVector = timeDifferential oldWorld
--         normVector = normalized diffVector
--         deltaVector = phaseDistance *^ normVector
--         -- deltaMag = (deltaVector <.> deltaVector)
--         -- h = phaseDistance/deltaMag

worlds :: World -> [(Double,World)]
worlds w = fmap (\x -> (0.0000000001,x)) $ integrator w
-- worlds w = integrator w

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
