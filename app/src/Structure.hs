{-# LANGUAGE MultiWayIf #-}
module Structure where

data Manifold a = Manifold [a -> [Int]] (Int -> Int -> Maybe (a -> Maybe a))
data Point a = Point a Int (Manifold a)

-- Example implentation will be a circle
type OneManifold = Manifold Double
type OnePoint = Point Double
circle :: OneManifold
circle = Manifold cIL transitionStuff
  where
    cIL = [\x -> if (x == 0) then [] else [1],\x -> if (x == 0) then [] else [0]]
    transitionStuff x y =
      if | x == y && x < length cIL  -> Just $ \a -> Just a
         | x == 0 && y == 1          -> Just $ transition01
         | x == 1 && y == 0          -> Just $ transition10
         | otherwise                 -> Nothing
    transition01 x
      | x == 0 = Nothing
      | otherwise = Just $ 1/x
    transition10 x
      | x == 0 = Nothing
      | otherwise = Just $ 1/x

type OneTangent = Tangent Double
type OneMetric = Metric Double

clockFlow :: OnePoint -> OneTangent
clockFlow point = Tangent 1.0 point

circleMetric :: OneMetric
circleMetric = Metric $ \_ -> \a -> \b -> a * b

data Tangent a = Tangent a (Point a)
getTangent :: Tangent a -> a
getTangent (Tangent a x) = a

data Metric a = Metric ((Point a) -> (a -> a -> Double))
eval :: Metric a -> Point a -> a -> a -> Double
eval (Metric f) x = \a -> \b -> f x a b

-- Now a function on the circle
myFunct :: OnePoint -> Double
myFunct (Point x 0 circle) = acos((x^2-1)/(x^2+1))/pi
myFunct (Point x 1 circle) = acos(-(x^2-1)/(x^2+1))/pi

getAngle :: OnePoint -> Double
getAngle (Point x 0 circle) = atan((2*x)/(x^2-1))
