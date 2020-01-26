{-# LANGUAGE MultiWayIf #-}
module Main where

main = do
  print "Hello World"

data Manifold a = Manifold [a -> [Int]] (Int -> Int -> Maybe (a -> Maybe a))
data Point a = Point a Int (Manifold a)

-- Example implentation will be a circle

type OneManifold = Manifold Double
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

-- Next I need to construct the tangent structure
