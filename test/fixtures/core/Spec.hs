module Spec where

{-# RULES "mymap/mymap" forall f g xs. mymap f (mymap g xs) = mymap (f . g) xs #-}

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x : mymap f xs
{-# NOINLINE [1] mymap #-}

class Shape a where
  area :: a -> Double

newtype Sq = Sq Double

instance Shape Sq where
  area (Sq s) = s * s

total :: Shape a => [a] -> Double
total = sum . map area
{-# SPECIALISE total :: [Sq] -> Double #-}

useMap :: [Int] -> [Int]
useMap xs = mymap (+ 1) (mymap (* 2) xs)

fused :: Int -> Int
fused n = sum (map (* 2) [1 .. n])
