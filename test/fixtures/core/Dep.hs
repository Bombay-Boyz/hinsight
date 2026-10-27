module Dep (dep) where

dep :: Int -> Int
dep n = length (filter even (map (+ 1) [1 .. n]))
