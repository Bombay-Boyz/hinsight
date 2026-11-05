module Extract (f, g) where

f :: Int -> Int -> Int
f k n = let m = n * 2 in (m + k) * (m + k)

g :: Int -> Int
g x = x + 1
