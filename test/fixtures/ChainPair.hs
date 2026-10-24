module ChainPair where

pair :: a -> a -> [a]
pair x y = [x, y]

r :: [Int]
r = pair (1 :: Int) "x"
