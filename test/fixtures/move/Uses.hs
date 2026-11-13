module Uses (twice) where

double :: Int -> Int
double = (* 2)

twice :: Int -> Int
twice = double . double
