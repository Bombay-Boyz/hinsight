module Pragma (inlined) where

{-# INLINE inlined #-}
inlined :: Int -> Int
inlined = succ
