module MissedDep (twiceOf) where

twiceOf :: Num a => a -> a
twiceOf x = x + x
{-# NOINLINE twiceOf #-}
