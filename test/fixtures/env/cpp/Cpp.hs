{-# LANGUAGE CPP #-}
module Cpp (v) where

v :: Int
#ifdef BROKEN
v = "text"
#else
v = 1
#endif
