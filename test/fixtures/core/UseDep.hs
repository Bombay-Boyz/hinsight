module UseDep where

import Dep

twice :: Int -> Int
twice n = dep n + dep (n + 1)
