module C (useC) where

import A (count)
import B (useB)

useC :: Int
useC = useB + count 2
