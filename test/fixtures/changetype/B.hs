module B (useB) where

import A (count)

useB :: Int
useB = count 1 + 1
