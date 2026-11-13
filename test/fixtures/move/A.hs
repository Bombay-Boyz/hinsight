module A (keep, moved) where

import Data.Char (toUpper)
import Data.List (sortOn)
import qualified Data.Map as M

keep :: Int -> Int
keep = succ

-- | Pairs each number with its place in descending order.
moved :: [Int] -> M.Map Int Int
moved xs = M.fromList (zip (sortOn negate xs) xs)

shout :: String -> String
shout = map toUpper
