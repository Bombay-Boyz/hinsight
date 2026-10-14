{-# LANGUAGE TemplateHaskell #-}
module UsesTh (count) where

import Gen (greeting)

count :: Int
count = $(greeting)
