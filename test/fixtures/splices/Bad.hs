{-# LANGUAGE TemplateHaskell #-}
module Bad where

import Language.Haskell.TH

bad :: Int
bad = $(litE (stringL "x")) + 1
