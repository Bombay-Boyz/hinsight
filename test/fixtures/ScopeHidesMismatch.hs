module ScopeHidesMismatch (f, g) where

f :: Int
f = missingName

g :: Int
g = "text"
