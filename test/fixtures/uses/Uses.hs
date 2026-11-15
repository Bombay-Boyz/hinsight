module Uses where

shared :: Int
shared = 4

firstUse :: Int
firstUse = shared + shared

secondUse :: Int
secondUse = shared + 1

unused :: Int
unused = 9

hole :: Int
hole = _
