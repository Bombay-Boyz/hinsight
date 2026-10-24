{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} {-# HLINT ignore "Redundant bracket" #-}
module Use where

import Lib
import Language.Haskell.TH

a :: Int
a = $(litE (integerL 7))

b :: Int
b = $( [| 1 + $(litE (integerL 2)) |] )

c :: Int
c = $$(typedOne)

$(mkConst "k1" 10)

mkConst "k2" 20

$( do { ds <- mkConst "k3" 30; e <- [| $(litE (integerL 1)) |]; return ds } )

twiceUse :: Int
twiceUse = $(twice [| 3 |])

d :: $(tupleT2)
d = (1, True)

f :: Maybe Int -> Int
f $(justP) = 1
f _ = 0

s :: String
s = [str|hello|]

g :: Int
g = h where h = $(litE (integerL 9))
