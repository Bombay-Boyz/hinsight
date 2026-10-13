module Main (main) where

import HInsight.Tadka.AdapterSpec qualified
import HInsight.Tadka.CliSpec qualified
import HInsight.Tadka.LocateSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.Tadka.LocateSpec.spec
  HInsight.Tadka.AdapterSpec.spec
  HInsight.Tadka.CliSpec.spec
