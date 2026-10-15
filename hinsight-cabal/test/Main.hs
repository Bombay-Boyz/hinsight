module Main (main) where

import HInsight.Cabal.CabalSpec qualified
import HInsight.Cabal.CaptureSpec qualified
import HInsight.Cabal.CliSpec qualified
import HInsight.Cabal.ProjectSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.Cabal.CaptureSpec.spec
  HInsight.Cabal.CliSpec.spec
  HInsight.Cabal.ProjectSpec.spec
  HInsight.Cabal.CabalSpec.spec
