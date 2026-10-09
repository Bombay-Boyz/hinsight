module Main (main) where

import HInsight.ErrorSpec qualified
import HInsight.ExplanationSpec qualified
import HInsight.GhcSessionSpec qualified
import HInsight.HoleSpec qualified
import HInsight.SourceSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.SourceSpec.spec
  HInsight.ExplanationSpec.spec
  HInsight.HoleSpec.spec
  HInsight.ErrorSpec.spec
  HInsight.GhcSessionSpec.spec
