module Main (main) where

import qualified HInsight.ErrorSpec
import qualified HInsight.ExplanationSpec
import qualified HInsight.GhcSessionSpec
import qualified HInsight.HoleSpec
import qualified HInsight.SourceSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.SourceSpec.spec
  HInsight.ExplanationSpec.spec
  HInsight.HoleSpec.spec
  HInsight.ErrorSpec.spec
  HInsight.GhcSessionSpec.spec
