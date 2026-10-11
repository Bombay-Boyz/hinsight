module Main (main) where

import HInsight.CliSpec qualified
import HInsight.ErrorSpec qualified
import HInsight.ExplanationSpec qualified
import HInsight.FlowSpec qualified
import HInsight.GhcSessionSpec qualified
import HInsight.GraphSpec qualified
import HInsight.HoleSpec qualified
import HInsight.IdentifierSpec qualified
import HInsight.ProjectSpec qualified
import HInsight.ReportSpec qualified
import HInsight.SourceSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.SourceSpec.spec
  HInsight.ExplanationSpec.spec
  HInsight.FlowSpec.spec
  HInsight.GraphSpec.spec
  HInsight.HoleSpec.spec
  HInsight.IdentifierSpec.spec
  HInsight.ProjectSpec.spec
  HInsight.ErrorSpec.spec
  HInsight.ReportSpec.spec
  HInsight.CliSpec.spec
  HInsight.GhcSessionSpec.spec
