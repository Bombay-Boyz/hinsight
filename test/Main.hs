module Main (main) where

import HInsight.ChainGhcSpec qualified
import HInsight.ChainSpec qualified
import HInsight.CliSpec qualified
import HInsight.CoreGhcSpec qualified
import HInsight.CoreSpec qualified
import HInsight.EditSpec qualified
import HInsight.EnvironmentSpec qualified
import HInsight.ErrorSpec qualified
import HInsight.EvidenceSpec qualified
import HInsight.ExplanationSpec qualified
import HInsight.FlowSpec qualified
import HInsight.GhcEnvironmentSpec qualified
import HInsight.GhcSessionSpec qualified
import HInsight.GraphSpec qualified
import HInsight.HoleSpec qualified
import HInsight.IdentifierSpec qualified
import HInsight.InstanceSpec qualified
import HInsight.ProjectSpec qualified
import HInsight.ReportSpec qualified
import HInsight.SourceSpec qualified
import HInsight.SpliceGhcSpec qualified
import HInsight.SpliceSpec qualified
import HInsight.SuggestionSpec qualified
import HInsight.TraceSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HInsight.SourceSpec.spec
  HInsight.SuggestionSpec.spec
  HInsight.TraceSpec.spec
  HInsight.ChainSpec.spec
  HInsight.SpliceSpec.spec
  HInsight.CoreSpec.spec
  HInsight.ExplanationSpec.spec
  HInsight.EnvironmentSpec.spec
  HInsight.FlowSpec.spec
  HInsight.GraphSpec.spec
  HInsight.HoleSpec.spec
  HInsight.EditSpec.spec
  HInsight.IdentifierSpec.spec
  HInsight.InstanceSpec.spec
  HInsight.ProjectSpec.spec
  HInsight.ErrorSpec.spec
  HInsight.ReportSpec.spec
  HInsight.CliSpec.spec
  HInsight.GhcSessionSpec.spec
  HInsight.GhcEnvironmentSpec.spec
  HInsight.EvidenceSpec.spec
  HInsight.ChainGhcSpec.spec
  HInsight.SpliceGhcSpec.spec
  HInsight.CoreGhcSpec.spec
