-- |
-- Module      : HInsight.ChangeTypeGhcSpec
-- Description : GHC checks a changed signature, in the file and in the files that use it.
module HInsight.ChangeTypeGhcSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import HInsight (Refactor (..), mkLibDir)
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Ghc.Session (ghcRefactor)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.Verify (VerifiedPlan, planEdits, planNotes)
import HInsight.Source (mkSourceFile)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import HInsight.TypeText (mkTypeText)
import Test.Hspec

folder :: FilePath
folder = "test/fixtures/changetype/"

changedBy :: FilePath -> String -> String -> [FilePath] -> IO (Either InsightError VerifiedPlan)
changedBy libDir name wanted others = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile (folder <> "A.hs"))
  identifier <- ok (mkIdentifier (T.pack name))
  t <- ok (mkTypeText (T.pack wanted))
  watched <- traverse (\o -> ok (mkSourceFile (folder <> o <> ".hs"))) others
  changeType (ghcRefactor dir) file identifier t watched

changedText :: VerifiedPlan -> IO Text
changedText plan = do
  file <- ok (mkSourceFile (folder <> "A.hs"))
  text <- TIO.readFile (folder <> "A.hs")
  case applyEditSet (const (Just text)) (planEdits plan) of
    Left e -> fail (show e)
    Right m -> pure (M.findWithDefault text file m)

isVerificationFailure :: Either InsightError a -> Bool
isVerificationFailure = \case
  Left (RefactorFailure (VerificationFailed _)) -> True
  _ -> False

spec :: Spec
spec = describe "changeType, checked by GHC" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "changes a signature" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "refuses a name the module does not define" $
    changedBy libDir "nothere" "Int" [] `shouldReturn` Left (RefactorFailure (DefinitionNotFound "nothere"))
  it "refuses a signature that names other bindings too" $
    changedBy libDir "many" "Int" [] `shouldReturn` Left (RefactorFailure SignatureNamesOthers)
  it "refuses a type the binding itself does not fit, with GHC's message" $
    changedBy libDir "count" "Int -> String" [] >>= (`shouldSatisfy` isVerificationFailure)
  it "adds a signature to a binding that has none" $ do
    Right plan <- changedBy libDir "bare" "Int" []
    planNotes plan `shouldBe` []
    changedText plan >>= (`shouldSatisfy` T.isInfixOf "bare :: Int\nbare = 7\n")
  it "lists the places in other files that the new type breaks" $ do
    Right plan <- changedBy libDir "count" "Integer -> Integer" ["B", "C"]
    changedText plan >>= (`shouldSatisfy` T.isInfixOf "count :: Integer -> Integer\n")
    let notes = planNotes plan
    notes `shouldSatisfy` any (T.isInfixOf "B.hs")
    notes `shouldSatisfy` any (T.isInfixOf "C.hs")
    notes `shouldSatisfy` all (T.isPrefixOf "GHC reports a place still to fix: ")
