-- |
-- Module      : HInsight.MoveGhcSpec
-- Description : GHC's parsed and renamed programs supply the facts of a move.
module HInsight.MoveGhcSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import HInsight (Refactor (..), mkLibDir)
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Ghc.Session (ghcMoveFacts, ghcRefactor)
import HInsight.Graph (mkModuleName)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.Move (Anchor (..), ExportClose (..), MoveFacts (..))
import HInsight.Refactor.Verify (VerifiedPlan, planEdits, planNotes)
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import Test.Hspec

at :: (Int, Int) -> IO Position
at (l, c) = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf from to = do
  a <- at from
  b <- at to
  ok (mkSpan a b)

folder :: FilePath
folder = "test/fixtures/move/"

factsOf :: FilePath -> FilePath -> FilePath -> String -> IO (Either InsightError MoveFacts)
factsOf libDir from to name = do
  dir <- ok (mkLibDir libDir)
  source <- ok (mkSourceFile (folder <> from <> ".hs"))
  target <- ok (mkSourceFile (folder <> to <> ".hs"))
  identifier <- ok (mkIdentifier (T.pack name))
  ghcMoveFacts dir source target identifier

spec :: Spec
spec = describe "ghcMoveFacts, read from GHC" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "reads the facts of a move" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

movedBy :: FilePath -> FilePath -> FilePath -> String -> [FilePath] -> IO (Either InsightError VerifiedPlan)
movedBy libDir from to name others = do
  dir <- ok (mkLibDir libDir)
  source <- ok (mkSourceFile (folder <> from <> ".hs"))
  target <- ok (mkSourceFile (folder <> to <> ".hs"))
  identifier <- ok (mkIdentifier (T.pack name))
  watched <- traverse (\o -> ok (mkSourceFile (folder <> o <> ".hs"))) others
  moveDefinition (ghcRefactor dir) source target identifier watched

-- | The two files as the plan leaves them.
changed :: FilePath -> FilePath -> VerifiedPlan -> IO (Text, Text)
changed from to plan = do
  source <- ok (mkSourceFile (folder <> from <> ".hs"))
  target <- ok (mkSourceFile (folder <> to <> ".hs"))
  fromText <- TIO.readFile (folder <> from <> ".hs")
  toText <- TIO.readFile (folder <> to <> ".hs")
  case applyEditSet (\f -> if f == source then Just fromText else Just toText) (planEdits plan) of
    Left e -> fail (show e)
    Right m -> pure (M.findWithDefault fromText source m, M.findWithDefault toText target m)

isVerificationFailure :: Either InsightError a -> Bool
isVerificationFailure = \case
  Left (RefactorFailure (VerificationFailed _)) -> True
  _ -> False

refusal :: RefactorError -> Either InsightError MoveFacts
refusal = Left . RefactorFailure

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "reads a definition with its signature, both modules' imports and the export list it joins" $ do
    source <- ok (mkSourceFile (folder <> "A.hs"))
    target <- ok (mkSourceFile (folder <> "B.hs"))
    name <- ok (mkIdentifier "moved")
    targetName <- ok (mkModuleName "B")
    definition <- spanOf (11, 1) (12, 50)
    fromImports <- traverse (uncurry spanOf) [((3, 1), (3, 27)), ((4, 1), (4, 26)), ((5, 1), (5, 31))]
    toImports <- traverse (uncurry spanOf) [((3, 1), (3, 24))]
    fromAnchor <- AfterImport <$> at (5, 31)
    toAnchor <- AfterImport <$> at (3, 24)
    close <- at (1, 16)
    factsOf libDir "A" "B" "moved"
      `shouldReturn` Right
        MoveFacts
          { moveName = name,
            moveFrom = source,
            moveTo = target,
            moveToModule = targetName,
            moveSpan = definition,
            moveFromImports = fromImports,
            moveFromAnchor = fromAnchor,
            moveToImports = toImports,
            moveToAnchor = toAnchor,
            moveToExports = Just (ExportClose (close, True)),
            moveKeepsImport = True
          }
  it "reads that the source needs no import back when it neither uses nor exports the name" $ do
    Right facts <- factsOf libDir "A" "B" "shout"
    moveKeepsImport facts `shouldBe` False
    expected <- spanOf (14, 1) (15, 20)
    moveSpan facts `shouldBe` expected
  it "places imports after the module header of a target that has none, and adds no export" $ do
    Right facts <- factsOf libDir "A" "Bare" "moved"
    anchor <- AfterModuleHeader <$> at (1, 18)
    moveToAnchor facts `shouldBe` anchor
    moveToImports facts `shouldBe` []
    moveToExports facts `shouldBe` Nothing
  it "refuses a definition that uses another name of its module" $
    factsOf libDir "Uses" "B" "twice" `shouldReturn` refusal (UsesNamesOfItsModule ["double"])
  it "refuses a name the module does not define" $
    factsOf libDir "A" "B" "nothere" `shouldReturn` refusal (DefinitionNotFound "nothere")
  it "refuses a target that is the source" $
    factsOf libDir "A" "A" "moved" `shouldReturn` refusal MoveToSameModule
  it "refuses a target that already defines the name" $
    factsOf libDir "A" "Clash" "moved" `shouldReturn` refusal (TargetAlreadyDefines "moved")
  it "refuses a definition a pragma names" $
    factsOf libDir "Pragma" "B" "inlined" `shouldReturn` refusal HasPragmaOrAnnotation
  it "moves a definition and leaves out the imports GHC calls unused afterwards" $ do
    Right plan <- movedBy libDir "A" "B" "moved" []
    planNotes plan `shouldBe` []
    (a, b) <- changed "A" "B" plan
    a `shouldBe` "module A (keep, moved) where\n\nimport Data.Char (toUpper)\nimport B (moved)\n\nkeep :: Int -> Int\nkeep = succ\n\nshout :: String -> String\nshout = map toUpper\n"
    b `shouldBe` "module B (other, moved) where\n\nimport Data.List (sort)\nimport Data.List (sortOn)\nimport qualified Data.Map as M\n\nother :: [Int] -> [Int]\nother = sort\n\n-- | Pairs each number with its place in descending order.\nmoved :: [Int] -> M.Map Int Int\nmoved xs = M.fromList (zip (sortOn negate xs) xs)\n"
  it "lists the places in other files that GHC says still need a fix" $ do
    Right plan <- movedBy libDir "NoList" "B" "moved" ["Caller"]
    case planNotes plan of
      [note] -> do
        note `shouldSatisfy` T.isInfixOf "Caller.hs"
        note `shouldSatisfy` T.isInfixOf "does not export"
      notes -> expectationFailure ("expected one note, got " <> show notes)
  it "refuses a move that makes the modules import each other, with GHC's message" $
    movedBy libDir "A" "Back" "moved" [] >>= (`shouldSatisfy` isVerificationFailure)
