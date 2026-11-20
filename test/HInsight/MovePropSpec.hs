-- |
-- Module      : HInsight.MovePropSpec
-- Description : What a move does to any module of a generated shape, and to a very large one.
module HInsight.MovePropSpec (spec) where

import Data.Bool (bool)
import Data.List (isSubsequenceOf)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (applyEditSet)
import HInsight.Graph (mkModuleName)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.Move
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | One top-level definition of a generated module.
data Def = Def
  { defDoc :: !(Maybe Text),
    defBody :: ![Text]
  }
  deriving stock (Show)

-- | A generated source module: its imports, its definitions, which one moves,
-- how the text ends, and what line ends it uses.
data Shape = Shape
  { shapeImports :: ![Text],
    shapeDefs :: ![Def],
    shapeMoved :: !Int,
    shapeFinalNewline :: !Bool,
    shapeCrlf :: !Bool,
    shapeKeeps :: !Bool,
    shapeTargetImports :: ![Text]
  }
  deriving stock (Show)

importPool :: [Text]
importPool = ["import Data.List (sort)", "import Data.Char (toUpper)", "import qualified Data.Map as M", "import Data.Maybe (fromMaybe)"]

genShape :: Gen Shape
genShape = do
  imports <- sublistOf importPool
  count <- chooseInt (1, 6)
  defs <- vectorOf count genDef
  which <- chooseInt (0, count - 1)
  Shape imports defs which <$> arbitrary <*> arbitrary <*> arbitrary <*> sublistOf importPool

genDef :: Gen Def
genDef = do
  doc <- oneof [pure Nothing, Just <$> elements ["-- | A doc.", "{- | A block doc. -}"]]
  extra <- chooseInt (0, 2)
  pure (Def doc (replicate extra "  + 1"))

nameOf :: Int -> Text
nameOf i = "d" <> T.pack (show i)

-- | The lines of a definition: its signature and its equation.
defLines :: Int -> Def -> [Text]
defLines i d = [nameOf i <> " :: Int -> Int", nameOf i <> " x = x"] <> defBody d

-- | The text of a module and the 1-based line each piece starts on.
render :: Shape -> (Text, [(Int, Int)])
render s = (ending (T.intercalate eol allLines), spans)
  where
    eol :: Text
    eol = bool "\n" "\r\n" (shapeCrlf s)
    ending :: Text -> Text
    ending t = if shapeFinalNewline s then t <> eol else t
    header :: [Text]
    header = ["module A (" <> T.intercalate ", " [nameOf i | i <- [0 .. length (shapeDefs s) - 1]] <> ") where", ""]
    importBlock :: [Text]
    importBlock = shapeImports s <> [""]
    blocks :: [[Text]]
    blocks = [maybe [] pure (defDoc d) <> defLines i d | (i, d) <- zip [0 ..] (shapeDefs s)]
    allLines :: [Text]
    allLines = header <> importBlock <> T.splitOn "\n" (T.intercalate "\n\n" (map (T.intercalate "\n") blocks))
    start0 :: Int
    start0 = length header + length importBlock + 1
    spans :: [(Int, Int)]
    spans = go start0 (zip [0 :: Int ..] (shapeDefs s))
    go :: Int -> [(Int, Def)] -> [(Int, Int)]
    go _ [] = []
    go from ((i, d) : rest) =
      let docLines = maybe 0 (const 1) (defDoc d)
          signature = from + docLines
          end = signature + length (defLines i d) - 1
       in (signature, end) : go (end + 2) rest

at :: (Int, Int) -> IO Position
at (l, c) = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf a b = do
  p <- at a
  q <- at b
  ok (mkSpan p q)

-- | The facts GHC would give for the generated module and a small target.
factsFor :: Shape -> Text -> IO (MoveFacts, Text)
factsFor s sourceText = do
  a <- ok (mkSourceFile "A.hs")
  b <- ok (mkSourceFile "B.hs")
  m <- ok (mkModuleName "B")
  n <- ok (mkIdentifier (nameOf (shapeMoved s)))
  let ls = T.splitOn "\n" sourceText
      width :: Int -> Int
      width l = maybe 0 (T.length . T.dropWhileEnd (== '\r')) (lookup l (zip [1 ..] ls))
  (from, to) <- maybe (fail "the shape has no such definition") pure (listToMaybe (drop (shapeMoved s) (snd (render s))))
  def <- spanOf (from, 1) (to, width to + 1)
  let importLines = [1 + 2 + i | i <- [0 .. length (shapeImports s) - 1]]
  imports <- traverse (\l -> spanOf (l, 1) (l, width l + 1)) importLines
  fromAnchor <- case reverse importLines of
    l : _ -> AfterImport <$> at (l, width l + 1)
    [] -> AfterModuleHeader <$> at (1, width 1 + 1)
  let targetText = T.unlines (["module B (b0) where", ""] <> shapeTargetImports s <> ["", "b0 :: Int", "b0 = 0"])
      tw :: Int -> Int
      tw l = maybe 0 T.length (lookup l (zip [1 ..] (T.lines targetText)))
      targetImportLines = [3 .. 2 + length (shapeTargetImports s)]
  toImports <- traverse (\l -> spanOf (l, 1) (l, tw l + 1)) targetImportLines
  toAnchor <- case reverse targetImportLines of
    l : _ -> AfterImport <$> at (l, tw l + 1)
    [] -> AfterModuleHeader <$> at (1, tw 1 + 1)
  close <- at (1, 16)
  pure (MoveFacts n a b m def imports fromAnchor toImports toAnchor (Just (ExportClose (close, True))) (shapeKeeps s), targetText)

-- | Both texts after the plan, when there is one.
moved :: Pruning -> Shape -> IO (Maybe (Text, Text, Text))
moved pruning s = do
  let (sourceText, _) = render s
  (fs, targetText) <- factsFor s sourceText
  case planMove pruning sourceText targetText fs of
    Left _ -> pure Nothing
    Right set -> case applyEditSet (\f -> if f == moveFrom fs then Just sourceText else Just targetText) set of
      Left _ -> pure Nothing
      Right m -> pure (Just (sourceText, M.findWithDefault sourceText (moveFrom fs) m, M.findWithDefault targetText (moveTo fs) m))

-- | The lines of the moved definition, with its doc, as the source writes them.
movedText :: Shape -> Text -> [Text]
movedText s sourceText = case drop (shapeMoved s) (snd (render s)) of
  (from, to) : _ ->
    let docLines = maybe 0 (const 1) (defDoc =<< rightToMaybe' (shapeDefs s) (shapeMoved s))
     in take (to - from + 1 + docLines) (drop (from - 1 - docLines) (map (T.dropWhileEnd (== '\r')) (T.splitOn "\n" sourceText)))
  [] -> []
  where
    rightToMaybe' ds i = case drop i ds of
      d : _ -> Just d
      [] -> Nothing

spec :: Spec
spec = describe "planMove over generated modules" $ do
  prop "always gives a plan for facts that describe the module" $
    forAll genShape $ \s -> ioProperty $ do
      result <- moved mempty s
      pure (result =/= Nothing)
  prop "takes the moved definition, with its doc, out of the source and puts it once at the end of the target" $
    forAll genShape $ \s -> ioProperty $ do
      result <- moved mempty s
      pure $ case result of
        Nothing -> counterexample "no plan" False
        Just (sourceText, newSource, newTarget) ->
          let block = T.intercalate "\n" (movedText s sourceText)
              count needle hay = length (T.breakOnAll needle hay)
           in counterexample (T.unpack newTarget) (count block (T.replace "\r\n" "\n" newTarget) === 1 .&&. count block (T.replace "\r\n" "\n" newSource) === 0)
  prop "leaves every other definition in the source as it was" $
    forAll genShape $ \s -> ioProperty $ do
      result <- moved mempty s
      pure $ case result of
        Nothing -> counterexample "no plan" False
        Just (sourceText, newSource, _) ->
          let others = [T.intercalate "\n" (map (T.dropWhileEnd (== '\r')) (defLines i d)) | (i, d) <- zip [0 ..] (shapeDefs s), i /= shapeMoved s]
              plain = T.replace "\r\n" "\n" newSource
           in counterexample (T.unpack sourceText) (conjoin [counterexample (T.unpack o) (o `T.isInfixOf` plain) | o <- others])
  prop "only inserts into the target" $
    forAll genShape $ \s -> ioProperty $ do
      result <- moved mempty s
      pure $ case result of
        Nothing -> counterexample "no plan" False
        Just (_, _, newTarget) -> counterexample (T.unpack newTarget) (isSubsequenceOf (T.unpack (T.unlines (["module B (b0) where", ""] <> shapeTargetImports s <> ["", "b0 :: Int", "b0 = 0"]))) (T.unpack newTarget))
  prop "copies no import when every one is pruned" $
    forAll genShape $ \s -> ioProperty $ do
      (fs, _) <- factsFor s (fst (render s))
      result <- moved (Pruning (moveFromImports fs) []) s
      pure $ case result of
        Nothing -> counterexample "no plan" False
        Just (_, _, newTarget) -> counterexample (T.unpack newTarget) (conjoin [counterexample (T.unpack i) (not (i `T.isInfixOf` newTarget) .||. i `elem` shapeTargetImports s) | i <- shapeImports s])
  prop "writes the line ending of the file each piece goes into" $
    forAll genShape $ \s -> ioProperty $ do
      result <- moved mempty s
      pure $ case result of
        Nothing -> counterexample "no plan" False
        Just (_, newSource, newTarget) ->
          counterexample (show (newSource, newTarget)) (not (T.isInfixOf "\r\n" newTarget) .&&. (shapeCrlf s || not (T.isInfixOf "\r\n" newSource)))
  describe "a very large module" $
    it "moves one definition out of a hundred thousand lines" $ do
      let defs = replicate 25000 (Def (Just "-- | A doc.") ["  + 1"])
          s = Shape ["import Data.List (sort)"] defs 12345 True False True []
          (sourceText, _) = render s
      (fs, targetText) <- factsFor s sourceText
      case planMove mempty sourceText targetText fs of
        Left e -> expectationFailure (show e)
        Right set -> case applyEditSet (\f -> if f == moveFrom fs then Just sourceText else Just targetText) set of
          Left e -> expectationFailure (show e)
          Right m -> do
            let newSource = M.findWithDefault sourceText (moveFrom fs) (m :: M.Map SourceFile Text)
            T.length newSource `shouldSatisfy` (< T.length sourceText)
            (nameOf 12345 <> " :: Int -> Int") `shouldSatisfy` (\n -> not (n `T.isInfixOf` newSource))
