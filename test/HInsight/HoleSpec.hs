module HInsight.HoleSpec (spec) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import HInsight.Edit (editFile, editSpan, editText)
import HInsight.Error (DomainError (..))
import HInsight.Hole
import HInsight.Identifier (mkIdentifier)
import HInsight.Source (mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (genHoleFit, ok, rightToMaybe)
import HInsight.TypeText (mkTypeText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

fitWith :: String -> Int -> Locality -> Int -> IO HoleFit
fitWith name level loc matches = do
  n <- ok (mkIdentifier (T.pack name))
  t <- ok (mkTypeText "Int")
  r <- ok (mkRefinementLevel level)
  pure (HoleFit n t r loc (replicate matches t) Nothing False)

syntheticFit :: Int -> Maybe HoleFit
syntheticFit i = do
  n <- rightToMaybe (mkIdentifier (T.pack ("f" <> show i)))
  t <- rightToMaybe (mkTypeText "Int")
  r <- rightToMaybe (mkRefinementLevel (i `mod` 4))
  pure (HoleFit n t r (if even i then Local else Imported) (replicate (i `mod` 3) t) Nothing False)

levels :: [HoleFit] -> [Int]
levels = map (unRefinementLevel . fitRefinement)

adjacentOk :: [HoleFit] -> Bool
adjacentOk fs = and (zipWith pairOk fs (drop 1 fs))
  where
    pairOk :: HoleFit -> HoleFit -> Bool
    pairOk a b = fitRefinement a /= fitRefinement b || fitLocality a <= fitLocality b

spec :: Spec
spec = do
  describe "mkRefinementLevel" $ do
    it "rejects the value just below zero" $
      mkRefinementLevel (-1) `shouldBe` Left (NegativeRefinement (-1))
    it "accepts zero" $
      (unRefinementLevel <$> mkRefinementLevel 0) `shouldBe` Right 0
    it "accepts the largest Int" $
      (unRefinementLevel <$> mkRefinementLevel maxBound) `shouldBe` Right maxBound

  describe "rankFits" $ do
    it "keeps the empty list empty" $
      rankFits [] `shouldBe` []
    it "keeps a single fit" $ do
      f <- fitWith "x" 0 Local 0
      rankFits [f] `shouldBe` [f]
    it "orders by refinement, then locality, then new holes" $ do
      localOne <- fitWith "a" 1 Local 0
      importedZero <- fitWith "b" 0 Imported 0
      localZero <- fitWith "c" 0 Local 0
      importedOne <- fitWith "d" 1 Imported 0
      localZeroWithHole <- fitWith "a" 0 Local 1
      rankFits [localOne, importedZero, importedOne, localZeroWithHole, localZero]
        `shouldBe` [localZero, localZeroWithHole, importedZero, localOne, importedOne]
    prop "keeps exactly the same fits" $
      forAll (listOf genHoleFit) $
        \fs -> sort (rankFits fs) === sort fs
    prop "is idempotent" $
      forAll (listOf genHoleFit) $
        \fs -> rankFits (rankFits fs) === rankFits fs
    prop "does not depend on the input order" $
      forAll (listOf genHoleFit) $ \fs ->
        forAll (shuffle fs) $ \shuffled -> rankFits shuffled === rankFits fs
    prop "never puts a fit needing more holes first" $
      forAll (listOf genHoleFit) $ \fs ->
        let ls = levels (rankFits fs) in ls === sort ls
    prop "puts local before imported at the same level" $
      forAll (listOf genHoleFit) $
        \fs -> adjacentOk (rankFits fs)
    it "ranks a hundred thousand fits without losing any" $ do
      let fs = mapMaybe syntheticFit [1 .. 100_000]
          ranked = rankFits fs
      length ranked `shouldBe` 100_000
      levels ranked `shouldBe` sort (levels fs)
      adjacentOk ranked `shouldBe` True

  describe "mkModuleAlias" $ do
    it "refuses blank text" $
      mkModuleAlias "  " `shouldBe` Left (BlankText "module alias")
    it "keeps the alias without surrounding whitespace" $
      (unModuleAlias <$> mkModuleAlias " M ") `shouldBe` Right "M"

  describe "acceptFit" $ do
    let accepted level name qualifier operator = do
          f <- fitWith name level Imported 0
          alias <- traverse (ok . mkModuleAlias) qualifier
          file <- ok (mkSourceFile "A.hs")
          line <- ok (mkLine 3)
          start <- mkPosition line <$> ok (mkColumn 5)
          end <- mkPosition line <$> ok (mkColumn 6)
          sp <- ok (mkSpan start end)
          holeId <- ok (mkIdentifier "_")
          ty <- ok (mkTypeText "Int")
          let chosen = f {fitQualifier = alias, fitIsOperator = operator}
              hole = HoleReport holeId sp ty [chosen]
          e <- ok (acceptFit file hole chosen)
          (editFile e, editSpan e) `shouldBe` (file, sp)
          pure (editText e)
    it "writes a fit with no new holes as its name" $
      accepted 0 "x" Nothing False `shouldReturn` "x"
    it "writes a fit with new holes as an application to that many holes" $
      accepted 2 "f" Nothing False `shouldReturn` "(f _ _)"
    it "writes a fit imported qualified with its alias" $
      accepted 0 "lookup" (Just "M") False `shouldReturn` "M.lookup"
    it "puts an operator in parentheses, qualified or not" $ do
      accepted 0 "+" Nothing True `shouldReturn` "(+)"
      accepted 0 "+" (Just "M") True `shouldReturn` "(M.+)"
    it "applies an operator that has new holes" $
      accepted 1 "+" Nothing True `shouldReturn` "((+) _)"
