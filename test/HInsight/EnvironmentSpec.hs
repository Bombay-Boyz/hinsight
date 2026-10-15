module HInsight.EnvironmentSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Environment
import HInsight.Error (DomainError (..), EnvironmentError (..), renderEnvironmentError)
import HInsight.Support (ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

flag :: Text -> IO GhcFlag
flag = ok . mkGhcFlag

-- | An environment for the supported compiler, from these flags.
environmentWith :: [Text] -> IO Environment
environmentWith flags = do
  component <- ok (mkComponentName "pkg:lib:pkg")
  root <- ok (mkProjectRoot "/work/pkg")
  fs <- traverse flag flags
  ok (mkEnvironment component root pinnedGhcVersion fs)

-- | Flags that change what errors there are, and flags that do not.
genFlagText :: Gen Text
genFlagText =
  frequency
    [ (3, elements ["-Wall", "-Wwarn", "-Wno-error=unused-imports", "-XLambdaCase", "-i.", "-package-db", "/store/package.db", "-fno-defer-type-errors", "-Werror-like", "-O2"]),
      (2, elements ["-Werror", "-Werror=incomplete-patterns", "-fdefer-type-errors", "-fdefer-typed-holes", "-fdefer-out-of-scope-variables", "-fwrite-ide-info", "-fwrite-interface"])
    ]

spec :: Spec
spec = describe "HInsight.Environment" $ do
  describe "mkComponentName" $ do
    it "refuses a blank name" $ do
      mkComponentName "" `shouldBe` Left (BlankText "component name")
      mkComponentName "  \t " `shouldBe` Left (BlankText "component name")
    it "keeps the name without surrounding whitespace" $
      fmap unComponentName (mkComponentName "  pkg:lib:pkg \n") `shouldBe` Right "pkg:lib:pkg"
  describe "mkProjectRoot" $ do
    it "refuses an empty path" $
      mkProjectRoot "" `shouldBe` Left (EmptyPath "project root")
    it "refuses a relative path and names it" $ do
      mkProjectRoot "pkg" `shouldBe` Left (RelativeRoot "pkg")
      mkProjectRoot "./pkg" `shouldBe` Left (RelativeRoot "./pkg")
    it "accepts an absolute path, including one with a space" $
      fmap projectRootPath (mkProjectRoot "/work/my project") `shouldBe` Right "/work/my project"
  describe "mkGhcFlag" $ do
    it "refuses an empty flag" $
      mkGhcFlag "" `shouldBe` Left (InvalidFlag "")
    it "refuses a flag with a NUL character" $
      mkGhcFlag "-DX\NUL" `shouldBe` Left (InvalidFlag "-DX\NUL")
    it "keeps a flag with spaces whole, since flags are not split again" $
      fmap unGhcFlag (mkGhcFlag "-optc-DNAME=a b") `shouldBe` Right "-optc-DNAME=a b"
  describe "GhcVersion" $ do
    it "reads the supported version and finds it equal to the pinned one" $
      parseGhcVersion "9.10.3" `shouldBe` Right pinnedGhcVersion
    it "ignores whitespace around the version, as a command prints it" $
      parseGhcVersion "9.10.3\n" `shouldBe` Right pinnedGhcVersion
    it "does not treat 9.10 as 9.10.0 or as 9.10.3" $ do
      parseGhcVersion "9.10" `shouldNotBe` Right pinnedGhcVersion
      parseGhcVersion "9.10" `shouldNotBe` parseGhcVersion "9.10.0"
    it "refuses text that is not dotted decimal numbers" $
      mapM_
        (\t -> parseGhcVersion t `shouldBe` Left (MalformedVersion t))
        ["", ".", "9..3", "9.10.", ".9", "9.x", "-1.2", "9.10.3-rc1", "+9", "9 10", "٩.1"]
    it "refuses a number too large for an Int instead of wrapping round" $
      parseGhcVersion "99999999999999999999999.1" `shouldBe` Left (MalformedVersion "99999999999999999999999.1")
    it "refuses no numbers and a negative number when built from numbers" $ do
      mkGhcVersion [] `shouldBe` Left (MalformedVersion "")
      mkGhcVersion [9, -1] `shouldBe` Left (MalformedVersion "9.-1")
    prop "reads back what it renders" $
      forAll (listOf1 (chooseInt (0, 100_000))) $ \parts ->
        fmap renderGhcVersion (mkGhcVersion parts) === Right (T.intercalate "." (map (T.pack . show) parts))
          .&&. (mkGhcVersion parts >>= parseGhcVersion . renderGhcVersion) === mkGhcVersion parts
  describe "mkEnvironment" $ do
    it "keeps the component, the root and the flags in order" $ do
      env <- environmentWith ["-XLambdaCase", "-Wall", "-i."]
      unComponentName (environmentComponent env) `shouldBe` "pkg:lib:pkg"
      projectRootPath (environmentRoot env) `shouldBe` "/work/pkg"
      map unGhcFlag (environmentFlags env) `shouldBe` ["-XLambdaCase", "-Wall", "-i."]
    it "accepts an environment without flags" $
      fmap (map unGhcFlag . environmentFlags) (environmentWith []) `shouldReturn` []
    it "refuses another compiler, naming both versions" $ do
      component <- ok (mkComponentName "pkg:lib:pkg")
      root <- ok (mkProjectRoot "/work/pkg")
      other <- ok (parseGhcVersion "9.14.1")
      let refused = mkEnvironment component root other []
      refused `shouldBe` Left (CompilerMismatch "9.14.1" "9.10.3")
      either renderEnvironmentError (const "") refused `shouldSatisfy` (\t -> "9.14.1" `T.isInfixOf` t && "9.10.3" `T.isInfixOf` t)
    prop "refuses every compiler but the pinned one" $
      forAll (listOf1 (chooseInt (0, 20))) $ \parts ->
        (mkGhcVersion parts /= Right pinnedGhcVersion) ==> ioProperty $ do
          component <- ok (mkComponentName "c")
          root <- ok (mkProjectRoot "/r")
          version <- ok (mkGhcVersion parts)
          pure (either (const True) (const False) (mkEnvironment component root version []))
  describe "summarise" $ do
    it "names the component, counts the flags and lists those left out of the session" $ do
      env <- environmentWith ["-Wall", "-Werror", "-fwrite-ide-info", "-XLambdaCase"]
      let s = summarise env
      unComponentName (summaryComponent s) `shouldBe` "pkg:lib:pkg"
      summaryFlagCount s `shouldBe` 4
      map unGhcFlag (summaryDropped s) `shouldBe` ["-Werror", "-fwrite-ide-info"]
    prop "counts every flag and leaves out exactly what sessionFlags does" $
      forAll (listOf genFlagText) $ \texts -> ioProperty $ do
        env <- environmentWith texts
        let s = summarise env
        pure (summaryFlagCount s === length texts .&&. summaryDropped s === sessionDropped (sessionFlags env))
  describe "sessionFlags" $ do
    it "starts with the working directory, written with GHC's own flag, then ignores environment files" $ do
      env <- environmentWith ["-XLambdaCase"]
      map unGhcFlag (sessionArguments (sessionFlags env)) `shouldBe` ["-working-dir", "/work/pkg", "-package-env=-", "-XLambdaCase"]
    it "keeps warnings as warnings by dropping -Werror in both forms, and reports them" $ do
      env <- environmentWith ["-Wall", "-Werror", "-Werror=incomplete-patterns", "-Wwarn"]
      let result = sessionFlags env
      map unGhcFlag (sessionArguments result) `shouldBe` ["-working-dir", "/work/pkg", "-package-env=-", "-Wall", "-Wwarn"]
      map unGhcFlag (sessionDropped result) `shouldBe` ["-Werror", "-Werror=incomplete-patterns"]
    it "drops the flags that defer errors, and keeps the ones that undo it" $ do
      env <- environmentWith ["-fdefer-type-errors", "-fdefer-typed-holes", "-fdefer-out-of-scope-variables", "-fno-defer-type-errors"]
      let result = sessionFlags env
      map unGhcFlag (sessionDropped result) `shouldBe` ["-fdefer-type-errors", "-fdefer-typed-holes", "-fdefer-out-of-scope-variables"]
      map unGhcFlag (drop 3 (sessionArguments result)) `shouldBe` ["-fno-defer-type-errors"]
    it "drops the flags that make GHC write files into the project" $ do
      env <- environmentWith ["-fwrite-ide-info", "-fwrite-interface", "-fwrite-if-simplified-core", "-fno-write-ide-info", "-hiedir=.hie"]
      let result = sessionFlags env
      map unGhcFlag (sessionDropped result) `shouldBe` ["-fwrite-ide-info", "-fwrite-interface", "-fwrite-if-simplified-core"]
      map unGhcFlag (drop 3 (sessionArguments result)) `shouldBe` ["-fno-write-ide-info", "-hiedir=.hie"]
    it "does not treat a similar flag as -Werror" $ do
      env <- environmentWith ["-Werror-like", "-Wno-error=unused-imports"]
      sessionDropped (sessionFlags env) `shouldBe` []
    it "reports nothing dropped for an environment without flags" $ do
      env <- environmentWith []
      sessionDropped (sessionFlags env) `shouldBe` []
    it "passes a very long flag list on whole" $ do
      let flags = replicate 20_000 "-XLambdaCase"
      env <- environmentWith flags
      length (sessionArguments (sessionFlags env)) `shouldBe` 20_003
    prop "loses nothing and reorders nothing: kept and dropped are the input" $
      forAll (listOf genFlagText) $ \texts -> ioProperty $ do
        env <- environmentWith texts
        let result = sessionFlags env
            kept = drop 3 (sessionArguments result)
        pure (length kept + length (sessionDropped result) === length texts)
    prop "leaves no flag behind that changes diagnostics, and a second pass drops nothing" $
      forAll (listOf genFlagText) $ \texts -> ioProperty $ do
        env <- environmentWith texts
        let kept = map unGhcFlag (drop 3 (sessionArguments (sessionFlags env)))
        second <- environmentWith kept
        pure
          ( all (`notElem` droppedTexts) kept
              .&&. not (any (T.isPrefixOf "-Werror=") kept)
              .&&. null (sessionDropped (sessionFlags second))
          )
    prop "keeps the relative order of the flags it keeps" $
      forAll (listOf genFlagText) $ \texts -> ioProperty $ do
        env <- environmentWith texts
        let kept = map unGhcFlag (drop 3 (sessionArguments (sessionFlags env)))
        pure (kept === filter (`notElem` droppedTexts) texts)
  where
    droppedTexts :: [Text]
    droppedTexts = ["-Werror", "-Werror=incomplete-patterns", "-fdefer-type-errors", "-fdefer-typed-holes", "-fdefer-out-of-scope-variables", "-fwrite-ide-info", "-fwrite-interface", "-fwrite-if-simplified-core"]
