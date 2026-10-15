# hinsight-cabal

Asks cabal, through `hie-bios`, how the component a file belongs to is
compiled, and returns that as a hinsight `Environment`.

```haskell
import HInsight.Cabal (cabalEnvironment)

env <- cabalEnvironment "src/Foo.hs"   -- Either InsightError Environment
```

Or let hinsight ask for each file's environment itself:

```haskell
import HInsight.Cabal (cabalSource)
import HInsight.Ghc.Session (ghcInsightWith)

analyseFile (ghcInsightWith libDir cabalSource) file
```

Try it with `cabal run hinsight-cabal-demo -- [--environment cabal|header] FILE.hs`.

The result of `cabalEnvironment` is applied with `HInsight.Ghc.Session.ghcInsightIn`. The project
must pin GHC 9.10.3 (`with-compiler: ghc-9.10.3` in `cabal.project`); a plan
built by another compiler is reported as `CompilerMismatch`. Each call runs
cabal afresh, in its own cache folder, and prints nothing.

Only a cabal project is understood: a `hie.yaml` or a stack project is not
consulted. See `docs/phase-environment.md` in the repository root.
