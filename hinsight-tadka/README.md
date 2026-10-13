# hinsight-tadka

Draws what [hinsight](../README.md) finds with [Tadka](https://hackage.haskell.org/package/tadka):

- a type mismatch is an error, with the expression underlined, the signature
  marked, and what GHC could not match as help;
- a typed hole is an error, with the wanted type and the ranked fits as notes;
- a pipeline joined by `.`, `$` or `>>=` is advice, with each stage's type as a
  note and the type of the whole chain on the chain.

```sh
cabal run hinsight-tadka-demo -- test/fixtures/TypeMismatch.hs
cabal run hinsight-tadka-demo -- --format narratable test/fixtures/Hole.hs
cabal run hinsight-tadka-demo -- --format json test/fixtures/Pipeline.hs
```

Run these from the repository root. `--format json` writes one JSON object per
diagnostic to standard output; the module graph and the count of errors
hinsight did not explain go to standard error so that standard output stays
JSON.

As a library:

```haskell
import HInsight.Tadka (findings, namedSource)
import Tadka (defaultConfig, reportDiagnostic)

-- text is the text of the file that was analysed
-- Right src = namedSource path text
-- Right fs = findings src analysis
-- mapM_ (reportDiagnostic defaultConfig) fs
```

Every `Finding` is a Tadka `Diagnostic`, so any Tadka renderer works. The
conversion from GHC's line and column to Tadka's character offsets counts a tab
as GHC does; see `docs/decisions.md`, item 12.
