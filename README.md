# hinsight

Answers questions about Haskell code that need GHC's own data.

This first version does two things for a single Haskell module:

- **Type mismatches.** For an error where GHC expected one type and found
  another, it returns the two types, the sub-types at which they first differ,
  the source span, and where GHC says the expectation came from.
- **Typed holes.** For a `_` hole, it returns the candidate fits GHC found,
  ranked: fits that need no further holes first, then local before imported.

Results are plain typed values. The library draws nothing. The GHC-backed
implementation sits behind a record of functions (`HInsight.Insight`), so code
that consumes analyses can be tested with a pure stand-in.

It supports exactly one GHC version, **9.10.3**: the `ghc` library it links
must match the GHC that reads the project.

## Status

Early. Read this before relying on it.

- The pure core (`HInsight.Source`, `Explanation`, `Hole`, `Error`, `Config`,
  `Analysis`) is covered by example and property tests.
- The modules under `HInsight.Ghc` target the GHC 9.10.3 API and were written
  against its source. They had **not been compiled** when this file was
  written. Expect small fixes on the first build. `docs/decisions.md` lists
  what to check.
- It analyses one module that imports only installed packages. A file that
  pulls in other modules of its own project is reported, not analysed.
- It does not trace the constraint solver. GHC does not expose solver steps;
  the explanation is built from the mismatch GHC records.

## Build and test

This package needs GHC **9.10.3** exactly. `cabal.project` pins it with
`with-compiler: ghc-9.10.3`, so the default GHC in ghcup does not matter, but
9.10.3 must be installed (`ghcup install ghc 9.10.3`). Then, from this folder:

```sh
cabal build all
cabal test all
```

The tests that run a real GHC ask `ghc-9.10.3` for its library directory. To
use a different installation, set `HINSIGHT_GHC_LIBDIR` to its library
directory.

## Use

```haskell
import HInsight
import HInsight.Ghc.Session (ghcInsight)

main :: IO ()
main = do
  let Right libDir = mkLibDir "/path/from/ghc-9.10.3 --print-libdir"
      Right file = mkSourceFile "Example.hs"
  result <- analyseFile (ghcInsight libDir) file
  either (putStrLn . show . renderError) print result
```

The paths are arguments: the library never searches for tools.

## Layout

| Module | Job |
|---|---|
| `HInsight` | public facade |
| `HInsight.Analysis` | the result type and the `Insight` handle |
| `HInsight.Source`, `Config` | validated positions, spans and paths |
| `HInsight.Explanation` | a type mismatch as data, and its text rendering |
| `HInsight.Hole` | typed holes and fit ranking |
| `HInsight.Error` | every failure, in one place |
| `HInsight.Ghc.*` | the GHC-facing edge: session, message and hole conversion |

Dependencies point inward: `HInsight.Ghc.*` imports the domain modules, never
the reverse.

## Standards

The code follows `ENGINEERING_STANDARD.md` in this repository. Deliberate
deviations are recorded in `docs/decisions.md`.
