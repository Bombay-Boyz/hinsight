# hinsight

[![ci](https://github.com/Bombay-Boyz/hinsight/actions/workflows/ci.yml/badge.svg)](https://github.com/Bombay-Boyz/hinsight/actions/workflows/ci.yml)

Answers questions about Haskell code that need GHC's own data.

This first version does three things for a single Haskell module:

- **Type mismatches.** For an error where GHC expected one type and found
  another, it returns the two types, the sub-types at which they first differ,
  the source span, and where GHC says the expectation came from.
- **Typed holes.** For a `_` hole, it returns the candidate fits GHC found,
  ranked: fits that need no further holes first, then local before imported.
- **Type flow.** For a chain joined by `.`, `$` or `>>=`, it returns each stage
  with the type GHC gave it, in the order data flows through them, and the type
  of the whole chain. For a file with errors the types come from a second
  typecheck with the errors deferred.

Results are plain typed values. The library draws nothing. The GHC-backed
implementation sits behind a record of functions (`HInsight.Insight`), so code
that consumes analyses can be tested with a pure stand-in.

It supports exactly one GHC version, **9.10.3**: the `ghc` library it links
must match the GHC that reads the project.

## Status

Early. Read this before relying on it.

- The pure core and the GHC-facing modules under `HInsight.Ghc` build and pass
  their tests against a real GHC 9.10.3: type mismatches, typed-hole fits and
  out-of-scope errors are extracted from real files.
- GHC's own origin text for a mismatch proved to be noise; the context line
  comes from the parsed source instead (`docs/decisions.md`, item 4).
- A file is analysed together with the modules it imports from its own
  project: the source folder is worked out from the file's module header and
  place, and those modules are typechecked first. A dependency that does not
  compile is reported as such and the file is not analysed against it.
  Packages are not resolved: imports from outside the project must be
  installed. Each analysis starts a fresh GHC session, so it is correct but
  not fast; see `docs/decisions.md`, item 1.
- Type flow reads the typechecked program. For a file with errors it
  typechecks again with type errors, typed holes and out-of-scope variables
  deferred, so a chain next to an error is still shown; errors GHC cannot
  defer leave no pipelines. It recognises the infix uses of the standard `(.)`, `($)` and
  `(>>=)` only: not `(.) f g`, backticks, other operators, or a user's own
  definitions of these names. Its integration test runs against GHC 9.10.3;
  see `docs/decisions.md`, item 10.
- It does not trace the constraint solver. GHC does not expose solver steps;
  the explanation is built from the mismatch GHC records.

## Build and test

This package needs GHC **9.10.3** exactly. `cabal.project` pins it with
`with-compiler: ghc-9.10.3`, so the default GHC in ghcup does not matter, but
9.10.3 must be installed (`ghcup install ghc 9.10.3`). Then, from this folder:

```sh
cabal build all
cabal test all
cabal bench
```

`cabal bench` times the ranking of hole fits against the plain sort it wraps,
the rendering of large reports, and real analyses of two fixtures (the last
only when `ghc-9.10.3` is found).

The tests that run a real GHC ask `ghc-9.10.3` for its library directory. To
use a different installation, set `HINSIGHT_GHC_LIBDIR` to its library
directory.

## Try it

A small demo program prints what hinsight finds in a file:

```sh
cabal run hinsight-demo -- test/fixtures/TypeMismatch.hs
cabal run hinsight-demo -- test/fixtures/Hole.hs
cabal run hinsight-demo -- test/fixtures/Pipeline.hs
```

It asks `ghc-9.10.3` for its library directory, or uses
`HINSIGHT_GHC_LIBDIR` if set. The file may import other modules of its own
project; imports from packages must be installed.

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
| `HInsight.Project` | where a project's source folder is, from one file's header and path |
| `HInsight.Source`, `Config` | validated positions, spans and paths |
| `HInsight.Explanation` | a type mismatch and where it sits, as data, and its text rendering |
| `HInsight.TypeText`, `Context` | a validated rendered type; where in the program a finding sits |
| `HInsight.Flow` | pipelines and the type at each stage, as data, and their text rendering |
| `HInsight.Identifier` | a validated name as written in source |
| `HInsight.Hole` | typed holes and fit ranking |
| `HInsight.Report` | plain-text reports of an analysis |
| `HInsight.Cli` | argument handling for the demo program |
| `HInsight.Error` | every failure, in one place |
| `HInsight.Ghc.*` | the GHC-facing edge: session, message and hole conversion |

Dependencies point inward: `HInsight.Ghc.*` imports the domain modules, never
the reverse.

## Standards

The code follows `ENGINEERING_STANDARD.md` in this repository. Deliberate
deviations are recorded in `docs/decisions.md`.
