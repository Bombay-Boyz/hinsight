# hinsight

[![ci](https://github.com/Bombay-Boyz/hinsight/actions/workflows/ci.yml/badge.svg)](https://github.com/Bombay-Boyz/hinsight/actions/workflows/ci.yml)

Answers questions about Haskell code that need GHC's own data.

It does these things for a Haskell module:

- **Type mismatches.** For an error where GHC expected one type and found
  another, it returns the two types, the sub-types at which they first differ,
  the source span, and where GHC says the expectation came from.
- **Typed holes.** For a `_` hole, it returns the candidate fits GHC found,
  ranked: fits that need no further holes first, then local before imported.
- **Dependency graph.** The project modules reachable from the file and the
  ordinary imports between them, in dependency order, with import cycles
  named. `{-# SOURCE #-}` imports are left out, so a cycle that an `hs-boot`
  file breaks is not reported.
- **Type flow.** For a chain joined by `.`, `$` or `>>=`, it returns each stage
  with the type GHC gave it, in the order data flows through them, and the type
  of the whole chain. For a file with errors the types come from a second
  typecheck with the errors deferred.
- **Other errors and hints.** A mismatch against a rigid type variable is
  explained like any mismatch. An unresolved class constraint is shown with the
  instances GHC says could match. A hint GHC gives (a similar name, a missing
  extension, a missing `do`) is shown in GHC's words. An error that is none of
  these is counted, never dropped.
- **What GHC logged on the way to an error.** The unification steps GHC took
  before it gave up on a type, from its own trace (`Deep`, `chainsFor`).
- **Template Haskell splices.** What GHC says each splice became, by position
  (`splicesFor`).
- **Core.** The simplifier's input and output, the specialisations and the rules
  that fired, for the file, optimised or not (`coreOf`). No STG.

Each of the last three runs GHC again with a dump on, so an ordinary analysis
does not pay for them. They present what GHC printed and infer nothing; where
the text could not be read it is carried raw and counted.

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
  By default packages are not resolved: imports from outside the project must
  be installed. With a cabal project they are: `hinsight-cabal` asks cabal how
  the file's component is compiled (its extensions, flags and packages) and the
  analysis runs in that environment; the report says which flags were left out
  of the session (`docs/phase-environment.md`). Each analysis starts a fresh
  GHC session, so it is correct but not fast; see `docs/decisions.md`, item 1.
- Type flow reads the typechecked program. For a file with errors it
  typechecks again with type errors, typed holes and out-of-scope variables
  deferred, so a chain next to an error is still shown; errors GHC cannot
  defer leave no pipelines. It recognises the infix uses of the standard `(.)`, `($)` and
  `(>>=)` only: not `(.) f g`, backticks, other operators, or a user's own
  definitions of these names. Its integration test runs against GHC 9.10.3;
  see `docs/decisions.md`, item 10.
- GHC exposes no structured solver steps. The explanation of a mismatch is built
  from the mismatch GHC records; the chains are GHC's own text trace, read and
  filtered, and are a presentation of what it logged, not a reconstruction.
  A trace is a text format that GHC does not promise to keep, so a GHC release
  means refreshing the fixtures (`docs/feasibility.md`).
- The trace, splice and Core passes are checked against GHC 9.10.3 by tests that
  run it. Open items of the explanation phase are listed in
  `docs/phase-explanation.md`, section 5, "Owed after the phase".

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
cabal run hinsight-demo -- --chains test/fixtures/ChainPair.hs
cabal run hinsight-demo -- --splices test/fixtures/splices/Use.hs
cabal run hinsight-demo -- --core test/fixtures/core/Spec.hs
```

`--chains=whole` shows every record of the binding, and `--core=unoptimised`
shows the Core at `-O0`. The modes are exclusive. It asks `ghc-9.10.3` for its library directory, or uses
`HINSIGHT_GHC_LIBDIR` if set. The file may import other modules of its own
project; imports from packages must be installed.

## Draw the findings with Tadka

`hinsight-tadka/` is a second package in this repository. It turns an analysis
into [Tadka](https://hackage.haskell.org/package/tadka) diagnostics, so the
findings can be drawn as annotated source, read aloud by a screen reader, or
emitted as JSON. hinsight itself depends on no renderer.

```sh
cabal run hinsight-tadka-demo -- test/fixtures/TypeMismatch.hs
cabal run hinsight-tadka-demo -- --format narratable test/fixtures/Hole.hs
cabal run hinsight-tadka-demo -- --format json test/fixtures/Pipeline.hs
```

See `hinsight-tadka/README.md` and `docs/decisions.md`, item 12.

`hinsight-cabal/` is a third package. It asks cabal, through
[hie-bios](https://hackage.haskell.org/package/hie-bios), how the component a
file belongs to is compiled, and returns that as an `Environment` for
`HInsight.Ghc.Session.ghcInsightIn`, or through the handle `ghcInsightWith`. Try
it with `cabal run hinsight-cabal-demo -- src/HInsight/Environment.hs`. See
`docs/phase-environment.md`, steps 3 and 4, and `docs/decisions.md`, items 13
and 15.

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
| `HInsight.Graph` | the project's modules and imports, as data, and their text rendering |
| `HInsight.Flow` | pipelines and the type at each stage, as data, and their text rendering |
| `HInsight.Identifier` | a validated name as written in source |
| `HInsight.Hole` | typed holes and fit ranking |
| `HInsight.Instance`, `Suggestion` | the instances GHC lists for a constraint; the hints GHC gives |
| `HInsight.Trace`, `Trace.Clean`, `Chain` | GHC's typechecker trace: reading a record, cleaning it, and the chain that ends at an error |
| `HInsight.Splice`, `Core` | what GHC's splice and Core dumps say, read into data |
| `HInsight.Deep` | the handle for the three passes above |
| `HInsight.Report` | plain-text reports of an analysis |
| `HInsight.Cli` | argument handling for the demo program |
| `HInsight.Error` | every failure, in one place |
| `HInsight.Ghc.*` | the GHC-facing edge: session, message and hole conversion |
| `HInsight.Toolchain` | finds GHC 9.10.3's library directory (public sub-library `hinsight-toolchain`) |

Dependencies point inward: `HInsight.Ghc.*` imports the domain modules, never
the reverse.

## Standards

The code follows `ENGINEERING_STANDARD.md` in this repository. Deliberate
deviations are recorded in `docs/decisions.md`.
