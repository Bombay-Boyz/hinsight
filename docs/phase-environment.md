# Next phase: the project environment (package resolution)

Status: **planned, not started.** Nothing in this document is built. It follows
the engineering standard: prior art first (5.7), one canonical representation
(2.1), effects behind a handle (0.5), and a named test for every claim (5.5).

## 1. The gap

Today `hinsight` analyses a file with one piece of project knowledge: the
source folder, worked out from the module header and the file's place. A real
project tells GHC much more, and none of it reaches the session:

| What a project says | Where it lives | Today |
|---|---|---|
| Packages the code imports | `build-depends`, resolved by cabal into package databases | Not resolved; the import must be installed globally |
| Language extensions | `default-extensions`, `default-language`, `{-# LANGUAGE #-}` | Only the pragmas in the file; cabal's defaults are missing |
| Compiler options | `ghc-options`, `cpp-options`, `-D` flags | Missing |
| Which component owns the file | library, executable, test suite, each with its own folders and dependencies | Guessed from the header |
| Generated modules | `Paths_pkg`, alex, happy, hsc2hs | Not found |

The sharpest consequence is a concrete one, which I expect but have not run:
`hinsight` cannot analyse its own source, because its modules rely on
`LambdaCase`, `OverloadedStrings` and `DerivingStrategies` set as cabal
defaults. Making that work is the phase's first acceptance test.

## 2. Goal and non-goals

**Goal.** Given a file in a cabal project, analyse it in the same environment
cabal would compile it in, and say which environment that was. When no
environment can be found, say so with a typed error and fall back to today's
behaviour only when asked.

**Not in this phase:** stack, bios or other build tools (the design leaves a
place for them, nothing more); building or generating modules; several roots in
one workspace; Backpack; a long-lived session (it is the phase after, on
evidence from the benchmark); semantic refactoring.

## 3. Design

### 3.1 One representation, one handle

A new domain type, `Environment`, is the single value every source of project
knowledge funnels into: the component's name, an absolute root, and the GHC
flags that component is compiled with. It is built only through a smart
constructor, and it carries the compiler it was captured for.

```haskell
newtype EnvironmentSource m = EnvironmentSource
  { environmentFor :: SourceFile -> m (Either EnvironmentError Environment)
  }
```

This axis is open by design (Standard 0.2, 2.7): another consumer may supply
stack or a custom build, so it is a record of functions, not a closed sum.

**The handle is added in step 4, not step 1.** Nothing consumes it before the
analysis is wired to take an environment, and a handle with no producer or
consumer is speculative (Standard 0.7). For the same reason today's behaviour is
not modelled as an `Environment` yet: its source folder is found from the
module header, which needs a first GHC pass to read, so it cannot be a pure
value. It stays in the session code until step 4 decides how the two meet.

### 3.2 How the flags are obtained

Three options were weighed.

| Option | For | Against |
|---|---|---|
| **A. Read `plan.json` and the `.cabal` file ourselves** | No process to run | `plan.json` names units and databases but not extensions or options, so we would re-implement cabal's own flag computation. That is duplicated knowledge (0.6) and drifts from cabal. **Rejected.** |
| **B. Ask cabal: run `cabal repl` with a stand-in compiler that records its arguments** (the method of `hie-bios`' cabal cradle) | Exactly what cabal would pass, including CPP, preprocessors and package databases | Needs `cabal` on the path; costs a process start and cabal's lock on `dist-newstyle` |
| **C. Depend on the `hie-bios` library** | Maintained, used by every language server (2.11, 4.7) | Heavy dependency with tight `ghc` bounds; must support GHC 9.10.3 |

**Decision (step 0): C.** `hie-bios`' cabal capture has needed a fix for
nearly every cabal release (prior-art class 10), which is the case for reusing
it. It declares `ghc >=9.2.1 && <10.2`, so 9.10.3 is inside its bounds, but
that it builds against 9.10.3 is **not verified** and is checked the first
time the adapter is built. If it does not build, this is reopened.

Either way the implementation lives in a **separate adapter package**, the same
shape as `hinsight-tadka`. `hinsight` core defines `Environment`, the handle
and the code that applies flags to a session. The adapter supplies a handle
that talks to cabal. Core stays free of process and build-tool code, and the
dependency points inward (2.6).

### 3.3 Applying the flags to the session

The GHC edge parses the flags with GHC's own flag parser (`parseDynamicFlags`;
the exact signature is to be read from 9.10.3's source in step 2) and sets the
result on the session. Three rules, each with a test:

1. **Our own settings win, and are applied last.** No code generation and no
   linking, whatever the project says.
2. **Warnings stay warnings.** A project's `-Werror` must not turn warnings
   into errors, or the count of "errors not explained" would be wrong.
3. **Unknown or rejected flags are typed errors**, naming the flag, never
   dropped silently.

Relative paths in the flags are resolved with GHC's own `-working-dir` flag,
which GHC 9.10.3's user guide says offsets every relative path the compiler
uses: `-i`, `-I`, output folders and input files. So no path is rewritten, and
the process's working directory is never changed (it is global state, 1.8, and
the library may be used from several threads). Not confirmed by that text:
`-package-db`. Build tools write package databases as absolute paths, and step 2
tests it.

### 3.4 Errors

The error vocabulary gains one closed sum, `EnvironmentError`, in
`HInsight.Error` (2.15). Every constructor is added in the change that reaches
it, not before (1.5). The ones the design already expects:

- no cabal project found above the file;
- the file belongs to no component, or to more than one (the message lists them);
- cabal could not be run or exited with an error (its output is kept);
- the plan was built with a different compiler than GHC 9.10.3. This matters
  here: your default compiler in ghcup is 9.14.1, so a plan built by it holds
  interface files this GHC cannot read;
- a flag GHC rejected.

### 3.5 What the user sees

`Analysis` gains a summary of the environment used (component, root, number of
flags), shown in the plain report and in the Tadka demo, so an unexpected
result can be traced to the environment rather than guessed at. A new demo
option chooses `--environment cabal` or `--environment header` (today's
behaviour); the default is decided after step 4 from how reliable cabal mode
proves.

## 4. Steps, in order

Each step ends with the verification practice used so far: formatted, linted,
built with `-Werror`, pure tests run here, and the GHC and cabal parts run on
your machine.

**Step 0 — prior art (Standard 5.7). Done at changelog level; see `docs/prior-art.md`, classes 10 to 22.** The issue trackers could not be read from here, which that document says plainly. Read `hie-bios`' cabal cradle and its
issue tracker, and the `ghcide` and `haskell-language-server` trackers, for how
projects fail to load. Extend `docs/prior-art.md` with numbered classes, each
with a named regression test, **before any code**. This also answers the open
question in 3.2. The existing prior-art note says plainly that this deeper pass
is still owed. Failure classes I expect to find, from general knowledge and to
be verified, not assumed:

- the wrong component chosen when a file sits in several (library and tests);
- flags with paths relative to a directory that is not the current one;
- `-Werror` in a project's options changing what counts as an error;
- a plan stale after the `.cabal` file changed;
- cabal's lock on `dist-newstyle` blocking a second invocation;
- generated modules (`Paths_pkg`, preprocessor output) missing;
- Template Haskell, which needs more than "no code generation" to run a splice.

The last is the one most likely to force a design change, so it gets its own
real-GHC experiment in this step, before the design is frozen.

**Step 1 — the pure core. Done.** `HInsight.Environment`: `ComponentName`,
`ProjectRoot` (absolute), `GhcFlag`, `GhcVersion`, and `Environment`, whose
smart constructor refuses flags captured for any compiler but 9.10.3 (prior-art
class 14). `sessionFlags` turns an environment into session arguments: it puts
`-working-dir` first and drops `-Werror`, `-Werror=...` and the three
`-fdefer-*` flags, reporting what it dropped (rules 3.3.2 and 3.3.3, prior-art
class 16). New errors: `CompilerMismatch`, and three construction errors. All of
it is pure and tested here, with properties that nothing is lost or reordered
and that a second pass drops nothing. It is not yet connected to anything.

**Step 2 — the GHC edge. Written; GHC-facing parts unverified until built.**
`HInsight.Ghc.Environment.applyEnvironment` hands `sessionArguments` to GHC's
own `parseDynamicFlags` (9.10.3: `Logger -> DynFlags -> [Located String] ->
m (DynFlags, [Located String], [Warn])`). A thrown `GhcException` becomes
`FlagRejected` (a flag with a value it cannot read); leftover arguments become
`UnrecognisedArguments`. Measured on real GHC: an *unknown* flag is a leftover,
not a thrown exception; both are in `InsightError` as `EnvironmentFailure`.
`HInsight.Ghc.Session` gains `ghcInsightIn :: LibDir -> Environment -> Insight
IO`. In that mode the environment's flags are applied first, then `noBackend`
and `NoLink` last, so no project flag can switch code generation back on
(rule 3.3.1). The header-based source-root guess is skipped, because `-i` comes
from the flags, and the target is given to GHC as an absolute path because GHC
resolves relative targets against `-working-dir`. `sessionFlags` now also
passes `-package-env=-`, so a `.ghc.environment` file in the process's directory
cannot add packages (prior-art class 15).

`-package-db`, the open question of 3.3: GHC 9.10.3's `GHC.Unit.State` maps
`offsetPackageDb (workingDirectory dflags)` over the package database flags, so
relative `-package-db` paths are offset by `-working-dir` like `-i`. Read from
source, not run; a package-database fixture is left to step 3, where a real
cabal store exists.

Real-GHC tests (`test/HInsight/GhcEnvironmentSpec.hs`, fixtures in
`test/fixtures/env`): a default extension (`-XLambdaCase`; without it the file
does not parse), a `-D` definition under CPP, an unknown flag, a flag with an unreadable value, a stray
file-name argument, `projectWerrorKeepsErrorCount`, `templateHaskellInProjectModule`,
and a dogfood check that analyses `src/HInsight/Environment.hs` with the
library's own flags.

The Template Haskell experiment is the one most likely to force a change. The
dependency (`Gen`) is loaded as a target with `noBackend`, and `UsesTh` splices
a function from it; `enableCodeGenForTH` in `GHC.Driver.Make.load` should give
`Gen` bytecode, but only if the graph that `load` sees contains the TH-using
module, which hinsight's two-step load may not arrange. If the test fails, the
fix is to be chosen from its message, not guessed.

**Step 3 — the cabal adapter. Done and verified on real cabal and GHC.** The package
`hinsight-cabal` (module `HInsight.Cabal`, `cabalEnvironment :: FilePath -> IO
(Either InsightError Environment)`). It finds the project the way cabal does
(`HInsight.Cabal.Project`: nearest `cabal.project`, else nearest `.cabal`),
builds a cabal cradle for it with `hie-bios`, and asks for the file's component
flags (`LoadFile`) and the compiler's version. The build folder it gives cabal
is `hie-bios`'s own cache, kept after the call: a first real run showed that the
flags name package databases inside it, so a cache deleted after capture breaks
the analysis that follows (`can't find a package database`). `hie-bios` locks
that cache, so simultaneous calls are safe. `HInsight.Cabal.Capture` turns that into an `Environment` and is pure:
it refuses another compiler, drops empty arguments, and names the component by
its `-this-unit-id`. A `hie.yaml` and stack are not consulted in this version.
Core changes with it: `EnvironmentError` gains `NoCabalProject` and
`CaptureFailed`; `sessionFlags` also drops `-fwrite-ide-info`,
`-fwrite-interface` and `-fwrite-if-simplified-core`, which would make GHC write
into the project's build folders (class 11); and `applyEnvironment` leaves out
the targets cabal lists after a component's flags (arguments GHC leaves over
that do not start with a dash), while a leftover that does start with a dash is
still an unknown flag.

Tests, with cabal projects written into temporary folders: the pure capture
(`otherCompilerIsRefused` and others), the project finder (`spaceInPathAnalyses`,
`symlinkedFileAnalyses`, `nestedProjectUsesInnerCabalFile`), and, when `cabal`
and GHC 9.10.3 are found, real captures: the component's root, extensions and
name; an end-to-end analysis with the captured flags; `editedCabalFileIsSeen`;
`captureLeavesProjectBuildAlone`; `twoAnalysesAtOnce`. Not written: a file in
no component or in two (class 12), a custom `Setup` on an old Cabal (class 10),
`dotGhciDoesNotChangeFlags` (class 15), `generatedModuleMissingIsUnexplained`
(class 17). What `hie-bios` does for those is not known from its source, so
they wait for a real run to show it.

**Unverified, and the first thing to check:** that `hie-bios` 0.21 builds against
GHC 9.10.3 here, that cabal's flags (which include `-package-id` and similar)
are accepted by the session, and that `-this-unit-id` is among them.

**Step 4 — wire it through. Done; acceptance run by hand on real cabal and GHC.**
- The handle `EnvironmentSource m` (`environmentFor :: SourceFile -> m (Either
  InsightError Environment)`) is in `HInsight.Environment`. Its consumer is
  `HInsight.Ghc.Session.ghcInsightWith`, which checks the file exists, asks the
  source, then analyses in the result; its producer is `HInsight.Cabal.cabalSource`.
- `Analysis` gains `analysisEnvironment :: Maybe EnvironmentSummary` (component,
  flag count, and the flags left out of the session). `Nothing` means the
  file's place was worked out from its header. `renderAnalysis` and the Tadka
  `renderExtras` print one line from it, so what was dropped is said, not lost.
- The demo with `--environment cabal|header` is `hinsight-cabal-demo`, in
  `hinsight-cabal`. The plan said `hinsight-demo`, but that executable is in
  core, and core must not depend on the adapter that depends on it. The core
  `hinsight-demo` and `hinsight-tadka-demo` are unchanged and stay on the
  header placement; the Tadka demo prints the environment line only when an
  analysis has one. Giving it `--environment` would make `hinsight-tadka`
  depend on `hie-bios`, which is left undone.

Acceptance, run by hand from the repository root (each takes cabal's time on
the first run, because the dependencies are built into `hie-bios`'s cache):

```
cabal run hinsight-cabal-demo -- src/HInsight/Environment.hs
cabal run hinsight-cabal-demo -- hinsight-tadka/src/HInsight/Tadka.hs
cabal run hinsight-cabal-demo -- --environment header hinsight-tadka/src/HInsight/Tadka.hs
```

Result. The first run analysed `src/HInsight/Environment.hs` in component
`hinsight-0.1.0.0-inplace` (91 flags; `-Werror` and `-fwrite-ide-info` left out)
and showed its type flow and a three-module graph. The second analysed
`hinsight-tadka/src/HInsight/Tadka.hs` in `hinsight-tadka-0.1.0.0-inplace` (67
flags), resolving `tadka` and `prettyprinter` from the cabal store and the
inplace `hinsight` package from `hie-bios`'s cache, and showed its type flow and
a two-module graph. The third, with `--environment header`, failed before it got
to the packages: `Illegal \case ... Perhaps you intended to use LambdaCase`,
because the header placement does not know the project's default extensions.
That is a stronger demonstration than the missing package I predicted, and it
is what the environment is for.

**Step 5 — cost and a baseline. Written; numbers still to be measured by hand.**
`hinsight-cabal-bench` (a benchmark of the adapter package) times, on
`src/HInsight/Cabal/Capture.hs`, which imports the package's own modules:
capturing the environment with the cache warm; analysing in an environment that
was captured beforehand; and both together (`ghcInsightWith`). None is
thresholded, and the benchmark is not part of CI: every capture starts a cabal
process, and an absolute time needs a fixed runner (decisions item 6). CI's
benchmark step now names the core benchmark only.

Run it with `cabal bench hinsight-cabal-bench --benchmark-options='--stdev 100'`
(the loose target keeps a seconds-long operation from being repeated for
minutes). The first capture of a project is not in these numbers: it builds the
project's dependencies into `hie-bios`'s cache. Measure it once, with a cache
folder of its own, cold and then warm:

```
export HIE_BIOS_CACHE_DIR=$(mktemp -d)
time cabal run hinsight-cabal-demo -- hinsight-cabal/src/HInsight/Cabal/Capture.hs   # cold
time cabal run hinsight-cabal-demo -- hinsight-cabal/src/HInsight/Cabal/Capture.hs   # warm
unset HIE_BIOS_CACHE_DIR
```

Whether a long-lived session is now worth its stale-state bugs is judged from
these numbers, not before. The recorded baseline goes below, once measured.

**Step 6 — documentation.** `docs/decisions.md` gets the entries for 3.1 to 3.3;
the README status loses "packages are not resolved" only once step 4's
acceptance runs pass.

## 5. Acceptance, in one list

1. `hinsight` analyses its own modules.
2. `hinsight-tadka`'s modules analyse with `tadka` resolved from the cabal store.
3. A plan from a different compiler is a typed error, not a wrong answer.
4. A project `-Werror` does not change the count of unexplained errors.
5. Nothing is printed by GHC or cabal during an analysis; their output is kept
   and reported as data.
6. `weeder`, `hlint`, `ormolu` and `-Werror` stay green, with every new export
   used.

## 6. Risks

- **I cannot run GHC 9.10.3 or cabal where I write the code.** As in earlier
  phases, every GHC-facing and cabal-facing step is written from source and
  confirmed by your runs. Pure steps are confirmed here.
- **Template Haskell may not work with a session that generates no code.** That
  is common to language servers and has a known cost; step 0 measures it.
- **Per-analysis cost rises** by a cabal invocation. This is why step 5 exists
  and why the fresh-session rule is not touched in this phase.
- **`hie-bios` may not support GHC 9.10.3**, in which case the adapter is
  ours. That is a decision for step 0, not a risk to the phase.
