# Decisions and deviations

Each entry records a rule that was relaxed or a choice that is not obvious,
what was tried first, and when to revisit it (Standard 5.9).

## 1. A fresh session per analysis; no package resolution

Each analysis starts its own GHC session, finds the file's source folder from
its header and place, typechecks the modules the file imports from its own
project, and then analyses the file. Nothing is carried between analyses, which
removes the stale-dependency class of bugs by construction (`docs/prior-art.md`,
classes 1 and 3) at the cost of repeating the work each time. Packages are not
resolved: imports from outside the project must be installed. Revisit the
session lifetime when the analysis benchmarks in item 6 show the cost matters, and
package resolution when the library is used on projects with their own
dependencies.

## 2. A mutable cell to receive hole fits

GHC renders the supplementary part of a hole error into a document before it
stores the diagnostic (`errInfoSupplementary :: SDoc` in GHC 9.10.3), so valid
fits cannot be read from the error. The hole-fit plugin hook receives them
structured, and the plugin runs inside GHC's monad. The only way out is a
reference the caller creates. Standard 1.8 and 1.9 prefer no mutable state;
the cell is created per session, written only by the plugin, read once after
the session, and never shared. Revisit if GHC exposes structured fits in the
diagnostic.

## 3. Hole fits that GHC already turned into documents are skipped

`RawHoleFit` carries no structure to rank, and GHC's own search does not
produce it. Another plugin could. They are skipped rather than ranked.
Revisit if a real case appears.

## 4. Context comes from the parsed source, not from GHC's origin

GHC's origin for a type-equality mismatch only restates the two types
("arising from a type equality Int ~ String"), so it is not used. Instead the
top-level binding around the error is found in the parsed module, and its
signature, if any, is reported as a fact about the source ("in greeting,
declared at 3:1 as String"), never as the cause of the mismatch. A binding
with no signature is reported as such. Only top-level function and variable
bindings are considered; an error inside an instance method or a pattern
binding gets no context. Revisit when project loading is added, and consider
GHC's own "In the expression" context lines as a further source.

## 5. Record fields carry a type prefix

Fields are named `explanationSpan`, `fitName` and so on, instead of using
`DuplicateRecordFields` with `OverloadedRecordDot`. Plain selectors compose
with `map` and `traverse` and avoid ambiguous-update warnings. Revisit if
dot syntax is adopted across the wider code base.

## 6. Benchmarks: one machine-independent threshold, the rest reported

`cabal bench` (tasty-bench) measures `rankFits` against the plain `sortOn` it
wraps at 1,000, 10,000 and 100,000 fits, rendering of an analysis at 100 to
10,000 holes, and two real analyses (one module; a module with two project
imports). CI fails when `rankFits` is more than 100% slower than the sort it
wraps; that ratio holds on any machine. The rendering and analysis timings are
printed but have no threshold, because their absolute times depend on the
machine; a threshold needs a stored baseline from a fixed runner, which does not
exist yet. The analysis benchmarks are skipped when no GHC 9.10.3 is found. The cost of
capturing a project environment is a separate benchmark in `hinsight-cabal`
(`hinsight-cabal-bench`); it is reported, not thresholded, and not run in CI
because each capture starts a cabal process. CI's benchmark step names
`hinsight-bench`.
The library-directory lookup used to be written three times (demo, tests,
benchmark); a fourth copy was due with the Tadka demo, so it now lives in the
public sub-library `hinsight-toolchain` (`HInsight.Toolchain`). An empty
`HINSIGHT_GHC_LIBDIR` counts as unset.

## 7. Dead-code check is enforced, with a guard against an empty report

`cabal.project.freeze` is committed. CI runs weeder and fails on any weed. The
first real report was empty, and a deliberately unused function was reported
by weeder before it was removed, so the check is known to see dead code.
Weeder passes silently when it has no `.hie` files to read, and a fully cached
build can leave none, so the step fails when `.hie` holds no files, and `.hie`
is cached with the build it belongs to. The roots in `weeder.toml` are the
`main` functions only; revisit them if a library export that only a consumer
outside this repository uses starts to be reported.

## 8. Prior-art review not done (Standard 5.7)

A first review for project loading is in `docs/prior-art.md`: nine classes,
each with the regression test that ships with the loader. It read two sources
in full and relies on experience for the rest, so a deeper pass over the
`ghcide` and `hie-bios` issue trackers is still owed. Hole-fit tooling has not
been reviewed.

## 9. Ranking uses fit and locality only

The module document names a third signal, usage. Nothing in the first
extraction could supply it, so it was not in the type. It is now: a fit carries
an optional use count (item 16), shown beside the fit. The order GHC's data
gives (fewer new holes, local before imported, fewer new hole types) is
unchanged; the count is the last field of the sort key, so it only separates
fits that are otherwise equal.

## 10. Type flow walks the typechecked tree generically

GHC rewrites `a op b` into `(op) a b` inside an expansion node while
typechecking, and keeps the original only as renamer output. `HInsight.Ghc.Flow`
therefore finds chains by walking the typechecked tree with `Data.Data`
(`gmapQ`), recognising the expansion of an infix application whose operator is
`.`, `$` or `>>=` defined in base (module `GHC.Internal.Base` in 9.10), and reading each stage's type with
`hsExprType`. A generic walk keeps the module independent of the many
expression constructors, at the price of visiting every node. Limits: infix
uses only, three operators. A file with errors is typechecked a second time
with the errors deferred, only to read its pipelines; the second pass's
diagnostics and holes are discarded, so the first pass stays the source of
truth. It costs a second typecheck of the file when there are errors. The module was written from
GHC 9.10.3's source and is checked by the real-GHC test in
`GhcSessionSpec`, not by the pure test run.

## 11. The module graph follows ordinary imports only

The graph is read from the module summaries GHC produces while chasing the
file's imports. It follows ordinary imports and leaves out `{-# SOURCE #-}`
imports, so a cycle that an `hs-boot` file legally breaks is not reported as a
cycle; `hs-boot` files are not modules of their own in the graph. Imports of
modules outside the project are dropped, and a module that imports itself is
reported as a cycle. The graph always contains the analysed file, so even a
file with no imports has a one-module graph; the report shows the graph only
when some module imports another. When the file's imports cannot be resolved
the graph is empty. Revisit when package-level dependencies are wanted: those
are not in this graph.

## 12. Tadka is a separate package, and GHC positions are converted, not copied

The Tadka demo is the package `hinsight-tadka` in this repository, not a
module of `hinsight`. hinsight stays free of any renderer, so a consumer that
draws differently pays for nothing it does not use. Both packages build in one
project and are checked by the same CI.

Positions need a conversion. GHC gives a line and a column, and a tab advances
the column to the next multiple of eight plus one (`advance_tabstop` in
`GHC.Types.SrcLoc`). Tadka locates a span by the character offset of its first
character and its length. Tadka's own interop module for GHC treats a column
as a character index, which is wrong after a tab, so `HInsight.Tadka.Locate`
does the conversion itself and is tested with GHC's counting written
independently. A span that is not a position of the text (the file changed
after it was analysed) is an error, not a guess.

Tadka's `help` is one sentence and the graphical renderer flattens line
breaks, so a list (hole fits, pipeline stages) is drawn as one related note per
item. Pipeline stages are not yet labelled in the source, because `Stage`
carries no span; the whole chain is. Add a span to `Stage` when per-stage
labels are wanted. The tests pin one narratable rendering, so a Tadka release
that rewords it fails a test instead of changing output unnoticed; the package
bounds are `tadka >=2.0 && <2.1`.

## 13. Environment capture is delegated to hie-bios, in a separate package

Decided in step 0 of the project-environment phase
(`docs/phase-environment.md`, `docs/prior-art.md` classes 10 to 22). Capturing a
component's flags from cabal has needed a fix in `hie-bios` for nearly every
cabal release, so `hinsight` does not write it. The dependency (about two dozen
packages) sits in an adapter package, so `hinsight` core keeps its small
dependency set and the handle `EnvironmentSource` stays open to other build
tools. Status: the package `hinsight-cabal` is written (step 3); whether
`hie-bios` builds against GHC 9.10.3 is verified only when it is first built.
Revisit if it does not. Only a cabal project is understood: the project is
found by cabal's own rule and a `hie.yaml` or stack project is not consulted.

## 14. Environment flags go through GHC's own parser, and ours are applied last

Decided in step 2 of the project-environment phase. The flags of a project
component are parsed by `parseDynamicFlags`, so each means what it means to
`ghc`; a refused flag or an argument taken for a file name is a typed
`EnvironmentFailure`, never ignored. `-working-dir` carries the root, so no path
is rewritten and the process's directory is never changed; `-package-env=-`
keeps `.ghc.environment` files out. `noBackend` and `NoLink` are set after the
flags. `-Werror` and the `-fdefer-*` flags are dropped (and reported by
`sessionFlags`) because they change which errors there are to explain. Status:
the GHC-facing parts are written from the 9.10.3 source and verified only when
the real-GHC tests run.

## 15. The environment handle is in core; the cabal demo is in the adapter

Decided in step 4 of the project-environment phase. `EnvironmentSource` is a
record of functions in `HInsight.Environment` (open axis, Standard 0.2), with a
consumer (`ghcInsightWith`) and a producer (`cabalSource`) in the same change
(Standard 0.7). The demo that takes `--environment cabal|header` is an
executable of `hinsight-cabal`, because the dependency points inward (2.6):
core cannot depend on the adapter. An analysis carries a summary of its
environment (`analysisEnvironment`), including the flags left out of the
session, so a report never hides that `-Werror` or a defer flag was ignored.

## 16. `hinsight` presents what GHC and its tools say, and never infers

Decided after reading GHC 9.10.3's source for the next phase
(`docs/feasibility.md`). Formatting, filtering, ordering and parsing the output
of GHC and of existing tools is presentation and is in scope. Reconstructing a
cause, ranking by a measure of our own, or completing what a tool left out is
not, and neither is writing edit machinery that an existing tool already
provides. Where a piece cannot be read, the result is a typed refusal that
carries the raw output. Features whose only source would be our own theory
(space-leak findings) are dropped; features whose source is unconfirmed wait
for a spike.

Amended after the refactoring work. A ranking is presentation when its key is a
fixed lexicographic order over facts the result also shows. It stays forbidden
when the key mixes facts with weights, learns from the user, or hides a fact. A
count that hinsight makes itself is allowed only when it is exact, is shown, and
is labelled as hinsight's, never as GHC's statement: the use count of a hole fit
is the number of occurrences of the name in the expressions of the analysed
file's typechecked source (variables, constructors and record selectors), and
the report says "counted by hinsight". Names are told apart by module and
occurrence, so a local `lookup` and an imported one are separate; only
module-level names are counted, a local name has no count. The count reaches the
order only as the last tie-breaker.

## 17. Chains, splices and Core are one handle apart from the analysis

Decided across steps 5 to 7 of the explanation phase
(`docs/phase-explanation.md`, "Decisions taken while building", items 5 to 23).
`Deep m` is a record of three functions (`chainsFor`, `splicesFor`, `coreOf`),
separate from `Insight m`, so an ordinary analysis keeps its meaning and its
cost. Each reads a message GHC logs when a flag is on (`-ddump-tc-trace`,
`-ddump-splices`, `-ddump-simpl` and friends), in a fresh session with the
flags set after the project is loaded. What GHC printed is carried raw beside
what was read from it; anything that could not be read is counted, never
dropped. A file GHC cannot get past (a parse error, an unresolved import) is
reported as blocked, and a failure of the step itself is a typed error with
GHC's message, never folded into the blocked count.

## 18. An unrecognised hint is shown in GHC's own words

Decided in step 3 (`docs/phase-explanation.md`, decision 1; open decision 2 of
section 3.9). `HintKind` has a constructor for each hint this package knows and
`OtherHint` for the rest, and a hint's text is what GHC prints for it. The
alternative, refusing a hint it does not know, would hide something GHC said,
which item 16 forbids. The cost is that the text of an `OtherHint` is GHC's
wording and can change with a GHC release; the bump procedure refreshes the
fixtures.

## 19. STG on request, and no claim about what the optimiser did to dependencies

The Core mode offers the simplifier's input and output, the specialisations and
the rule firings. STG was first left out, because it needs a backend that writes
an object file. It is now offered on request (`CoreAndStg`, `--core --stg` in
the demo): the file and the project modules it imports are compiled to object
code in a temporary folder that is removed, which costs several times a
typecheck, and the documentation says so. At `-O` the Core mode also lists the
missed-specialisation warnings GHC itself gives for the file
(`-Wmissed-specialisations` and `-Wall-missed-specialisations`), classified by
the warning's flag and never by its text. Other kinds of missed optimisation
have no GHC warning and are not offered. Only the analysed file is simplified;
its dependencies are typechecked, so a rule that needs one of their unfoldings
may not fire, and the documentation says so (`docs/phase-explanation.md`,
decision 17).

## 20. Refactorings are verified plans, built from spans GHC reports

`Refactor m` (`extractFunction`, `moveDefinition`, `changeType`) is a record of
functions apart from `Insight` and `Deep`, because it proposes changes to
source. No operation writes a file: each returns a `VerifiedPlan`, a set of edits
that GHC accepted when it checked the result with the edited text supplied as
in-memory targets, plus notes in plain words. `VerifiedPlan` can be built only in
`HInsight.Refactor.Verify`, so a plan that was not checked is not a value. The
gate is pure over a record of checkers, so it is tested without GHC.

Every edit is a replacement of a span GHC reports, so `HInsight.Edit` is the
whole editing layer. The spike (HLS 2.14.0.0, GHC 9.10.3) found no extract or
move action in HLS, no buildable `ghc-exactprint` library for 9.10.3, `retrie`
failing without `--allow-newer`, and `apply-refact` applying hlint hints only,
so no exact-printing dependency was added. A plan proves that the files still
typecheck, not that behaviour is preserved. Extracting an expression can lose
sharing of a shared subexpression, and the documentation says so.

## 21. ChangeType lets GHC name the places that still need a change

The plan edits the signature, or adds one. GHC then checks the project files the
caller lists. The unchanged files must compile first (strict check). The check
after the edit defers type errors, typed holes and out-of-scope variables, so GHC
compiles the dependents as well and reports each place as a warning instead of
stopping at the first failing module. An error in the changed file refuses the
plan with GHC's message. An error in another file is a note, "GHC reports a place
still to fix", with its position and GHC's text. Nothing in those files is
edited.

## 22. MoveDefinition: GHC decides the imports, and a cycle is refused

The definition moves with its signature and the Haddock comment directly above
it; other comments stay. The target gets the source's import declarations
verbatim; hinsight then removes only those that GHC's `-Wunused-imports` flags in
the target, only when GHC still accepts both files without them, and never one
that was already unused before the move. A source module with an explicit export
list that names the definition imports it from the target and re-exports it, so
importers keep compiling. A definition that uses a name defined in its own
module is refused with the names, since the move would need an import cycle.
Modules beyond the two are listed by the caller; what GHC reports in them is in
the notes. Text moved into a file takes that file's line ending.

## 23. One static binary is possible; no helper process yet

A program that links the `ghc` library, parses flags and typechecks a module
links and runs as a dynamic, a static-Haskell and a fully static binary on glibc
(76 to 78 MB, 79 to 98 ms cold start and typecheck), and a Template Haskell splice
typechecks from each. Not tested: a musl toolchain and a plugin loaded from the
static binary. The program still reads the compiler's library folder at run time.
The helper-process design (protocol, server and client packages) is therefore not
built. `scripts/spike-link.sh` repeats the test.

## Checks that run on every push

Build with `-Werror`, tests (including those that run a real GHC 9.10.3),
HLint with the standard's bans as errors, Ormolu 0.9.0.0 in check mode, a
no-mention-of-the-application check, and weeder. Tool versions are
pinned in the workflow's `env` block. Dependabot proposes updates to the
actions weekly.
