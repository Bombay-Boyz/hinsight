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
exist yet. The analysis benchmarks are skipped when no GHC 9.10.3 is found.
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

The module document names a third signal, usage. Nothing in the current
extraction can supply it, so it is not in the type. Add it in the change that
makes it available.

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
tools. Status: **not yet built**; whether `hie-bios` builds against GHC 9.10.3 is
unverified. Revisit if it does not.

## Checks that run on every push

Build with `-Werror`, tests (including those that run a real GHC 9.10.3),
HLint with the standard's bans as errors, Ormolu 0.9.0.0 in check mode, a
no-mention-of-the-application check, and weeder. Tool versions are
pinned in the workflow's `env` block. Dependabot proposes updates to the
actions weekly.
