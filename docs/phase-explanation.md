# Next phase: what GHC says, presented in depth

Status: **built (steps 0 to 7); closed in step 8.** The acceptance list in
section 5 says what was checked and how, and what was not. It follows
the engineering standard: prior art first (5.7), one canonical representation
(2.1), effects behind a handle (0.5), a named test for every claim (5.5), and a
bound on anything that grows with the input (2.4, 4.11). Every capability named
here was checked against GHC 9.10.3 in `docs/feasibility.md`; where a point was
not checked, it says so.

## 1. The gap

`hinsight` explains one kind of GHC diagnostic: a type mismatch that arrives as
`Mismatch`. Everything else GHC reports is counted as unexplained
(`HInsight.Ghc.Messages.classify`, the `_ -> Unexplained` branches). The spikes
showed what is available and not yet shown:

| What GHC provides | Where | Today |
|---|---|---|
| A rigid type variable that cannot be unified (`h :: a -> Int; h x = x`) | `CannotUnifyVariable`, with a `MismatchMsg` of the same type as `Mismatch` | Counted as unexplained |
| An unresolved class constraint, with the instances that could unify | `CannotResolveInstance` | Counted as unexplained |
| Typed hints: similar names, a missing extension, a missing `do` | `diagnosticHints`, `GhcHint` | Not shown |
| The unification steps GHC took, with spans | `-ddump-tc-trace -dppr-debug -ferror-spans`, one log message per record | Not shown |
| What a Template Haskell splice expanded to | `-ddump-splices`, one line per splice with its span | Not shown |
| Core, specialisation, rule firings, STG | `-ddump-simpl/-spec/-rule-firings/-stg-final` in a compile mode | Not shown |

## 2. Goal and non-goals

**Goal.** For every diagnostic GHC gives for a file, present what GHC says
about it as typed data, and say explicitly when it says nothing we can present.
Nothing is inferred (`docs/decisions.md`, item 16).

**Not in this phase:**

- **Refactoring** (extract function, move definition, change a type and
  propagate). No existing tool provides these to present.
- **Space-leak or missed-optimisation findings.** GHC reports none.
- **Ranking holes by usage.** The data is not in GHC's hole fits, and a measure
  of our own is what the rule forbids.
- **Import suggestions.** GHC does not search packages (`sort` without an
  import got a similar-name hint only). Another tool owns that.
- **Applying edits.** Hints carry no replacement text. We show them.
- A long-lived session and the environment cache (still not demanded by
  anything).

## 3. Design

### 3.1 One representation, and nothing dropped

Today the result of classifying one GHC error is explained, a typed hole seen by
the plugin, or unexplained. This phase widens the first case into one closed sum
of **evidence**, one constructor per kind of thing GHC said:

- a type mismatch (exists; shares its parts with the rigid-variable case);
- an unresolved instance: the constraint and the instances that could unify;
- the diagnostic's hints, attached to whichever diagnostic they came with.

Each carries the diagnostic's span and GHC's own rendering of anything we do not
model (a constraint, an instance), as text. A diagnostic that has no evidence
constructor stays *counted as unexplained*, as now. The invariant, tested as a
property: **explained plus unexplained plus holes equals the number of errors
GHC reported.** The exact fields are fixed in step 1 after reading
`HInsight.Explanation`, not here.

### 3.2 The heavy passes are a second handle

Unification chains, splice expansion and the Core mode each cost a second pass
over the file (measured: the trace pass is about 4.5 times a typecheck of the
target). They are requested, never default. They live in a separate record of
functions, not in `Insight`:

```haskell
data Deep m = Deep
  { chainsFor :: SourceFile -> m (Either InsightError [Chain]),
    splicesIn :: SourceFile -> m (Either InsightError [Splice]),
    coreOf    :: SourceFile -> Optimisation -> m (Either InsightError CoreDump)
  }
```

`Insight` keeps its meaning and cost. This is interface segregation (0.4) and
keeps `Insight`'s version stable (2.13). The names are working names. Each field
is added in the step that makes it reachable (1.5, 0.7).

### 3.3 Capturing the trace

The trace pass reuses the shape of `deferredPipelines`: set the session flags,
`depanal`, find the target's summary, `parseModule`, `typecheckModule` that
module only. A log hook receives each trace record as its own message, so no
boundary parsing is needed. The hook keeps a record only if its span names the
target file and lies inside the binding that holds the error; everything else is
dropped on arrival. This bounds memory (4.11) at the cost of rendering every
record, which is the measured price. A limit on kept records is a typed error
(`TraceLimitExceeded`), never silent truncation (2.4).

Setting `-dppr-debug` changes how GHC prints everything, so the trace pass never
supplies diagnostics: those come from the normal first pass.

### 3.4 Reading a record

A record is `file:span::` and then a heading and indented lines. The pure parser
produces a `TraceRecord` (span, heading, body). A closed `Step` sum names the
headings the chain uses (a unification, a type variable filled, a deferral);
anything else is kept as the raw record. A record that does not parse is kept
raw and counted, never invented (typed refusal with the raw lines).

The cleaner from the spike (`{(w) tc 3u}` decorations, `(a{tv} Nothing [tau:0] ::
k)` binders, module qualifiers unless two modules would collide) is a *view*:
the raw line is always carried beside it, and a shape it does not recognise is
reported as residue.

### 3.5 Which records form a chain

For an equality mismatch the chain ends at the `utype_defer` record whose span
equals the error's span (true in all three cases run). Two views over the same
records, both deterministic:

1. **Following the variable (default).** Start from the type variables named
   in the deferral. Add every record in the enclosing binding that mentions one
   of them; add the variables of those records; repeat until nothing is added
   (a fixed point, `until`, 4.1). Emission order is kept.
2. **Every step in the binding.** All records in the enclosing binding, minus
   kind-level steps.

How to hide kind-level steps without guessing is an open question (3.9): the
9.10.3 trace has no label for them, only their printed form (`Type ~ Type`).

### 3.6 Class constraints and hints

`CannotResolveInstance` is presented as the constraint and the instances that
could unify, rendered by GHC, with their count. The runs showed lists of 31, 37
and 41; a list is shown in full or with an explicit "n more", never cut
silently. Hints are typed `GhcHint` values; the ones we recognise
(`SuggestSimilarNames`, `SuggestExtension`, `SuggestMissingDo`, and the others
named in `docs/feasibility.md`) become constructors of a closed `Suggestion`
sum. Any other hint is carried as GHC's own rendering. That fallback is the one
deliberate relaxation of 1.5 and is recorded as an ADR in `docs/decisions.md`
(5.9): it is presentation of GHC's text, not an error vocabulary.

### 3.7 Splices and Core

Splices come from the `-ddump-splices` lines, parsed by span and the
`======>` separator. The spike printed only expression splices, so declaration
splices are examined before the parser is written (step 6).

The Core mode compiles the target with `-fno-code -fwrite-if-simplified-core`
(Tidy Core, specialisation, rule firings) or the bytecode backend (adds STG),
writing into a temporary directory that is created and removed with the
resource-safe `bracket` pattern, leaving nothing behind. Measured cost against a
typecheck: 1.1 to 1.2 times at `-O0` (unoptimised, so no rule firings of
interest), 3.4 to 4.4 times at `-O`, 6 to 9 times with the dumps written, about
5 times for bytecode. It is asked for per file. Whether compiling the dependencies
at `-O0` and only the target at `-O` is cheaper is not measured (step 7).

### 3.8 Errors

`InsightError` gains constructors in the change that reaches them (1.5, 2.15):
`TraceLimitExceeded`, a trace that cannot be read (with the raw lines), a dump
section that is missing, and a failed temporary-directory setup. Each message
names the file and span.

### 3.9 Open decisions (resolved while building)

Resolved: 1 by item 5 of the decisions below, 2 by item 1 (and
`docs/decisions.md`, item 18), 4 by item 6. Open: 3, whose default is
`defaultRecordLimit` = 100 000 until the trace benchmark has been run on a large
module and its figure recorded. The original text follows.

1. **Handle shape.** `Deep` as a second record (recommended) or new fields on
   `Insight`.
2. **The hint fallback** (3.6): allow GHC's own rendering for unrecognised
   hints, recorded as an ADR, or refuse unrecognised hints.
3. **Limits.** The default record limit and the default list length for
   instances. Set from the benchmark in step 5, not guessed.
4. **Kind-level steps.** Hide by exact printed form, by heading, or show all
   with a marker. Decided from fixtures in step 4.

## 4. Steps, in order

Each step ends with the verification used so far: formatted, linted, built with
`-Werror`, pure tests run here, GHC-facing parts run on your machine.

**Step 0 — prior art (5.7).** Extend `docs/prior-art.md` with named classes,
each with a regression test planned, before any code. Classes to find or
confirm: a trace record with no span; multi-line spans (`(l,c)-(l,c)`); `LINE`
pragmas and CPP shifting spans; a module with Template Haskell; several errors
in one binding; an error in a `where` or a mutually recursive group; an error
with no deferral (a parse error, an out-of-scope name); a trace far larger than
the limit; type variables with the same printed name in different bindings; a
splice that expands to declarations; a class error with overlapping instances.
The issue trackers could not be reached from where the plan was written, so the
list comes from GHC's source, the spikes and general knowledge, and is marked
accordingly.

**Step 1 — the evidence sum and the rigid variable.** Read
`HInsight.Explanation`; fix the evidence sum; explain `CannotUnifyVariable`
through the same parts as `Mismatch`. This closes a gap in what is already
built. Tests: `rigidVariableIsExplained`; the count identity property;
`mismatchStillExplained`.

**Step 2 — unresolved instances.** `CannotResolveInstance` as evidence. Tests:
`foldableAmbiguityListsInstances`, `noInstanceShowsConstraint`, a property that
the printed count equals the list length, and an explicit "n more" at the cap.

**Step 3 — hints.** Attach typed hints to any diagnostic. Tests:
`misspeltNameHasSimilarNameHint`, `missingExtensionHasExtensionHint`,
`missingDoHasMissingDoHint`, and `missingImportIsNotInvented`, which asserts
that `sort` without an import yields no import suggestion.

**Step 4 — the trace reader (pure).** `HInsight.Trace`: the record parser, the
`Step` sum and the cleaner, tested on fixtures taken from real 9.10.3 traces
(the lines the spikes printed, and more from larger modules). Properties: the
parser is total; the cleaner is idempotent and never longer than its input;
every unrecognised shape is reported as residue; a 100,000-record input runs in
bounded memory (4.10). Decides open question 4.

**Step 5 — chains.** The GHC edge (second pass, hook, filter, limit) and the two
views. Acceptance on the spike programs: `chainOfPairMismatch` (the golden
sequence, ending at a deferral whose span equals the error's),
`chainOfRigidVariable`, `noChainForAParseError`. A benchmark records the trace
pass's wall time beside the normal pass, not thresholded (decisions item 6).

**Step 6 — splices.** First extend the spike to declaration splices and nested
splices, then write the parser. Tests: one per splice kind seen.

**Step 7 — the Core mode.** The temporary-directory lifecycle, the dump section
reader, and the `-O0`-dependencies measurement. Tests: `coreModeLeavesNoFiles`
(directory listing before and after), `ruleFiringsAreListed` on the spike's
`Spec.hs`.

**Step 8 — documentation and close.** `docs/decisions.md` entries, README
status, and the acceptance list below ticked with how each was checked.

## 5. Acceptance

Ticked only where a test or a run checked it; the rest say what is missing.

1. [x] The rigid-variable example is explained. Checked by `EvidenceSpec`
   ("explains a mismatch against a rigid type variable", on
   `test/fixtures/RigidVariable.hs`, real GHC 9.10.3).
2. [x] For the spike programs the result has the expected kind, or the
   diagnostic is counted unexplained; none is dropped. Checked by `EvidenceSpec`
   ("accounts for every error GHC reports"): eight fixtures, each with the
   number of errors GHC reports equal to explained + unexplained. The list has
   eight programs, not the ten the plan named; the plan's ten were not kept as a
   list, so this is the set the fixtures cover, not a claim about the ten.
3. [x] The chain for the `Pair` program equals its golden sequence and ends at a
   deferral with the error's span. Checked by `ChainSpec` (golden sequence from
   real lines, at both views) and `ChainGhcSpec` (the same program through real
   GHC, and inside a project environment).
4. [~] The standard path's cost is unchanged. By construction: the trace, splice
   and Core passes are the separate handle `Deep` and are never entered by
   `analyseFile`. Measured after the phase (`hinsight-bench`, the user's
   machine): a plain analysis takes 18.3 ms (one module), 26.7 ms (with two
   project imports) and 22.4 ms (one mismatch); the same mismatch's trace pass
   38.8 ms; a module with eleven splices 80.5 ms plain and 86.9 ms for the splice
   pass; a module with a rule 29.7 ms plain, 71.4 ms for optimised Core and
   37.8 ms for unoptimised Core. These are well under the analysis time recorded
   before the phase (under 100 ms, `docs/phase-environment.md`), but there is no
   like-for-like figure from before the phase, so "no regression" is not claimed
   from numbers. CI thresholds only the ranking benchmark.
5. [x] A module with more records than the limit gives `TraceLimitExceeded`, not
   a truncated chain. Checked by `TraceSpec` ("gives an error, not a truncated
   trace, past the limit", a property, and a 100 000-record input).
6. [x] An unreadable record is carried raw and counted. Checked by `ChainSpec`
   ("counts an uninterpreted record that came before the deferral"),
   `SpliceSpec` (`splicesUnread`) and `CoreSpec` (`coreUnread`, and the
   totality property).
7. [~] Nothing is left on disk after the Core mode: checked by `CoreGhcSpec`
   (`coreModeLeavesNoFiles`: fixture folder, working folder and temporary folder
   listed before and after). "Nothing is printed by GHC during an analysis" has
   no test; GHC's output goes through the log hook, and nothing was seen on the
   terminal in any run, but that is an observation, not a check.
8. [x] `weeder`, `hlint`, `ormolu` and `-Werror` were green in CI on steps 7 and
   8 (read by the user). Every new export is used (weeder), and the
   independence check (the repository never names the application that uses it)
   is part of CI.

### Owed after the phase

- Done after the phase: the scope-error hypothesis was checked and is true for
  scope errors only (decision 24); hole types and fits now print in the user
  style (decision 25).
- Done after the phase: `defaultRecordLimit` was checked against measurements
  and kept at 100 000 (decision 9); the analysis benchmarks were run and are
  recorded under acceptance 4.
- Keep only the records of the binding with the error, so that a module larger
  than about 900 lines can have chains (decision 9). It needs the error's span
  before the trace is captured, which the second pass does not have.
- For a like-for-like acceptance 4: check out the commit before the phase, run
  `cabal bench hinsight-bench`, and compare the plain analysis entries.
- STG is not offered (decision 20).

## 6. Size

An estimate, kept as planned. Measured at the close: the library's modules are
about 4,500 lines (comments included) and the test modules about 2,600, so the
estimate of about 4,400 source lines for the whole package held, and the tests
came in above the estimate of about 1,750 for the phase. The split by part was
not measured.

| Part | Source lines | Test lines |
|---|---|---|
| Evidence sum, rigid variables, instances, hints | 450 | 500 |
| Trace reader and cleaner | 300 | 350 |
| Chains (edge and views) | 500 | 450 |
| Splices | 200 | 150 |
| Core mode | 300 | 200 |
| Handle, errors, wiring | 150 | 100 |
| **Total** | **about 1,900** | **about 1,750** |

That is roughly 4,400 source lines for `hinsight` as a whole against about 2,500
now. The original estimate for the module was 6,650; the difference is the
refactoring and ranking work this phase does not do. Uncertainty is about 30
percent either way.

## 7. Risks

- **I cannot run GHC 9.10.3 where I write the code.** As before, every
  GHC-facing step is written from source and confirmed by your runs; pure steps
  are confirmed here.
- **The trace is not a contract.** Its headings are GHC's. A GHC bump means
  refreshing fixtures; the bump procedure covers it, and an unparsed record
  falls back to raw.
- **Volume.** A large module produces tens of megabytes of trace (24 MB for a
  121-line module under `-dppr-debug`; about 6 s of GHC time against 0.2 s
  without it, when written to a file). The hook bounds memory but not the
  rendering time.
- **Kind-level noise** may not be separable by form alone. If not, the default
  view shows everything in the binding with kind-level lines marked, not hidden.
- **`-dppr-debug` is session-wide.** Confined to the second pass, whose
  diagnostics are discarded.

## Decisions taken while building

These record choices made in the steps above that the plan left open.

1. **Hints are a closed kind plus GHC's wording (step 3).** `HintKind` has one
   constructor per hint GHC defines that this package knows, and `OtherHint` for
   the rest. The text is what GHC prints for the hint. The payload of a hint (the
   extension to enable, the name to use instead) is not taken apart, because no
   consumer needs it yet and the text already says it. Revisit when one does.
2. **Hints are additive.** A diagnostic keeps its classification (explained,
   unresolved, hole or unexplained); its hints are a separate list. The count of
   errors is unaffected.
3. **A hint with no source location is not reported.** Nothing can anchor it.
   This is rare and is stated in the documentation of `HInsight.Ghc.Messages`.
4. **Printed types use GHC's user style (step 1).** Type variables print as
   written (`a`), not with the unique suffix of the debug style. Hole types and
   fits used the debug style until after the phase; see decision 25.
5. **Chains are a separate handle (step 5).** `Deep m` is its own record, not a
   field of `Insight m`, so an ordinary analysis keeps its meaning and its cost.
   A field is added to `Deep` in the change that makes it reachable.
6. **Kind-level steps are marked, not hidden (steps 4 and 5).** The 9.10.3 trace
   has no label for them, so a step whose words all belong to GHC's kind
   vocabulary is flagged `[kind level]` and stays in both views. Hiding them
   would be a guess about what the reader wants.
7. **A chain ends at the first deferral at the error's span.** If GHC logged no
   deferral there (class-constraint errors have none), the error has no chain and
   is counted in `chainsMissing`; nothing is built from the neighbouring records.
   A file GHC cannot typecheck (parse error, unresolved import) yields no chains
   and counts its errors the same way.
8. **The trace pass reruns the module graph.** GHC typechecks a module with the
   flags stored in its summary, so `depanal` runs again after the trace flags are
   set. The first half of the analysis (placement, dependencies, target) is
   shared with the normal pass as `prepareProject`.
9. **The record limit is a typed error.** `defaultRecordLimit` is 100 000; past
   it the answer is `TraceFailure (TraceLimitExceeded n)`, never a shortened
   trace. Set from measurements after the phase (GHC 9.10.3, two real modules of
   this package with a type error added): a line produces about 110 records
   (121 lines gave 13,681 records and 226 lines gave 25,187), the chains pass
   peaked at 99 MB and 126 MB against 78 MB for a tiny module, so a kept record
   costs up to about 2 KB (an upper bound: GHC's own memory grows with the
   module too). 100 000 records is therefore about 200 MB and a module of about
   900 lines. The value was kept: raising it trades the memory bound for modules
   most people would still call large. The records of every top-level binding
   are kept, not only the binding with the error; keeping only the latter would
   cut the volume by the share of the file the error is not in, but the error's
   binding is not known while the trace is captured, so it is left as owed work.
10. **Unverified until the real build (step 5).** That GHC hands the trace to
    the log hook as a message class other than diagnostic or fatal, that the
    trace names the target as `ml_hs_file` spells it (compared after
    `normalise`), and that a rigid-variable mismatch has a deferral at its
    span. `ChainGhcSpec` checks the first two on the Pair program and accounts
    for every error of the third whether or not it has a chain.
11. **Splices are read from `-ddump-splices`, in a pass of their own (step 6).**
    `HInsight.Splice` reads the one message GHC logs per splice it ran: the
    location, the kind word (`expression`, `declarations`, `type`, `pattern`),
    the code of the splice and the code it produced. The spike covered every
    kind of splice and three widths; the readings agree at all of them. The pass
    costs about one typecheck, because it needs neither `-dppr-debug` nor the
    trace.
12. **What is reported is only what GHC printed.** A typed splice and a
    quasi-quote are printed as expressions and are reported as such. A splice
    nested in another is not a record of its own; GHC notes it on the outer one
    as `pending(rn) [...]`, and that note is carried as printed, not taken apart.
    A kind word not in the list is kept as it is.
13. **Splices are listed by position, not in GHC's order.** GHC runs a
    declaration group before the expressions in it, so its order is not the
    file's. The raw message is carried with each splice.
14. **A declaration splice's result keeps its lines.** Elsewhere the lines of a
    message are joined, because GHC wraps to the width it renders to. A
    declaration splice's result is a list of declarations, so the lines at the
    least indentation each begin one and deeper lines continue it. This is a
    reading of the layout, stated in the module's documentation.
15. **`splicesBlocked` and `splicesUnread`.** When GHC stops before it can run
    any splice (a parse error, an import it cannot find), the file has no splice
    dump at all; the errors are counted in `splicesBlocked` so that "no splices"
    is not mistaken for "could not look". A message that says `Splicing` with a
    shape the reader does not know is counted in `splicesUnread`, never dropped.
    Splices that ran before a type error are still reported.
16. **The Core mode reads GHC's dumps from the log, not from files (step 7).**
    The spike showed two ways: with `-ddump-simpl -ddump-spec
    -ddump-rule-firings` alone the dumps arrive as log messages; with
    `-ddump-to-file` they are written as `<dumpdir>/<source path>.dump-*` files.
    The log is used, because a rule firing carries no module (`Rule fired: name
    (module-defining-the-rule)`), so attributing firings to one file in a
    multi-module compile needs the Core of that file alone. `HInsight.Core` reads
    a section (a header of equals signs around a name, then a body kept as
    printed) and a rule firing (name and defining module); anything that looks
    like either and is not is counted as unread.
17. **Only the file is simplified.** The pass loads the file's dependencies as
    an analysis does (typechecked, not simplified), then asks GHC for the
    simplified Core of the file with the dumps on. The rule firings are therefore
    the file's own. The cost of this is that the file's optimiser cannot inline a
    dependency's unfoldings, because the dependencies were never simplified, so a
    rule that needs such an inlining may not fire. This is stated, not hidden.
18. **The `-O0`-dependencies question is answered by construction, and the
    measurement is a benchmark.** The spike's timings (0.14 to 0.19 seconds for
    every mode) are all startup, too small to tell the modes apart, so no
    figure is claimed. `hinsight-bench` has entries for the optimised and
    unoptimised Core of a module and of a module with a project import; they are
    reported, not thresholded.
19. **A temporary folder is made and removed anyway.** GHC is told to write
    interface, object and dump files into a folder created with
    `withSystemTempDirectory`, which is removed however the call ends, so that
    nothing is left on disk even if GHC writes something. `coreModeLeavesNoFiles`
    lists the fixture folder, the working folder and the temporary folder before
    and after.
20. **No STG.** STG needs the bytecode backend, which writes an object file and
    costs a compile (the spike: `Final STG:` appears only there). It is left out
    of this phase rather than half-offered; the reader already accepts any
    section, so adding it later changes only which flags are set.
21. **No Core for a file that does not typecheck.** The errors are counted in
    `coreBlocked`; no section is shown, because GHC made none.
22. **A failed Core step is not "blocked" (step 7).** The Core pass typechecks the
    file itself first, with the Core flags set. If that fails, the errors are
    `coreBlocked`. If the file typechecks and GHC then fails to make its Core, the
    answer is `SessionFailure (CoreFailed file message)` carrying what GHC
    reported. Counting every failure as blocked had hidden a real defect (the
    target given twice), and a test that passed for the wrong reason.
23. **The targets are cleared before GHC makes Core.** `compileToCoreSimplified`
    adds the file as a target itself; a file given twice is an error ("module is
    defined in multiple files"). The pass therefore calls `setTargets []` first.
24. **A scope error hides every type error in the file; a typed hole hides none.**
    Checked with GHC 9.10.3 on files with a scope error in one binding and a
    mismatch in another: GHC reported only the scope errors, because it stops
    after the renamer. A hole is reported by the typechecker, so a mismatch next
    to it is reported too. The explanation therefore reports no mismatch for a
    file with a scope error, and says so by counting the scope error as
    unexplained; `EvidenceSpec` pins both cases.
25. **Hole types and fits print in the user style.** A rigid type variable
    printed as `a_aYt[sk:1]` in a hole's type and in the fit `k`; GHC prints `a`.
    The hole rendering now uses the same style as the rest (decision 4), and
    `EvidenceSpec` checks that no printed hole type or fit carries a unique.
