# Next phase: what GHC says, presented in depth

Status: **planned, not started.** Nothing in this document is built. It follows
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

### 3.9 Open decisions

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

1. The rigid-variable example is explained (`rigidVariableIsExplained`).
2. For the ten spike programs the result has the expected kind, or the
   diagnostic is counted unexplained; none is dropped (the count identity).
3. The chain for the `Pair` program equals its golden sequence and ends at a
   deferral with the error's span.
4. The standard path's cost is unchanged: the existing benchmark shows no
   regression, and the trace pass is not entered unless requested.
5. A module with more records than the limit gives `TraceLimitExceeded`, not a
   truncated chain.
6. An unreadable record is carried raw and counted.
7. Nothing is printed by GHC during an analysis; nothing is left on disk after
   the Core mode.
8. `weeder`, `hlint`, `ormolu` and `-Werror` stay green, with every new export
   used, and the repository never mentions the application that uses it.

## 6. Size

An estimate, to be replaced by the measured figure when the phase closes:

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
- **Volume.** A large module produces tens of megabytes of trace. The hook
  bounds memory but not the rendering time.
- **Kind-level noise** may not be separable by form alone. If not, the default
  view shows everything in the binding with kind-level lines marked, not hidden.
- **`-dppr-debug` is session-wide.** Confined to the second pass, whose
  diagnostics are discarded.
