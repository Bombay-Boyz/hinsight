# What GHC and its tools expose, and what `hinsight` may do with it

Status: **settled by reading GHC 9.10.3's source and by throwaway spikes run on
GHC 9.10.3** (one development machine, Linux). Where a claim was not run or
read, it says *unverified*. This document caps what the next phase may promise.

## The rule

`hinsight` presents what GHC and existing tools say. It does not infer what
they did not say. Formatting, filtering, ordering and parsing their text or
structured output is presentation and is allowed. Reconstructing causes,
ranking by a measure of its own, or filling a gap with a guess is not, and
neither is writing machinery that an existing tool already provides. When a
piece cannot be read, the answer is a typed refusal that carries the raw
output, never a best-effort sentence.

## Findings that were run on GHC 9.10.3

| Question | Result |
|---|---|
| Does GHC print the typechecker's steps? | Yes: `-ddump-tc-trace`, about 25 trace points in `GHC/Tc/Utils/Unify.hs` with fixed headings (`u_tys`, `found filled tyvar`, `uUnfilledVar2 ok`/`not ok`, `utype_defer`, ...). |
| Do the lines carry a source span? | Only with `-dppr-debug`. With `-ferror-spans` the span is a range (`Pair.hs:5:21-23`). 22,128 of 22,696 records carried one (97.5%). Those without are `writeMetaTyVar` lines, which are not needed. |
| Can a step be linked to the error? | For equality mismatches, yes: the `utype_defer` record's span equals the error's span (`5:21-23`, `5:7-9`, `3:10-20` in the three cases run), and its own text carries GHC's origin sentence ("In the second argument of ‘pair’, namely ‘"x"’"). |
| What does the chain look like? | `pair (1 :: Int) "x"` against `r :: [Int]`: at the whole call `a` is filled with `Int` (`uUnfilledVar2 ok`), the first argument finds it filled, the second argument's `String ~ a` finds it filled with `Int`, then `utype_defer`. A rigid variable shows `uUnfilledVar2 not ok` then `utype_defer`. |
| Is a mismatch an immediate failure? | No. GHC defers it (`utype_defer`, `utype_defer2`, a coercion variable) and the constraint solver reports it later. The chain therefore ends at the deferral, not at the report. |
| Class-constraint errors in the trace? | Not a unification chain. The unification steps at that line are trivial; the trace content is instance lookup (`matchClass`, `matchInstEnv`). The trace is not the right source for these (see the typed diagnostic below). |
| Can a log hook inside the session receive the trace? | Yes. `pushLogHookM` (already used by `Session.hs`) receives each trace record as its own message: 22,696 records for a 198-line module, no boundary parsing. |
| Cost | Tracing the whole session: 0.81 s against 0.12 s (198-line module, 4.25 MB of text), 1.86 s against 0.18 s for a module with two dependencies (57,198 records, 9.9 MB). Dependencies are traced too, and `load` with changed flags retraces them. |
| Tracing only the target | Works as a second pass shaped like `deferredPipelines` (set flags, `depanal`, `parseModule`, `typecheckModule` of the target): 20,059 records, all from the target, 0.66 s against 0.15 s for the normal pass. |
| Core, specialisation, rule firings | Nothing is printed under `-fno-code`. Under `-fno-code -fwrite-if-simplified-core` GHC prints Tidy Core, Specialise and rule firings and writes a `.hi` into the chosen directory. The bytecode backend adds Initial and Final STG and writes `.o` and `.hi`. Nothing was written next to the source. Cost of an optimising compile on a real module was **not** measured. |
| Template Haskell splices | `-ddump-splices` under `-fno-code` prints one line per splice with its span: `Th.hs:6:11-30: Splicing expression litE (integerL 42) ======> 42`. |
| Hole-fit plugin in our session | Already done: `installHolePlugin` attaches a static plugin and the hole capture is tested. `HoleFit` carries the identifier, type, refinement level, matches and documentation; there is no usage count. |
| Typed class-constraint diagnostics | Run on ten small programs. `length 5`, `show` of a type with no `Show`, `id == id` and `show (read "1")` all arrive as `CannotResolveInstance` with the constraint (`Foldable t0`, `Show T`, `Eq (a0 -> a0)`) and the instances that could unify, as typed values (31 for `Foldable t0`, 37 for `Read a0`, 41 for `Show a0`). The candidate list was empty in every case and no hints were attached. GHC's "Ambiguous type variable" wording is produced from this same shape, not from a separate constructor: `AmbiguityPreventsSolvingCt` did not appear. |
| A char where an `Int` is expected | `Mismatch` with `TypeEqMismatch`: differing types `Char` and `Int`, overall expected `Int`, overall actual `Char`. This is what the explainer already uses. |
| A rigid type variable (`h :: a -> Int; h x = x`) | Arrives as **`CannotUnifyVariable`, not `Mismatch`**. The explainer matches only `Mismatch`, so today this common error is counted as unexplained. `CannotUnifyVariable` carries a `mismatchMsg` of the same type plus a reason. Whether the existing extraction works on it unchanged was not run. |
| Typed hints on real errors | A misspelt name: `SuggestSimilarNames` ("Perhaps use `length' (imported from Prelude)"). `\case` without the extension: `SuggestExtension LambdaCase`. A missing `do`: `SuggestMissingDo`. **A name from a module that is not imported (`sort`): no import suggestion**; GHC offered only a similar name that is in scope (`sqrt`). GHC does not search packages for a missing import; that is a place for an existing tool to fill in, not for `hinsight` to guess. |
| Cleaner for printed types | `HInsight`-independent prototype, tested on twelve real 9.10.3 trace lines: strips decorations such as `{(w) tc 3u}`, turns `(a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)` into `a_ax0[tau:0]`, and drops a module qualifier unless two modules would then share a bare name. A shape it does not know (a coercion variable) is left as printed and reported as residue. Idempotent, never longer than its input. The raw line is always kept beside it. |
| Cost of a compile, wall seconds, one machine, dependencies included | `Environment.hs` (3 modules): typecheck only 0.21; simplified core at `-O0` 0.26; at `-O` 0.94; `-O` with simplifier, specialisation and rule dumps written to files 1.9; bytecode `-O` 1.35; bytecode with the STG dump 1.75. `Session.hs` (the large one): 1.06; 1.17; 3.63; 6.3; 5.2; 5.9. Dumps are 1.2 MB and 5 MB. The interface files go to a temporary directory (3 and 22 files). |

## Findings from reading source and documentation

| Question | Result |
|---|---|
| Unification steps as data? | No. A type-checker plugin sees constraints, not unification steps (`GHC/Tc/Plugin.hs`). |
| Are class-constraint errors available as typed data? | Yes, without any trace. `TcSolverReportMsg` (`GHC/Tc/Errors/Types.hs`) has `CannotResolveInstance` (the constraint, the instances that could unify, the candidate instances, import errors, typed hints, relevant bindings), `AmbiguityPreventsSolvingCt` (the constraint and its ambiguous type variables), `OverlappingInstances`, `UnsafeOverlap`, and `Mismatch` with type-variable, ambiguity and coercibility info. Read from source; *not yet built or run*. |
| Is the mismatch itself richer than the trace? | `TypeEqMismatch` carries the two types that differ, the overall expected and actual types, what thing it concerns, and whether two names are the same name from different modules. This is the end state; the trace is how GHC got there. |
| Are GHC's hints typed? | Yes. `GhcHint` (`GHC/Types/Hint.hs`) is a closed sum of about fifty constructors, among them `SuggestSimilarNames`, `ImportSuggestion` (could import from / unhide from), `SuggestExtension`, `SuggestAddTypeSignatures` (with the available bindings), `SuggestFillInWildcardConstraint`, `SuggestParentheses`, `SuggestMissingDo`, `SuggestLetInDo`, `SuggestBindToWildcard`. They are suggestions, not edits: none carries replacement text. Which errors carry which hints can only be learned by running. |
| `-fdiagnostics-as-json` | An envelope (version "1.0", GHC version, span, severity, code, rendered message and hints). The message body is rendered text. Through the API the typed message is already available, so it matters only for diagnostics from other processes. The schema file itself was not read. |
| HLS | Fully supports GHC 9.10.3, as a server. Its documented code actions include Change Type Signature, Add argument to function, Insert contents of Template Haskell splice, Add missing class methods, and Rename. **Extract function, move definition, inline and "change type and propagate" are not listed.** |
| `ghc-exactprint` | 1.10.0.0 is bounded to GHC 9.10 only. It prints and edits source; it is not a refactoring tool. Whether it builds with 9.10.3 is not confirmed. |
| `haddock-api` | The latest release is bounded to GHC 9.6. Not usable on 9.10.3. Haddock text already reaches us through `hfDoc` and the interface files. |
| `hoogle` | 5.0.18.4, no dependency on `ghc`. Its Hackage page lists an executable; a library module was not confirmed from the page. It needs a database built from Haddock output. *Unverified in detail.* |

## What this caps

- **Unification chains** are possible for equality mismatches, from a second
  pass with `-ddump-tc-trace -dppr-debug -ferror-spans` over the target only,
  filtered by span. Two views over the same GHC lines: the steps that mention
  the variable named in the deferral (default), and every non-kind step in the
  enclosing binding (complete). They end at the deferral and are never "the
  chain that caused the error". The printed types are noisy and need a
  fixture-tested cleaner with the raw line as fallback.
- **Class-constraint errors** come from the typed diagnostic
  (`CannotResolveInstance`), not from the trace: the constraint and the
  instances that could unify, presented as GHC gives them.
- **Rigid type variables** (`CannotUnifyVariable`) are explained today as
  unexplained. Fixing that is the first thing to check, since the data is in
  the same `MismatchMsg`.
- **Missing imports** are not something GHC suggests; `hinsight` does not
  invent them.
- **Cost.** The trace pass runs only when there is a type error, never by
  default.
- **Hole fits** are filtered and ordered from GHC's own data only.
- **Core, specialisation, rule firings and STG** are presentations of GHC's
  dumps, from a compile mode with its own cost, writing to a temporary
  directory that is removed. Core at `-O0` is nearly free (about 10 to 20
  percent over a typecheck) but is not optimised, so specialisation and rule
  firings need `-O`: 3.4 to 4.4 times a typecheck, 6 to 9 times with the dumps
  written. STG needs the bytecode backend: about 5 times. These run on request
  for one module, never by default.
- **Splice expansion** is a presentation of `-ddump-splices`.
- **Space-leak findings** are out of scope: GHC reports nothing about them.
- **Refactoring** (extract function, move definition, change type and
  propagate) has no existing tool to present, so under the rule it is out of
  scope. What GHC itself suggests is in scope: its typed hints, shown with the
  diagnostic they belong to. They carry no edit text, so `hinsight` shows the
  suggestion and does not apply it.

## Still to look at

1. Whether the existing mismatch extraction works unchanged on
   `CannotUnifyVariable`, and what its reason says.
2. Compiling the dependencies at `-O0` and only the target at `-O`, to cut the
   cost of the Core mode. Not measured.
3. The cleaner on traces from larger modules than the twelve fixtures.
