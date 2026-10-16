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
| Class-constraint errors? | Not a unification chain. The unification steps at that line are trivial; the content is instance lookup (`matchClass`, `matchInstEnv`) and the ambiguity text. A separate, small look is needed. |
| Can a log hook inside the session receive the trace? | Yes. `pushLogHookM` (already used by `Session.hs`) receives each trace record as its own message: 22,696 records for a 198-line module, no boundary parsing. |
| Cost | Tracing the whole session: 0.81 s against 0.12 s (198-line module, 4.25 MB of text), 1.86 s against 0.18 s for a module with two dependencies (57,198 records, 9.9 MB). Dependencies are traced too, and `load` with changed flags retraces them. |
| Tracing only the target | Works as a second pass shaped like `deferredPipelines` (set flags, `depanal`, `parseModule`, `typecheckModule` of the target): 20,059 records, all from the target, 0.66 s against 0.15 s for the normal pass. |
| Core, specialisation, rule firings | Nothing is printed under `-fno-code`. Under `-fno-code -fwrite-if-simplified-core` GHC prints Tidy Core, Specialise and rule firings and writes a `.hi` into the chosen directory. The bytecode backend adds Initial and Final STG and writes `.o` and `.hi`. Nothing was written next to the source. Cost of an optimising compile on a real module was **not** measured. |
| Template Haskell splices | `-ddump-splices` under `-fno-code` prints one line per splice with its span: `Th.hs:6:11-30: Splicing expression litE (integerL 42) ======> 42`. |
| Hole-fit plugin in our session | Already done: `installHolePlugin` attaches a static plugin and the hole capture is tested. `HoleFit` carries the identifier, type, refinement level, matches and documentation; there is no usage count. |

## Findings from reading source and documentation

| Question | Result |
|---|---|
| Unification steps as data? | No. A type-checker plugin sees constraints, not unification steps (`GHC/Tc/Plugin.hs`). |
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
  fixture-tested cleaner with the raw line as fallback. Class-constraint
  errors need their own look.
- **Cost.** The trace pass runs only when there is a type error, never by
  default.
- **Hole fits** are filtered and ordered from GHC's own data only.
- **Core, specialisation, rule firings and STG** are presentations of GHC's
  dumps, from a compile mode with its own cost, writing to a temporary
  directory that is removed. STG needs the bytecode backend.
- **Splice expansion** is a presentation of `-ddump-splices`.
- **Space-leak findings** are out of scope: GHC reports nothing about them.
- **Refactoring** (extract function, move definition, change type and
  propagate) has no existing tool to present, so under the rule it is out of
  scope. What GHC itself suggests (its structured hints) is in scope; how
  rich those are is *unverified*.

## Still to look at

1. What GHC logs for class-constraint errors (`matchClass`), and whether it
   is presentable.
2. The cleaner for printed types, against fixtures from real traces.
3. The cost of an optimising compile of a real module.
4. What GHC's structured hints (`diagnosticHints`) contain.
