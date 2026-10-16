# What GHC and its tools expose, and what `hinsight` may do with it

Status: **findings from reading GHC 9.10.3's source and one support table.**
Nothing here was run. Items marked *unverified* have not been read or tried.
This document caps what the next phase may promise.

## The rule

`hinsight` presents what GHC and existing tools say. It does not infer what
they did not say. Formatting, filtering, ordering and parsing their text or
structured output is presentation and is allowed. Reconstructing causes,
ranking by a measure of its own, or filling a gap with a guess is not.
When a piece cannot be read, the answer is a typed refusal that shows the raw
output, never a best-effort sentence.

## Findings

| Question | Finding | Evidence |
|---|---|---|
| Does GHC expose the steps of unification as data? | No. A type-checker plugin sees wanted and given constraints, not unification steps. | `GHC/Tc/Plugin.hs`, `GHC/Tc/Types.hs` (`TcPlugin`) |
| Is the mismatch itself structured? | Partly. `CtOrigin` has `TypeEqOrigin` with the actual type, the expected type and the thing. Extra context is stored as rendered text. | `GHC/Tc/Types/Origin.hs`, `ErrInfo` in `GHC/Tc/Errors/Types.hs` |
| Does GHC print the typechecker's steps as text? | Yes: `-ddump-tc-trace`. About 25 trace points in `GHC/Tc/Utils/Unify.hs` (`u_tys`, `found filled tyvar`, `uUnfilledVar2 ok`/`not ok`, ...) with fixed headings. | `traceTc` in `GHC/Tc/Utils/Monad.hs` |
| Do those lines carry a source span? | Only when `-dppr-debug` is also set; GHC then prefixes the span being checked. | `wrapDocLoc` in `GHC/Tc/Utils/Monad.hs` |
| Are trace lines stable? | The headings are fixed text within one GHC version, which `hinsight` pins. Type variable names carry unique numbers and are not stable across edits. | `Unify.hs`; the second point is *unverified by a run* |
| Are solver-raised mismatches traced with a span? | *Unverified.* Many mismatches are raised in the constraint solver, which was not read. | |
| Does `-fdiagnostics-as-json` help? | It gives an envelope (version, GHC version, span, severity, code, rendered message and hints), schema 1.0. The message body is rendered text. Through the API we already receive the typed message, so it adds nothing there; it is useful for diagnostics from other processes. | `GHC/Types/Error.hs`; the schema file itself was not read |
| Typed-hole fits | `HoleFit` carries the identifier, its type, refinement level, what its holes matched, and documentation. A `HoleFitPlugin` can filter and reorder. No usage count. | `GHC/Tc/Errors/Hole/FitTypes.hs`, `.../Plugin.hs` |
| Can a hole-fit plugin be attached inside our session? | *Unverified.* | |
| Core, STG, specialisation | Core plugins receive typed Core; text dumps exist (`-ddump-simpl`, `-ddump-spec`, `-ddump-rule-firings`, `-ddump-stg-final`). Whether `noBackend` can produce optimised Core is *unverified*. | `GHC/Driver/Plugins.hs`, `GHC/Driver/Session.hs` |
| Space leaks | GHC reports nothing about them. **Not possible**; dropped. | |
| Template Haskell splices | Hooks (`runMetaHook`, `spliceRunAction`) and `-ddump-splices` text exist. | `GHC/Driver/Hooks.hs`, `GHC/Driver/Plugins.hs` |
| HLS | Fully supports GHC 9.10.3, as a server. Its plugins are code actions inside it, not libraries to import. | HLS GHC version support table |
| `haddock-api` | Latest release on Hackage is bounded to GHC 9.6. It cannot be a dependency on 9.10.3. Haddock text already reaches us through the interface files and `hfDoc`. | Hackage package page |
| Hoogle as a library; ghc-exactprint, retrie, apply-refact on 9.10.3 | *Unverified.* The package pages were not reachable. | |

## What this caps

- **Unification steps** may be shown as "what GHC logged while checking the
  span of the error", from a second pass with `-ddump-tc-trace -dppr-debug`,
  filtered by span. They are not "the chain that caused the error". Whether
  this is useful is decided by a spike (below), not by this document.
- **Hole fits** are filtered and ordered from GHC's own data only. No ranking
  of our own.
- **Core, STG and specialisation** are presentations of GHC's dumps.
- **Space-leak findings** are out of scope.
- **Refactoring** stays undecided until what existing tools offer as edits is
  known.

## Spikes before the next phase

Throwaway programs outside the repository, each ending in *possible*,
*partial* or *not possible* with the raw evidence:

1. `-ddump-tc-trace -dppr-debug` on a failing program, including an error
   raised by the solver: do lines carry spans, and do they match the error's?
2. Attach a hole-fit plugin to a session through the GHC API.
3. Core and STG dumps under `noBackend`.
4. Splice expansion under `noBackend`.
5. What HLS offers for extract, change-type and move, and which edit libraries
   build against GHC 9.10.3.
