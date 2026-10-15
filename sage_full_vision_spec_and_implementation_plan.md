# Sage (HInsight): Full Vision, Specification, Architecture and Implementation Plan

> **Purpose of this document:** one working specification for building Sage, Rasika's GHC-internals analysis module. It combines the Sage/Insight requirements from `rasika_modules.md`, the current HInsight implementation structure, explicit architectural constraints, a proposed internal code skeleton, acceptance criteria, and a recommendation on whether to split the module.
>
> **Source discipline:** sections marked **From Rasika specification** preserve requirements and boundaries from the supplied module breakdown. Sections marked **Current code baseline** describe what exists in the supplied HInsight archive. Sections marked **Recommended implementation plan** are engineering recommendations, not claims that the original specification already mandates those exact details.

## 0. Executive decision

**Keep Sage as one standalone package (`hinsight`) for now. Split its internals into well-defined modules, not into multiple independently published packages.**

The existing package already has a public facade (`HInsight`), domain/result modules (`HInsight.Flow`, `HInsight.Hole`, `HInsight.Explanation`, `HInsight.Graph`, etc.), a GHC adapter namespace (`HInsight.Ghc.*`), tests, a demo and a separate Tadka adapter package. That is a sound starting shape.

Sage's full scope is broad, but the features share the same expensive and version-sensitive GHC boundary, source-location model, typed results and analysis lifecycle. Splitting into several packages now would add versioning, build, API and dependency overhead before the boundaries have proved themselves. Split into a helper process only if linking the GHC API makes the final Rasika binary or deployment impractical; this is already an option in the original specification.

This document uses **Sage** for the Rasika capability and **HInsight** for its current package/codebase. The original module breakdown names it “Insight”, package `hinsight`, root namespace `HInsight`.

---

## 1. Vision: what Sage is supposed to be

### 1.1 Purpose from the Rasika specification

**From Rasika specification**

Sage answers questions that need GHC internals beyond what HLS offers. It is a headless, editor-agnostic Haskell analysis library. It receives questions/analysis inputs and returns typed data, explanation steps, annotations, ranked candidates and edits. It does not draw UI.

Its standalone value proposition is to answer questions such as:

- **“Why did GHC infer this?”**
- **“What does this become?”**

The module should expose GHC-derived semantic information in a form that Rasika can present, and that other software can consume without embedding Sage-specific UI.

### 1.2 Intended user-visible capabilities

1. **Explain type inference and type errors.** Go beyond a flat expected/actual mismatch and explain the chain of constraints/unifications that led to a type.
2. **Trace type flow.** Show the types at stages of selected operator pipelines, currently specified for `.`, `$` and `>>=`.
3. **Typed-hole assistance.** Collect GHC's candidate fits, rank them using fit quality, locality and usage, and return a source edit that accepts a selected candidate.
4. **Semantic refactoring.** Change a type and propagate the change; extract a function and infer its signature; move definitions between modules.
5. **Compiler-internals inspection.** Inspect Core and STG, report specialization and missed-optimization opportunities, and identify likely space-leak warnings.
6. **Template Haskell inspection.** Expand a splice and expose the result as data.
7. **Project-aware analysis.** Load the user's project with its intended compiler environment and analyse the relevant modules rather than assuming a bare source file.

### 1.3 What Sage is not

- Not a compiler or replacement typechecker: GHC remains the authority.
- Not an IDE, editor, terminal, renderer or UI framework.
- Not a duplicate of HLS. If HLS already supplies a capability, it belongs to Language, not Sage.
- Not the build/test/run driver; that belongs to Runner.
- Not the static knowledge base, Hoogle or Haddock browser; those belong to Library.
- Not the owner of editor keymaps, panes, leader menus, first-run tour or Rasika application routing.
- Not a research project to invent a new type system or compiler analysis from scratch. Prefer GHC's APIs and existing ecosystem libraries, and implement only the glue/analysis that Rasika needs.

### 1.4 Definition of success

Sage is successful when the Rasika app can pass a source location or analysis request to a stable public API and receive a typed, explainable result or an explicit failure. The app should not need to import GHC internals, parse human-readable compiler output, reconstruct edits or duplicate Sage's analysis logic.

---

## 2. Exact functional scope

### 2.1 GHC API layer and project loading

**From Rasika specification:** load a project using the supplied GHC/toolchain.

Required responsibilities:

- Accept the target project root, source path and relevant settings as inputs; do not discover or hard-code Rasika-specific paths.
- Capture the project's compiler environment and flags.
- Load enough of the target project for analysis to respect imports and typechecking.
- Return typed errors for project/environment/session failures.
- Make GHC-derived values cross the boundary only through deliberate conversion into Sage's public domain types.
- Support the one recommended GHC version in the Rasika plan: GHC 9.10.3 baseline. A later recommended-version bump is an explicit maintenance task, not a mandate to support a broad GHC matrix.

**Current code baseline:** `HInsight.Ghc.Session`, `HInsight.Ghc.Environment`, `HInsight.Environment`, `HInsight.Project`, `HInsight.Source`, `HInsight.Config` and `HInsight.Error` already establish a substantial session/environment foundation. The repo's decisions document says the current implementation creates a fresh session per analysis and does not do package resolution itself; environment capture is delegated to `hie-bios` in a separate package. Preserve this boundary unless tests or profiling demonstrate a concrete problem.

### 2.2 Unification-chain explanation

**From Rasika specification:** explain why GHC inferred a type, step by step.

This is a key missing Sage capability. It is not satisfied by displaying only an expected type and an actual type.

Expected behaviour:

- Accept a relevant source span or selected diagnostic/expression.
- Identify the relevant type constraints or type relationships available from GHC.
- Present an ordered sequence of explanation steps.
- Each step should say what is being compared or constrained, what matched, and where a mismatch or refinement occurred, to the extent GHC exposes reliable information.
- Keep the output structured: a UI should be able to render each step, highlight source ranges and provide a concise summary.
- When GHC does not expose enough evidence to reconstruct a trustworthy chain, return an explicit “not available / partial explanation” result rather than fabricate a derivation.

Important implementation realism: GHC's public interfaces do not necessarily expose a complete historical trace of every unification operation after typechecking. First build a small GHC-API experiment to establish what can be captured without invasive compiler changes. Reuse GHC's existing diagnostics and type information. Do not promise a complete constraint solver trace if the API cannot provide it.

### 2.3 Type flow through selected pipelines

**From Rasika specification:** trace type flow along `.`, `$` and `>>=`.

Expected behaviour:

- Find selected supported operator chains in the typechecked tree.
- Capture source spans and the type at each relevant stage.
- Return operator/pipeline kind, ordered stages, overall expression type and surrounding context.
- Preserve the current explicitly limited operator scope unless a concrete Rasika use case justifies expanding it.
- Handle expressions that cannot be analysed and source positions that cannot be mapped without crashing or silently inventing types.

**Current code baseline:** `HInsight.Ghc.Flow` extracts data from the typed tree; `HInsight.Flow` defines the public pipeline model and rendering. The current implementation walks typed syntax generically but only reports the specified operator forms. Keep the compiler-specific traversal in `HInsight.Ghc.Flow` and public types/rendering in `HInsight.Flow`.

### 2.4 Typed holes and smart fill

**From Rasika specification:** collect GHC hole fits, rank by fit, locality and usage, and provide the edit that accepts one.

Expected behaviour:

- Capture typed-hole location, expected type and available fits from GHC.
- Preserve the fit's expression/name and type information.
- Rank using a documented, deterministic policy based on fit quality, locality and usage.
- Provide an edit that replaces the hole with the selected fit.
- Return candidate explanations/metadata in structured values; do not render UI in the library.
- Make it possible for the app to preview/accept the edit.
- Do not duplicate an HLS hole action: the Rasika specification explicitly assigns typed holes and the single ranking/accept-edit contract to Sage.

**Current code baseline:** `HInsight.Ghc.HoleCapture`, `HInsight.Ghc.Holes`, `HInsight.Hole` and `HInsight.Report` already capture and rank candidate fits. The current repo's decision notes say ranking uses fit and locality only, while the target specification also mentions usage. GHC hole fits that have already been converted into documents are skipped in the current capture path. A mutable cell is used to receive fits from GHC's plugin interface. Context is extracted from parsed source rather than relying on GHC's origin label.

**Remaining:** close the ranking-policy gap if usage information is available/reliable, and provide a safe source edit for accepting a fit. Do not imply that a candidate list alone is “smart fill complete”.

### 2.5 Semantic refactoring

**From Rasika specification:** exactly these three operations are in the stated scope:

1. **Change a type and propagate it.**
2. **Extract a function with an inferred signature.**
3. **Move definitions between modules.**

Each must return proposed source edits; Sage should not silently write files itself unless a separate, explicit file-application contract is later chosen. The Rasika app is responsible for presenting/applying edits through the editor.

Minimum behavioural expectations:

- Validate that the target selection maps to valid syntax and semantic entities.
- Compute all affected edits, including import/signature changes where needed.
- Detect conflicts and reject unsupported cases explicitly.
- Avoid name capture and preserve scope.
- Make edits deterministic and applicable against the exact source version/ranges for which they were computed.
- Test successful transformations, invalid selections, conflicts, comments/formatting, imports, multi-module effects and negative cases.
- Where semantic preservation cannot be proven or the transformation is unsupported, return a typed refusal rather than applying a best-effort textual rewrite.

These are semantic operations, not search-and-replace helpers. Reuse GHC's parsed/renamed/typechecked structures and established refactoring facilities if a suitable library exists; do not write a new parser or pretend textual replacement is semantic refactoring.

### 2.6 Core and STG analysis

**From Rasika specification:** inspect Core, STG, specialization, missed optimizations and likely space leaks.

Required analysis categories:

- **Core inspection:** expose relevant GHC Core in a structured or otherwise consumable form.
- **STG inspection:** expose the relevant STG representation.
- **Specialization:** identify available specialization information and useful cases where GHC did or did not specialize, without overstating the certainty of a “missed optimization”.
- **Space-leak warnings:** identify likely suspicious patterns from available compiler representations/metadata. Phrase results as heuristics unless they are proofs.
- **Annotations:** include source association when GHC retains it; indicate when a compiler-generated node cannot be mapped reliably to source.

Keep the first version narrow: make representations inspectable and return a few defensible, testable findings. Do not build a speculative optimiser or a new profiling system. Runner owns profiling collection and profile/heap/event-log parsing; Sage owns analyses that require GHC internals.

### 2.7 Template Haskell splice expansion

**From Rasika specification:** expose Template Haskell splice expansion.

Expected behaviour:

- Identify a splice in a target module.
- Use GHC's existing facilities to obtain the expansion where supported.
- Return expansion text/structure and relevant source location as typed data.
- Report compile-time execution failures and unsupported cases as typed errors.
- Respect project environment and dependencies; do not assume a bare one-file compile.
- Do not execute arbitrary project code outside the compiler's normal Template Haskell model or invent a separate evaluator.

---

## 3. Ownership boundaries inside Rasika

**From Rasika specification**

| Capability | Owner | Sage boundary |
|---|---|---|
| Live diagnostics while typing | Language via HLS | Sage does not duplicate the HLS diagnostics engine |
| Build diagnostics | Runner, then app merges results | Sage is not the build runner |
| Static error-code explanations/examples | Library | Sage handles computed explanations from the user's code |
| Typed holes | Sage | One ranking policy and one accept-edit contract |
| Declared/inferred signatures as editor feature | Language via HLS | Sage uses types internally for its own refactorings |
| Evaluate `>>>` comments | Language via HLS | GHCi session remains Runner's responsibility |
| Rename across modules | Language via HLS | Sage's refactoring scope is moving definitions, not generic rename |
| Hoogle/Haddock/Hackage | Library | Sage does not own documentation browsing |
| Core/profile/coverage views | Sage computes data; app/Basilisk/Tadka/Theme render | Sage never draws panes or charts |
| Project setup/toolchain installation | Toolchain | Sage consumes supplied paths/environment |
| File tree, search, git, terminal | Workspace | Sage does not own workspace services |

The app routes “Explain this” to Library for static knowledge and Sage for computed explanations. The app merges live HLS and build diagnostics; Sage is not the merger.

### App-facing contract

The app should be able to provide:

- Project root and source path.
- The chosen GHC/toolchain path or library directory.
- Project environment/settings needed for correct analysis.
- A query: explain a diagnostic, inspect a pipeline, complete a hole, refactor a selection, inspect Core/STG, or expand a splice.
- Source spans/version context where applicable.

Sage should return:

- Typed explanation steps and summaries.
- Source-anchored annotations where possible.
- Ranked hole candidates and a candidate-acceptance edit.
- One or more validated proposed edits for refactorings.
- Core/STG or splice results and structured findings.
- Typed failures/unsupported-case results and enough context to diagnose them.

No UI objects, editor buffers, keymaps or Rasika application imports should cross into the package.

---

## 4. Current HInsight code baseline

**Current code baseline**

The archive already has a useful architecture and should be extended, not discarded.

```text
hinsight/
├── src/
│   ├── HInsight.hs                    # public facade
│   ├── HInsight/Analysis.hs           # analysis result and top-level orchestration
│   ├── HInsight/Config.hs             # GHC libdir/path configuration
│   ├── HInsight/Environment.hs        # effect/environment records and orchestration
│   ├── HInsight/Error.hs              # typed failure types and rendering
│   ├── HInsight/Explanation.hs        # structured type mismatch explanation
│   ├── HInsight/Flow.hs               # pipeline domain model and rendering
│   ├── HInsight/Graph.hs              # module graph domain model
│   ├── HInsight/Hole.hs               # hole-fit domain model and ranking
│   ├── HInsight/Project.hs            # project-level input/model
│   ├── HInsight/Report.hs             # aggregate report model
│   ├── HInsight/Source.hs             # source-file input model
│   ├── HInsight/Context.hs            # surrounding source context
│   ├── HInsight/Identifier.hs         # identifier model
│   ├── HInsight/TypeText.hs           # type text representation
│   └── HInsight/Ghc/
│       ├── Session.hs                 # GHC session and analysis entry point
│       ├── Environment.hs             # project compiler environment capture
│       ├── Messages.hs                # structured GHC diagnostics
│       ├── HoleCapture.hs             # GHC hole-fit capture
│       ├── Holes.hs                   # typed-hole analysis
│       ├── Flow.hs                    # typed-tree pipeline analysis
│       ├── Graph.hs                   # imports/module graph from GHC
│       ├── Bindings.hs                # binding/context discovery
│       └── Convert.hs                 # GHC values to public domain values
├── hinsight-tadka/
│   └── src/HInsight/Tadka*.hs         # separate diagnostics adapter package
├── app/Main.hs                        # standalone demo
├── test/                              # Hspec specs and source fixtures
├── bench/Main.hs                      # benchmark
└── toolchain/                         # GHC 9.10.3 libdir helper package


### Existing public facade

`src/HInsight.hs` re-exports public inputs/results/failures, including:

- `SourceFile`, `LibDir`
- `Analysis`, `Insight`
- `Explanation`, `Context`
- `Pipeline`, `FlowKind`, `Stage`
- `ProjectGraph`, `ModuleName`
- `Identifier`
- `HoleReport`, `HoleFit`, `rankFits`
- `InsightError`, `EnvironmentError`

This is the right direction: consumers import `HInsight` rather than reaching into compiler-specific modules. Keep the facade small and intentional.

### Existing build/toolchain constraints

- Package: `hinsight`
- Root namespace: `HInsight`
- `tested-with: GHC ==9.10.3`
- Direct dependency on `ghc ==9.10.3`
- A separate `hinsight-toolchain` sublibrary locates the GHC 9.10.3 library directory.
- `hinsight-tadka` is a separate adapter package, not the core analysis package.
- Hspec/QuickCheck tests, fixtures, a demo and a benchmark already exist.
- Warning options include `-Wall`, `-Wcompat`, incomplete-pattern and incomplete-record-update warnings, unused-binding checks and related checks. The manual `werror` flag enables `-Werror` for development/CI while remaining off by default for consumers.

### Existing design decisions to preserve or revisit deliberately

The repository's own `docs/decisions.md` records these choices:

- Fresh GHC session per analysis; no package resolution in HInsight itself.
- Environment capture delegated to `hie-bios` in a separate package.
- Hole fits are captured through a mutable cell required by GHC's callback-style interface.
- Hole fits already converted into documents are skipped.
- Source context comes from parsing the source, not solely from GHC's origin metadata.
- Ranking currently uses fit and locality only.
- Type flow walks the typechecked tree generically.
- Module graph follows ordinary imports only.
- GHC source positions are converted into public positions, not copied blindly.
- Environment flags are parsed using GHC's parser and HInsight's own flags are applied last.
- Tadka is a separate package.

Do not change these choices merely for aesthetic reasons. Change them only when a requirement, reproducible bug, test, or benchmark justifies it.

---

## 5. Recommended internal code skeleton

This is a proposed target layout, not a claim that these modules already exist. It keeps a single package and separates public domain models from GHC-specific extraction.

```text
src/
├── HInsight.hs                       # small, stable public facade
├── HInsight/
│   ├── API.hs                        # optional internal request/result orchestration
│   ├── Analysis.hs                   # aggregate analysis result / orchestration
│   ├── Config.hs                     # toolchain and configuration inputs
│   ├── Environment.hs                # effect records / environment interface
│   ├── Error.hs                      # typed errors, unsupported cases
│   ├── Source.hs                     # source file, spans, source version
│   ├── SourceEdit.hs                 # proposed edits and edit validation
│   ├── Context.hs                    # source/binding context
│   ├── Identifier.hs                 # identifier and semantic identity
│   ├── TypeText.hs                   # stable type display representation
│   ├── Explanation.hs                # explanation summary and ordered steps
│   ├── Unification.hs                # unification-chain public model
│   ├── Hole.hs                       # hole report, fits and ranking policy
│   ├── Flow.hs                       # pipeline/type-flow public model
│   ├── Graph.hs                      # module graph public model
│   ├── Refactor.hs                   # refactoring requests/results
│   ├── Core.hs                       # Core inspection/finding result types
│   ├── Stg.hs                        # STG inspection/finding result types
│   ├── Splice.hs                     # Template Haskell expansion result types
│   ├── Report.hs                     # aggregate report, if still needed
│   └── Ghc/
│       ├── Session.hs                # session lifecycle / project load
│       ├── Environment.hs            # GHC flags and project env
│       ├── Convert.hs                # GHC types -> stable public types
│       ├── Bindings.hs               # binding/context discovery
│       ├── Messages.hs               # compiler diagnostic extraction
│       ├── HoleCapture.hs            # GHC hole-fit capture
│       ├── Holes.hs                  # GHC-specific hole analysis
│       ├── Flow.hs                   # typed-tree pipeline extraction
│       ├── Graph.hs                  # GHC import/module graph extraction
│       ├── Unification.hs             # GHC-side evidence capture experiment
│       ├── Refactor.hs                # typed/renamed AST support for edits
│       ├── Core.hs                    # Core extraction/analysis
│       ├── Stg.hs                     # STG extraction/analysis
│       └── Splice.hs                  # splice expansion through GHC
└── ...
```

### Rules for this skeleton

- Public modules define stable result types and business meaning; `HInsight.Ghc.*` deals with GHC-specific representations.
- The public facade re-exports only the supported contract. Do not expose internal GHC AST types as part of the public API.
- If a feature cannot yet produce a stable result, keep it internal/experimental rather than prematurely committing to a public API.
- Do not create a module for every helper function. Split by cohesive responsibility and independently testable behaviour.
- Do not force an `API.hs` module if `HInsight.Analysis` already cleanly performs the orchestration. Avoid duplicate abstractions.
- Do not move Tadka into the core package. Its separation is intentional.

### Suggested public model sketches

These are illustrative type shapes; they need refinement against the actual source-position and error models already in the repo.

```haskell
-- HInsight.SourceEdit
data SourceEdit = SourceEdit
  { editFile        :: FilePath
  , editRange       :: SourceRange
  , editReplacement :: Text
  }

data EditSet = EditSet
  { editBaseVersion :: SourceVersion
  , edits           :: NonEmpty SourceEdit
  }

-- HInsight.Unification
data ExplanationStep
  = ConstraintIntroduced ConstraintSummary
  | TypesUnified TypeSummary TypeSummary
  | TypeMismatch TypeSummary TypeSummary MismatchReason
  | EvidenceUnavailable Text

data UnificationExplanation = UnificationExplanation
  { explanationSpan  :: SourceRange
  , explanationSteps :: [ExplanationStep]
  , explanationState :: ExplanationCompleteness
  }

-- HInsight.Refactor
data RefactorRequest
  = ChangeType TypeChangeRequest
  | ExtractFunction ExtractFunctionRequest
  | MoveDefinition MoveDefinitionRequest

data RefactorResult
  = RefactorProposed EditSet
  | RefactorRejected RefactorError

-- HInsight.Core / HInsight.Stg
data CompilerFinding = CompilerFinding
  { findingKind        :: FindingKind
  , findingMessage     :: Text
  , findingSourceRange :: Maybe SourceRange
  , findingConfidence  :: Confidence
  }
```

Do not copy these sketches verbatim without adapting them to the repo's existing `SourceFile`, position, error and effect types. They show the intended boundary, not a final API.

---

## 6. Proposed implementation order and acceptance criteria

### Phase 0 — Lock down baseline (first)

1. Build and test the current commit under GHC 9.10.3.
2. Run the existing Hspec/QuickCheck suites and CI checks.
3. Record a baseline for session startup and repeated analysis on a real multi-module Cabal project.
4. Review the current public API and source-span conversion rules.
5. Confirm what the current environment layer obtains from `hie-bios` and what is intentionally outside HInsight.

**Exit criteria:** current features pass tests and there is a recorded baseline. Do not start by rewriting the session layer.

### Phase 1 — Complete typed holes and the edit foundation

1. Add a reusable source-edit model.
2. Add validation against the source version/range for which the edit was produced.
3. Complete fit ranking according to the specified fit/locality/usage criteria, only where the information is available.
4. Generate the acceptance edit for a chosen hole fit.
5. Test local/imported candidates, malformed/unsupported locations, overlapping edits and stale source versions.

**Exit criteria:** Rasika can display a ranked fit and apply/preview the corresponding edit through its own editor layer.

### Phase 2 — Unification explanation spike, then implementation

1. Prototype the relevant GHC 9.10.3 API against a tiny fixture project.
2. Establish which constraint/unification evidence can be captured without modifying GHC.
3. Define a result model that distinguishes complete, partial and unavailable evidence.
4. Implement ordered explanation steps and source association.
5. Add golden tests for representative successful and failing examples.

**Exit criteria:** a real diagnostic can produce a structured explanation that never invents missing compiler evidence.

### Phase 3 — Semantic refactoring

Recommended order:

1. Extract function with inferred signature.
2. Change a type and propagate it.
3. Move definitions between modules.

Build shared edit validation and conflict detection first. Keep each refactoring independently testable. Use small, explicit supported-case boundaries rather than claiming to handle every Haskell syntax feature on the first release.

**Exit criteria:** each operation returns deterministic proposed edits, has positive and negative tests, and preserves semantics for its declared supported cases.

### Phase 4 — Core/STG and splice expansion

1. Establish the GHC 9.10.3 hooks and representations available for Core, STG and splice expansion.
2. Return inspectable data first.
3. Add only a small set of defensible specialization/missed-optimization/space-leak findings.
4. Preserve source provenance where possible and mark generated/unmapped nodes clearly.
5. Test with fixtures that exercise optimization and Template Haskell.

**Exit criteria:** compiler internals are exposed as typed data and every heuristic is labelled as a heuristic rather than a guarantee.

### Phase 5 — Rasika integration and release hardening

- Integrate through the app's wiring layer only.
- Route static error/extension knowledge to Library and code-derived explanations to Sage.
- Keep all rendering in Basilisk/Tadka/Theme/app.
- Test real multi-module projects and error cases.
- Verify package independence and GHC-version bump procedure.
- Document unsupported cases, API stability and build requirements.

---

## 7. Do's and don'ts

### Do

- **Do reuse GHC's APIs and existing Haskell ecosystem facilities.**
- Do keep GHC-specific types and traversals behind `HInsight.Ghc.*`.
- Do return typed data, source ranges, edits and typed errors.
- Do use GHC 9.10.3 as the explicit baseline from the Rasika plan.
- Do test real projects as well as small fixture files.
- Do distinguish an explanation supported by compiler evidence from a heuristic or incomplete result.
- Do define supported cases and reject unsupported refactorings explicitly.
- Do keep `hinsight-tadka` separate from the core package.
- Do benchmark before adding persistent sessions or caching.
- Do keep the public API small and intentional.
- Do use property/golden/integration tests where appropriate.
- Do keep the package headless and independently usable.

### Don't

- Don't duplicate features already assigned to HLS/Language.
- Don't turn Sage into a renderer, editor, terminal, build runner or package browser.
- Don't make Sage import Rasika application modules or other Rasika module roots.
- Don't parse human-readable compiler messages when structured GHC information is available.
- Don't expose GHC AST/compiler-internal types as long-lived public API types.
- Don't claim a complete unification trace until the GHC API experiment proves it can be captured.
- Don't call candidate listing “smart fill” without an acceptance edit.
- Don't implement semantic refactoring with raw search-and-replace.
- Don't silently apply edits or write files behind the app's back.
- Don't label optimization/space-leak heuristics as proofs.
- Don't expand operator support, compiler-version support or feature scope without a concrete need.
- Don't split the package prematurely or create a separate package per feature.
- Don't change established design decisions without a failing test, measured performance problem or concrete requirement.
- Don't add broad dependencies merely to avoid writing a small, well-bounded adapter.
- Don't ship an untested GHC-version bump.

---

## 8. Test strategy and release gates

Every feature should have four test layers where relevant:

1. **Pure model tests:** ranking, edit ordering, graph logic, explanation formatting and error classification.
2. **Fixture tests:** small source files with known types, holes, imports, refactor selections, splices and compiler outputs.
3. **Project integration tests:** multiple modules, local package configuration, CPP/extensions and environment flags.
4. **Rasika contract tests:** request in, typed result/edit out; app performs rendering/application.

Release gates:

- `cabal build all` succeeds on GHC 9.10.3.
- Core and Tadka tests pass.
- `-Wall` and the project's warning policy are clean; CI's `werror` mode passes.
- Public API functions have explicit signatures, including local helpers under the gold-standard policy.
- No partial functions or incomplete-pattern assumptions in supported paths.
- Refactoring tests prove semantic preservation for supported cases.
- Every unsupported path produces an explicit typed error/result.
- Benchmarks report session startup and repeated-analysis cost; do not introduce brittle machine-specific thresholds except where justified.
- Package boundary checks confirm no Rasika-specific references and no dependencies on sibling Rasika module packages.

---

## 9. Split decision: package, process or internal modules?

### Recommendation: one package, several internal modules

**Do not split Sage into separate Haskell packages now.** Split its implementation into cohesive internal modules as shown in Section 5.

Reasons:

- All feature areas depend on the same GHC-version-sensitive layer.
- Public result types, source edits and errors need to stay consistent.
- Semantic refactorings reuse source-edit infrastructure.
- Core/STG/splice analyses reuse the project/session boundary.
- Multiple packages would add release/version coordination without yet proving independently useful stable contracts.
- The existing code already has a workable separation between the public domain modules, GHC adapter modules and Tadka adapter package.

### When to reconsider

Split the GHC internals into a **helper process** if linking GHC makes Rasika's final binary, startup, deployment or dependency isolation impractical. Keep the same typed request/result contract so the app does not care whether the analysis runs in-process or out-of-process.

Consider separate packages only after a clear subset has a stable API, a distinct dependency/release lifecycle, and a real consumer other than the core Sage package. Do not split simply because the source tree grows.

### Internal boundaries worth preserving

- `HInsight.Ghc.*`: all GHC-specific work.
- `HInsight.*` domain modules: stable public types and pure transformations.
- `HInsight` facade: supported external API.
- `hinsight-tadka`: diagnostics adaptation, separate package.
- Rasika app wiring: the only layer that adapts across Rasika modules.

---

## 10. Effort estimate from the supplied Rasika specification

**From Rasika specification:** the full Insight/Sage estimate is **6,650 source LOC plus 5,000 test LOC**. The original breakdown is:

| Area | Estimated source LOC |
|---|---:|
| GHC API session and project loading | 500 |
| Typed-AST traversal toolkit | 650 |
| Unification chains | 1,100 |
| Type flow | 450 |
| Typed holes | 550 |
| Semantic refactoring | 1,750 (change type 600; extract function 500; move definition 650) |
| Core and STG analysis | 900 |
| Template Haskell splice expansion | 300 |
| Public API, errors and configuration | 450 |
| **Total** | **6,650** |

The spec assumes gold-standard Haskell: signatures for all functions, typed errors, pure core/effects at the edges, no partial functions, warnings-as-errors in CI, resource-safe process handling, property/golden/integration tests, Ormolu and HLint.

The spec also estimates a few hundred lines of concentrated changes in Insight when the recommended GHC version changes, plus refreshed GHC-output golden tests in Runner, Insight and tadkaGhc.

These are planning estimates, not a promise of actual line count. Reuse existing code and APIs where possible; do not add LOC merely to match the estimate.

---

## 11. Package naming and hosting constraints

**From Rasika specification**

- Package: `hinsight`
- Root namespace: `HInsight`
- The package must not mention Rasika in code, docs, error messages, default paths, environment variables or Cabal metadata.
- Tool paths, project roots, locations, templates and keys are passed in as arguments.
- Modules depend on base libraries only; sibling Rasika modules do not depend on one another. Only the Rasika app wiring layer imports multiple module roots.
- CI should enforce the package boundary and absence of `rasika` references.
- Hackage is optional. Rasika can pin the module from GitHub using `source-repository-package` or submodules.
- Before publishing, check that the package name and module root are available on Hackage.

---

## 12. Immediate checklist

Use this list to start implementation in order.

- [ ] Build/test the current HInsight revision on GHC 9.10.3.
- [ ] Record baseline session and repeated-analysis timings.
- [ ] Review source spans and the public `SourceFile`/error contracts.
- [ ] Add source-edit data types and stale/overlap validation.
- [ ] Complete typed-hole fit ranking and acceptance edits.
- [ ] Prototype GHC's available unification evidence before designing the full API.
- [ ] Implement structured unification explanations with partial/unavailable states.
- [ ] Implement extract-function refactoring.
- [ ] Implement type-change propagation.
- [ ] Implement cross-module definition move.
- [ ] Add Core/STG inspection and a few evidence-backed findings.
- [ ] Add Template Haskell splice expansion.
- [ ] Integrate via Rasika app wiring only.
- [ ] Run the full test/CI/package-boundary gates.
- [ ] Decide in-process versus helper process only after measuring the actual deployment constraint.

## 13. Final architectural call

Sage should remain **one standalone package with internal feature modules and a strict GHC adapter boundary**. Complete the current foundation first, then smart-fill edits and unification explanations, then semantic refactoring, then Core/STG and splice support. This is the narrowest path to the specified Sage capability without turning a solo project into a multi-package research effort.


---

## Appendix A — Original Rasika specification passages

### A1. Insight module specification (verbatim)

### Module 4: Insight (package `hinsight`, root `HInsight`)

**Purpose:** answer questions that need GHC internals beyond what HLS offers.

**Contains**
- The GHC-API layer that loads a project with the given GHC
- Unification-chain explanation: why GHC inferred a type, stepped through
- Type-flow along `.`, `$` and `>>=` pipelines
- Typed holes: GHC's fits gathered, ranked by fit, locality and usage, with the edit that accepts one
- Semantic refactoring: change a type and propagate it, extract a function with an inferred signature, move definitions between modules
- Compiler internals: Core, STG, specialization, missed-optimization and likely space-leak warnings
- Template Haskell splice expansion

**Gives the app:** explanation steps, annotations, ranked candidates, edits.
**Does not:** anything HLS already provides (Language).
**Useful on its own as:** a library that answers "why did GHC infer this?" and "what does this become?".
**Risk:** this is the highest-risk module. It should be the first thing the early spike exercises.

### A2. Feature ownership boundaries (verbatim)

## 3. Where two modules could have claimed the same feature

| Feature | Owner | Why the other module does not have it |
|---|---|---|
| Diagnostics while typing versus from builds | Language (live, per file, via HLS) and Runner (real `cabal build`) | Different engines at different moments. The app merges them; neither implements the other. |
| Typed holes | Insight | Language does not expose HLS's hole action, so there is one ranking and one accept-edit. |
| Declared versus inferred signatures | Language | HLS already supplies the inferred type. Insight uses inferred types only inside its own refactoring. |
| Evaluate in place (`>>>`) | Language | It is an HLS feature. Runner's GHCi is for interactive sessions only. |
| `.cabal` completion and validation | Language | `cabal check` runs in Runner. The add-dependency edit is Library's. |
| Hoogle | Toolchain installs it, Library uses it | One installs, one queries. |
| Extension names versus their explanations | Language completes, Library explains | The app joins them by key. |
| Rename versus move definition | Language renames, Insight moves between modules | HLS has rename. It has no cross-module move. |
| Error-code explanations | Library | Tadka and tadkaGhc receive the text from the app. |
| New project and migration | Toolchain | It already owns the recommended GHC and the Stackage snapshot. Workspace does not scaffold. |
| Workspace trust | Workspace | Toolchain creates projects. The app records them as trusted. |
| Terminal versus Runner output | Workspace emulates, Runner produces data | Runner talks to GHCi over pipes and never needs a terminal. |
| Profiling | Runner collects and parses | The app and Basilisk draw the views. |
| Hover docs versus documentation browser | Language (hover via HLS), Library (offline browser) | Live and in-context, versus browsable and offline. |
| Self-update | Toolchain | It already holds signing, download and install logic. |

---

### A3. Features that remain in the Rasika app (verbatim)

## 4. What stays outside the six modules

**Basilisk (assumed to cover):** modal editing, motions, operators, registers, marks, macros, multi-cursor, undo tree, buffers and buffer list, splits, panes and tabs, quickfix list, file I/O, crash recovery and autosave, clipboard including over SSH, large-file handling.

**Rasika app:**
- Default two-window layout and the Hello Rasika first launch
- Keymaps, default Cabal keystrokes, leader menu
- First-run tour and interactive tutor flow (content comes from Library)
- Merging HLS and build diagnostics, error deltas, the "add a signature" tip
- "Explain this" routing: Library for static knowledge, Insight for computed explanations
- Drawing profile, coverage, Core and other views through Basilisk, Tadka and the Theme
- Handing tool paths and project information from Toolchain to every other module

---

### A4. Rules every module follows (verbatim)

## 6. Rules every module follows

1. **Headless.** No module draws anything.
2. **No module imports another.** Only the Rasika app depends on all of them, so any module can be built, tested and used alone.
3. **Inputs are arguments.** Tool paths, project root and settings come in as parameters. Only Toolchain knows where tools live.
4. **Outputs are typed data and event streams.** The app adapts one module's output into another's input.
5. **Each module supervises its own external processes** (HLS in Language, GHCi and Cabal in Runner, git in Workspace, ghcup in Toolchain) and recovers from their crashes.
6. **Each is testable without the app**, and each has a small standalone front end that shows what it does on its own.
7. **Boundary test:** if HLS already provides it, it is Language. If it needs GHC internals beyond HLS, it is Insight.

---
