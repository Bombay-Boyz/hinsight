# Decisions and deviations

Each entry records a rule that was relaxed or a choice that is not obvious,
what was tried first, and when to revisit it (Standard 5.9).

## 1. One module per analysis

The session analyses a single file that imports only installed packages. A
file whose dependency graph has more than one module returns
`UnexpectedModuleCount`. Loading a whole project is separate work. Revisit
when project loading is added.

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

## 6. No benchmark yet (Standard 5.6)

The only code with a size-dependent cost is `rankFits`, which is a stable
merge sort over the fits of one hole, and a test ranks 100,000 fits. A
`criterion` benchmark with a CI threshold is still owed. Add it when
`hinsight` is first used on a real project.

## 7. Dead-code check is advisory; no benchmark yet

`cabal.project.freeze` is committed. CI runs weeder, but its step is allowed
to fail until the first real report has been read and the roots in
`weeder.toml` tuned; then remove `continue-on-error` in the workflow.

## 8. Prior-art review not done (Standard 5.7)

The known bug classes of comparable projects (language-server typecheck
sessions, hole-fit tooling) have not been reviewed. Before extending this to
project loading, review their issue trackers and add a named regression test
for each applicable class.

## 9. Ranking uses fit and locality only

The module document names a third signal, usage. Nothing in the current
extraction can supply it, so it is not in the type. Add it in the change that
makes it available.

## Checks that run on every push

Build with `-Werror`, tests (including those that run a real GHC 9.10.3),
HLint with the standard's bans as errors, Ormolu 0.9.0.0 in check mode, a
no-mention-of-the-application check, and weeder (advisory). Tool versions are
pinned in the workflow's `env` block. Dependabot proposes updates to the
actions weekly.
