# Prior-art review: loading more than one module (Standard 5.7)

Scope: the bug classes of comparable tools that load Haskell projects through
the GHC API, reviewed before `hinsight` loads a file together with the modules
it imports from its own project. Each class names the regression test that will
ship with the loader.

**How this was done, and its limit.** Two sources were read in full: the
`hie-bios` loading module (`HIE.Bios.Ghc.Load`, 0.12.1) and the
haskell-language-server issue about stale definitions across packages (#734).
The remaining classes come from general knowledge of how language servers
drive the GHC API and are marked *(experience)*. They have not been checked
against issue trackers one by one. A deeper pass over the `ghcide` and
`hie-bios` trackers is still owed and is not claimed here.

| # | Bug class | Source | Mitigation in `hinsight` | Regression test to add |
|---|---|---|---|---|
| 1 | A changed dependency is not reloaded, so a dependent module sees two versions of the same definition ("ambiguous occurrence") | HLS #734 | Every analysis runs in a fresh GHC session; nothing is carried between calls | Analyse a file, edit the module it imports, analyse again: the second result reflects the edit |
| 2 | Setting targets resets the previous ones, so only the named targets and their dependencies load | `hie-bios` docs | The session sets exactly one target; unrelated modules in the folder are never loaded | A folder with unrelated modules: only the target and its imports appear in the module graph |
| 3 | Reusing the recompilation check needs timestamps to be faked so that typechecked modules are produced | `hie-bios` docs | Fresh session and no code generation, so nothing is up to date and nothing is faked | Covered by test 1 |
| 4 | Diagnostics from a dependency go to GHC's log, not to the caller, and are lost or printed | *(experience)* | The loader captures them through a log action and reports them as errors of that dependency | A dependency with a type error is reported with its file name; nothing is written to standard error |
| 5 | The module name in the header disagrees with the file's place in the folder tree, or there is no header | *(experience)* | The source root is derived from the header and the path; on a disagreement the file's own folder is used, so its siblings still resolve | `sourceRoot` unit tests: hierarchical, flat, mismatched and `Main` files |
| 6 | Import cycles and boot files | *(experience)* | Whatever GHC reports for a cycle is surfaced, as an unexplained error or a failed dependency; never a crash or silence | Two modules importing each other: the result is not an empty analysis |
| 7 | An import of a package that is not installed | *(experience)* | Counted as an unexplained error; packages are not resolved in this version | A file importing a module that does not exist |
| 8 | Exceptions thrown from inside dependencies (including compile-time code) | *(experience)* | Already converted to `SessionError` at the session boundary | A dependency that throws while being typechecked |
| 9 | The same module name present in two source folders | *(experience)* | Only one source root is searched | Not tested: unreachable with one root |

## What this decides

- A fresh session per analysis stays. It is slower than a long-lived session
  but removes classes 1 and 3 by construction. Revisit only with a benchmark
  (decisions item 6) that shows the cost matters.
- The loader must capture diagnostics from dependencies (class 4) before it
  loads any dependency; without that, a broken import would look like success.
- Package resolution (class 7) is out of scope for the first loader.

# Prior-art review: the project environment (Standard 5.7)

Scope: the bug classes of tools that learn a project's packages, extensions and
compiler options from a build tool and hand them to the GHC API, reviewed before
`hinsight` does the same (`docs/phase-environment.md`). Classes continue the
numbering above.

**What was read, and what was not.**

Read in full or searched line by line:

- `hie-bios` 0.21.0: its cabal cradle (`HIE.Bios.Cradle.Cabal`, including the
  design note in its header), its types, its `ChangeLog.md` (323 lines), its
  cabal file and the names of its test projects.
- The `haskell-language-server` `ChangeLog.md` (4,643 lines), which also records
  the `ghcide` changes that ship with it. Searched for loading, flags,
  boot-file, preprocessor and Template Haskell entries.
- GHC 9.10.3's `GHC/Driver/Make.hs`, for how Template Haskell is handled when
  no code is generated.

**Not read: the issue trackers themselves.** GitHub's issue pages refuse
automated fetching, and its API needs access to the repositories that this
session should not request. Three issue titles surfaced in a web search are
listed as "title only": they were not opened. The trackers remain unread, so
the table below is the changelog-level picture, not the tracker-level one. A
person with a browser can close the gap by searching those two trackers for the
terms in the right-hand column of the "what to search" list at the end.

| # | Bug class | Evidence | Mitigation in `hinsight` | Regression test (named before the code) |
|---|---|---|---|---|
| 10 | **Capturing flags from cabal differs by cabal version.** `hie-bios` carries three mechanisms (a stand-in `ghc`, `--with-repl` for cabal and Cabal ≥ 3.15, and `cabal path`), and `--with-repl` fails in a project whose custom `Setup` uses an older Cabal. Fixes include "always specify `--with-hc-pkg`" (0.16), "use cabal path when available" (0.15), "support for cabal 3.16.1.0" (0.17) | read: `Cabal.hs` header note, ChangeLog | Do not write the capture. Depend on `hie-bios`, which tracks cabal releases. Name the cabal version in the error | `cabalCaptureFailureIsTyped`: a project with a custom `Setup` on an old Cabal gives an `EnvironmentError` that names cabal's output, never a wrong analysis (pending when no such cabal is installed) |
| 11 | **Capture clobbers the user's build directory.** `hie-bios` 0.19: "Pass `--builddir` to avoid clobbering `dist-newstyle`". Running `cabal repl` can also build dependencies, so the first capture may be slow | read: ChangeLog, `Cabal.hs` | Capture into its own build directory, never the project's | `captureLeavesProjectBuildAlone`: the file list and times of the project's `dist-newstyle` are identical before and after an analysis |
| 12 | **Wrong or ambiguous component.** A file can belong to several components (library and tests sharing a source folder) or to none. `hie-bios` added multi-component support (0.13), "keep track of not loaded files" (0.15), a `componentsToLoad` field (0.20); HLS "restrict batch session loads to files owned by the same cradle" | read: ChangeLog, HLS ChangeLog. title only: hie-bios #269 "better multi component support", #275 "Could not find module" | Never pick silently. The error lists the components the file belongs to | `fileInTwoComponentsIsReported`, `fileInNoComponentIsReported` |
| 13 | **Paths in flags are relative to the component root, not to the current directory.** `ComponentOptions` documents that every option is absolute or relative to `componentRoot`. HLS: "session-loader: set working directory on GHC 9.4+" | read: `Types.hs`, HLS ChangeLog | Make every path absolute against the component root. Never change the process's working directory (global state, Standard 1.8) | `analysisIgnoresCurrentDirectory`: the same result from two different working directories |
| 14 | **The plan was built by another compiler.** `hie-bios` 0.9.0 "use the proper GHC version given by cabal", honouring `with-compiler`. Interface files from another GHC cannot be read | read: ChangeLog | Compare the compiler cabal reports with 9.10.3 before applying anything; a mismatch is `CompilerMismatch` | `otherCompilerIsRefused`, using a stand-in compiler that reports another version |
| 15 | **User and project configuration leaks into the query.** `hie-bios` 0.9.1: "ignore `.ghci` files while querying project GHC"; its own query uses `-ignore-dot-ghci` and `-package-env=-` | read: ChangeLog, `Cabal.hs` | Inherited by depending on `hie-bios` | `dotGhciDoesNotChangeFlags`: a project whose `.ghci` would fail still analyses |
| 16 | **A project's `-Werror` changes what counts as an error.** No `hie-bios` handling found; HLS "demote home unit closure errors to warnings" is the nearest entry | experience, partly read | Rule 3.3.2 of the phase plan: warnings stay warnings | `projectWerrorKeepsErrorCount`: an unused import in a project with `-Werror` leaves the mismatch and unexplained counts unchanged |
| 17 | **Preprocessors and generated modules.** HLS documents a preprocessor limitation (#177) and fixed hashing source before preprocessing (GHC 9.4). Modules from alex, happy, hsc2hs or `Paths_pkg` exist only after cabal generates them | read: HLS ChangeLog | Flags that need `-D` or CPP are applied. A module that is missing because it is generated is counted as unexplained, never a crash. Generating modules is out of scope | `cppDefinesApply`; `generatedModuleMissingIsUnexplained` |
| 18 | **Template Haskell with no code generation.** HLS: "use object code for TH + UnboxedTuples/Sums", and several TH recompilation fixes. GHC 9.10.3's `load` calls `enableCodeGenForTH`, which gives the dependencies of a module that uses TH a bytecode backend even when the session's backend is none | read: HLS ChangeLog, `Make.hs` | Unknown whether `hinsight`'s own loading path reaches that code. Test it on real GHC before relying on it | `templateHaskellInProjectModule`: a module whose splice calls a function from another project module analyses (real GHC; this decides whether the session needs a different backend) |
| 19 | **Boot files and cycles.** HLS fixed boot-file handling several times (mark boot files to load first; account for them in dependency information; hs-boot discovered in the GHC codebase) | read: HLS ChangeLog | Class 6 above still holds; the graph leaves `{-# SOURCE #-}` out (decisions item 11) | `hsBootProjectAnalyses`: a two-module project joined by an `hs-boot` file analyses without an unexplained error |
| 20 | **A stale environment after the `.cabal` file changes.** HLS: "retry a failed cradle if the cradle descriptor changes"; `ComponentOptions` carries `componentDependencies` for exactly this | read: HLS ChangeLog, `Types.hs` | No cache in this phase: each analysis captures afresh. A cache, if added later, is keyed on `componentDependencies` | `editedCabalFileIsSeen`: change `default-extensions` between two analyses and the second reflects it |
| 21 | **Concurrency and temporary files.** `hie-bios` 0.13.1 "avoid deadlocks in multi-component support"; 0.21 "make cacheFile thread-safe using file locks", with the cache directory now passed explicitly; HLS #5004 (title only): a multi-component session failing because a temporary response file had gone | read: ChangeLog. title only: HLS #5004 | Use `hie-bios`' own cache folder, which it locks (a per-analysis temporary folder was tried and failed: the captured flags name package databases inside it, which must outlive the capture); pass `--keep-temp-files` as `hie-bios` does | `twoAnalysesAtOnce`: two simultaneous analyses of one project both succeed |
| 22 | **Unusual paths.** `hie-bios` tests cover a project folder with a space in its name, a symbolic link, and nested projects | read: test project names | Portable path handling (Standard 2.14) | `spaceInPathAnalyses`, `symlinkedFileAnalyses`, `nestedProjectUsesInnerCabalFile` |

## What this decides

- **Depend on `hie-bios`; do not write the cabal capture.** Class 10 is the
  decisive one: the capture has needed fixes for every cabal release, and the
  standard (2.11, 4.7) says to reuse a maintained implementation of a hard,
  moving target. `hie-bios` 0.21.0 declares `ghc >=9.2.1 && <10.2` and lists GHC
  9.10.1 as tested, so 9.10.3 is inside its bounds. **That it builds against
  9.10.3 here is not verified.** It is checked the first time the adapter is
  built on your machine. If it fails to build, the decision is reopened, not
  worked around.
- **The dependency lives in a separate adapter package.** `hie-bios` brings
  about two dozen packages (aeson, conduit, yaml and others). Keeping it out of
  `hinsight` core keeps core as light as it is today.
- **Rule 3.3 of the phase plan stands**, and classes 13 and 16 are its tests.
- **Class 18 is the open technical question.** It is answered by an experiment
  on real GHC at the start of step 2, before the session's backend is
  settled.

## What to search for in the trackers

Not done here. For anyone able to open them: in `haskell/hie-bios` and
`haskell/haskell-language-server`, search issues for "multi component", "Could
not find module", "response file", "Werror", "Template Haskell", "hs-boot",
"cabal repl", "builddir", "ghc-options" and "with-compiler". Add anything that
is not one of classes 10 to 22 to this table with a named test.
