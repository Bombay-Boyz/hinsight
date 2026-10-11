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
