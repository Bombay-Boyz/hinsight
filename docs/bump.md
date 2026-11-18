# Moving the GHC pin

hinsight reads what one GHC version prints. A new GHC can reword a message,
rename a constructor of a diagnostic, or change a dump's layout. This is the
order of work that finds each of those.

1. **Move the pin.** Change `with-compiler` in `cabal.project`, `tested-with`
   and the `ghc` bounds in the three cabal files, and `scripts/ghc-versions`.
   Regenerate `cabal.project.freeze` if the boot packages changed.
2. **Build.** `cabal build all`. Every constructor of a GHC type that hinsight
   matches on and that changed is a compile error; fix those first. A match
   that ends in a catch-all is not an error, so step 4 is what finds those.
3. **Refresh the captures.** `sh scripts/refresh-fixtures.sh` writes what the
   new GHC prints for the splice, trace and Core fixtures into `test/golden/`
   and lists what changed. Read each difference: a changed word is a message
   to follow in the matching reader, not noise.
4. **Update the readers and their expectations.** For each difference, change
   the reader that matches the text, and the constants of its test taken from
   the captures:
   * splices: `HInsight.Splice` and `atDefault` in `SpliceSpec`;
   * chains: `HInsight.Trace`, `HInsight.Chain` and `pairTrace` in `ChainSpec`;
   * Core: the section headers matched in `HInsight.Core`, and `CoreSpec`;
   * the heading strings the readers match are listed in those modules; a
     heading GHC no longer prints is an `OtherSection` or an unread message, so
     look for new counts of unread messages in the fixtures' tests.
5. **Test.** `cabal test all`, then `sh scripts/check.sh all`. The count
   invariant (every error is explained or counted as unexplained) must hold
   over every fixture.
6. **Check the captures are stable.** `sh scripts/refresh-fixtures.sh --check`
   exits 0 and a second plain run prints "nothing changed".
7. **Kind-level steps.** If the new GHC labels steps between kinds
   itself, the printed-form judgement in `HInsight.Chain` can be replaced by
   what GHC says; if it does not, leave it.
8. **Rehearse on a throwaway branch** the first time, and record here what
   surprised you.
