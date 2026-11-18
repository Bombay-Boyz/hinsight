#!/bin/sh
# Spike: can a program that links the ghc library be a single static binary?
#
#   sh scripts/spike-link.sh 2>&1 | tee spike-link.out
#
# Builds a minimal program that links the ghc library, parses dynamic flags and
# typechecks one module, in three ways (dynamic, statically linked Haskell
# libraries, fully static), and records for each: whether it links, whether it
# runs, its size, how long a cold run takes, and what it needs at runtime.
# Then each binary typechecks a module with a Template Haskell splice, which
# makes GHC load code at run time (the splice pass of hinsight does this), and
# the script reports whether a musl toolchain is available. Nothing in the
# repository is changed.
set -u

PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"
if [ -n "${GHC_BIN:-}" ]; then
  GHC=$GHC_BIN
else
  VERSION=$(head -n 1 scripts/ghc-versions 2>/dev/null)
  if command -v "ghc-$VERSION" >/dev/null 2>&1; then GHC=ghc-$VERSION; else GHC=ghc; fi
fi
command -v "$GHC" >/dev/null 2>&1 || { echo "no compiler found: $GHC"; exit 2; }
LIBDIR=$($GHC --print-libdir)
echo "compiler: $GHC ($($GHC --numeric-version)), libdir: $LIBDIR"
echo "musl-gcc: $(command -v musl-gcc || echo none)"
echo "platform: $(uname -sm), libc: $(ldd --version 2>&1 | head -n 1)"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
cd "$WORK" || exit 2

cat >Main.hs <<'HS'
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import GHC
import System.Environment (getArgs)

main :: IO ()
main = do
  libdir : file : extra <- getArgs
  runGhc (Just libdir) $ do
    logger <- getLogger
    flags0 <- getSessionDynFlags
    (flags, _, _) <- parseDynamicFlags logger flags0 (map noLoc (if null extra then ["-fno-code"] else extra))
    _ <- setSessionDynFlags flags
    target <- guessTarget file Nothing Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    graph <- depanal [] False
    case mgModSummaries graph of
      summary : _ -> do
        parsed <- parseModule summary
        _ <- typecheckModule parsed
        liftIO (putStrLn "typechecked")
      [] -> liftIO (putStrLn "no module")
HS
printf 'module T where\n\nanswer :: Int\nanswer = 41 + 1\n' >T.hs
printf '{-# LANGUAGE TemplateHaskell #-}\nmodule S where\n\nimport Language.Haskell.TH\n\nseven :: Int\nseven = $(litE (integerL 7))\n' >S.hs

attempt() {
  label=$1; shift
  echo
  echo "=== $label"
  rm -rf "build-$label"; mkdir "build-$label"
  if "$GHC" -package ghc -outputdir "build-$label" -o "bin-$label" "$@" Main.hs >"link-$label.txt" 2>&1; then
    echo "links: yes"
  else
    echo "links: NO"
    tail -n 15 "link-$label.txt"
    return
  fi
  echo "size: $(wc -c <"bin-$label") bytes"
  if ldd "bin-$label" >"ldd-$label.txt" 2>&1; then
    echo "needs at runtime (ldd):"; sed 's/^/    /' "ldd-$label.txt"
  else
    echo "ldd: $(head -n 1 "ldd-$label.txt")"
  fi
  start=$(date +%s%N)
  if "./bin-$label" "$LIBDIR" T.hs >"run-$label.txt" 2>&1; then
    end=$(date +%s%N)
    echo "runs: yes ($(cat "run-$label.txt")), cold start and typecheck: $(( (end - start) / 1000000 )) ms"
  else
    echo "runs: NO"
    tail -n 10 "run-$label.txt"
    return
  fi
  mkdir -p "splice-$label"
  if "./bin-$label" "$LIBDIR" S.hs -fobject-code -outputdir "splice-$label" -v0 >"splice-$label.txt" 2>&1; then
    echo "runs a Template Haskell splice: yes ($(tail -n 1 "splice-$label.txt"))"
  else
    echo "runs a Template Haskell splice: NO"
    tail -n 12 "splice-$label.txt"
  fi
}

attempt dynamic
attempt static-haskell -static
attempt fully-static -static -optl-static -optl-pthread

echo
echo "=== musl"
if command -v musl-gcc >/dev/null 2>&1; then
  echo "musl-gcc is installed; a musl-linked compiler is still needed to link the ghc library against musl"
else
  echo "musl-gcc is not installed"
fi
if command -v ghcup >/dev/null 2>&1; then
  echo "ghcup lists these compilers with musl in the name:"
  ghcup list -t ghc 2>&1 | grep -i musl | head -n 5 || true
  echo "(none above means ghcup offers no musl compiler; ghc-9.10.3 on Alpine, or a static bindist, would be needed)"
fi

echo
echo "=== a clean run reading only the compiler's own library folder"
echo "the program is given the folder as an argument and reads its .hi files:"
ls "$LIBDIR" | head -n 5
echo "done"
