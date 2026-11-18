#!/bin/sh
# Look for a file that typechecks but that GHC then fails to desugar or
# simplify: the case hinsight reports as CoreFailed.
#
#   sh scripts/find-corefailed.sh
#
# Each candidate is first checked with -fno-code (typecheck only), then
# compiled to object code with -O. A candidate qualifies when the first
# succeeds and the second fails. The compiler is the same as for
# scripts/refresh-fixtures.sh. Prints one line per candidate and, for a
# qualifying one, GHC's message.
set -u

PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"
if [ -n "${GHC_BIN:-}" ]; then
  GHC=$GHC_BIN
else
  VERSION=$(head -n 1 scripts/ghc-versions 2>/dev/null)
  if command -v "ghc-$VERSION" >/dev/null 2>&1; then GHC=ghc-$VERSION; else GHC=ghc; fi
fi
command -v "$GHC" >/dev/null 2>&1 || { echo "no compiler found: $GHC"; exit 2; }
echo "compiler: $GHC ($($GHC --numeric-version))"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
found=0

try() {
  name=$1
  dir="$WORK/$name"
  mkdir -p "$dir"
  cat >"$dir/C.hs"
  if ! ( cd "$dir" && "$GHC" -fno-code C.hs ) >"$dir/typecheck.txt" 2>&1; then
    echo "$name: does not typecheck"
    return
  fi
  if ( cd "$dir" && "$GHC" -O -fforce-recomp -outputdir out C.hs ) >"$dir/compile.txt" 2>&1; then
    echo "$name: typechecks and compiles"
  else
    echo "$name: TYPECHECKS, THEN FAILS TO COMPILE"
    sed 's/^/    /' "$dir/compile.txt"
    found=1
  fi
}

try rule-lambda-lhs <<'HS'
module C where
f :: Int -> Int
f x = x
{-# RULES "lam" forall x. (\y -> y) x = f x #-}
HS

try rule-literal-lhs <<'HS'
module C where
f :: Int -> Int
f x = x
{-# RULES "lit" forall x. 5 = f x #-}
HS

try rule-lhs-not-application <<'HS'
module C where
f :: Int -> Int
f x = x
{-# RULES "bare" forall x. x = f x #-}
HS

try rule-unbound-variable <<'HS'
module C where
f :: Int -> Int
f x = x
{-# RULES "free" forall x. f x = f y #-}
HS

try unlifted-top-level <<'HS'
{-# LANGUAGE MagicHash, UnliftedNewtypes #-}
module C where
import GHC.Exts
newtype N = N Int#
n :: N
n = N 3#
HS

try empty-case-void <<'HS'
{-# LANGUAGE EmptyCase #-}
module C where
import Data.Void
f :: Void -> Int
f v = case v of {}
HS

try main-wrong-type <<'HS'
module C (main) where
main :: Int
main = 1
HS

try specialise-missing <<'HS'
module C where
{-# SPECIALISE g :: Int -> Int #-}
g :: Num a => a -> a
g = (+ 1)
HS

if [ "$found" = 0 ]; then
  echo "no candidate typechecks and then fails to compile with this GHC"
fi
