#!/bin/sh
# Capture what a real GHC prints for the fixtures the readers parse, into
# test/golden/, and show how it differs from what is committed.
#
#   sh scripts/refresh-fixtures.sh [--check]
#
# Without --check the captures are rewritten and the difference is shown. With
# --check nothing is rewritten: the script exits 1 when GHC's output differs
# from the committed captures, which is how a GHC release that rewords a
# message is noticed. Running it twice in a row changes nothing.
#
# The compiler is ghc-VERSION, VERSION being the first line of
# scripts/ghc-versions, else plain ghc; GHC_BIN overrides both. The flags
# below are those of src/HInsight/Ghc/Trace.hs; keep them in step.
set -u

[ -f hinsight.cabal ] || { echo "run from the hinsight folder"; exit 2; }
PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"

MODE=write
[ "${1:-}" = "--check" ] && MODE=check

if [ -n "${GHC_BIN:-}" ]; then
  GHC=$GHC_BIN
else
  VERSION=$(head -n 1 scripts/ghc-versions)
  if command -v "ghc-$VERSION" >/dev/null 2>&1; then GHC=ghc-$VERSION; else GHC=ghc; fi
fi
command -v "$GHC" >/dev/null 2>&1 || { echo "no compiler found: $GHC"; exit 2; }
echo "compiler: $GHC ($($GHC --numeric-version))"

GOLDEN=test/golden
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$GOLDEN" "$WORK/new"

# capture NAME DIRECTORY FILE FLAGS...: what GHC prints for the file, with the
# scratch folder's name taken out so the capture does not depend on it.
capture() {
  name=$1; dir=$2; file=$3; shift 3
  out="$WORK/new/$name.txt"
  folder="$WORK/build-$name"
  mkdir -p "$folder"
  ( cd "$dir" && "$GHC" -outputdir "$folder" -fforce-recomp "$@" "$file" ) >"$out" 2>&1
  sed -i "s|$WORK|<scratch>|g" "$out"
  # The trace is large; the readers take the messages that start with a location.
  case $name in
    trace-*) grep "^$file:" "$out" >"$out.kept"; mv "$out.kept" "$out" ;;
  esac
}

capture splices-Use test/fixtures/splices Use.hs -fobject-code -ddump-splices -ferror-spans
capture trace-ChainPair test/fixtures ChainPair.hs -fno-code -fdefer-type-errors -ddump-tc-trace -dppr-debug -ferror-spans
capture core-Spec test/fixtures/core Spec.hs -fobject-code -O -ddump-simpl -ddump-spec -ddump-rule-firings
capture core-Spec-O0 test/fixtures/core Spec.hs -fobject-code -O0 -ddump-simpl -ddump-spec -ddump-rule-firings

if [ "$MODE" = check ]; then
  if diff -ru "$GOLDEN" "$WORK/new" >"$WORK/diff.txt" 2>&1; then
    echo "captures match what GHC prints"
    exit 0
  fi
  head -n 200 "$WORK/diff.txt"
  echo "GHC's output differs from the committed captures; see docs/bump.md"
  exit 1
fi

changed=0
for f in "$WORK"/new/*.txt; do
  base=$(basename "$f")
  if [ ! -f "$GOLDEN/$base" ] || ! cmp -s "$f" "$GOLDEN/$base"; then
    cp "$f" "$GOLDEN/$base"
    echo "updated $GOLDEN/$base"
    changed=1
  fi
done
[ "$changed" = 0 ] && echo "nothing changed"
git diff --stat -- "$GOLDEN" 2>/dev/null
exit 0
