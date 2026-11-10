#!/bin/sh
# Project checks that need more than one build.
#
#   sh scripts/check.sh compat [--install] [--test] [VERSION...]
#       Build (and with --test, test) with each GHC version. Versions come from
#       the arguments, else the GHC_VERSIONS variable, else scripts/ghc-versions.
#       --install fetches a missing compiler with ghcup.
#   sh scripts/check.sh dead
#       Build, then run weeder over every package's HIE files.
#   sh scripts/check.sh bench [save]
#       Run the benchmark, rank it, and compare with the saved baseline if there
#       is one. "save" keeps this run as the baseline.
#   sh scripts/check.sh profile
#       Build the benchmark with profiling, run it, and show the cost centres
#       that take the most time and allocation.
#   sh scripts/check.sh all
#       compat, dead and bench, in that order.
#
# Variables: SLOWER (percent slower than baseline that counts as a regression,
# default 20), WEEDER_VERSION (default 2.11.0), GHC_VERSION (the compiler used
# for dead and profile, default the one in cabal.project).
set -u

OUT=.check
SLOWER=${SLOWER:-20}
WEEDER_VERSION=${WEEDER_VERSION:-2.11.0}
HIE_DIRS=".hie hinsight-tadka/.hie hinsight-cabal/.hie"

[ -f hinsight.cabal ] || { echo "run from the hinsight folder"; exit 2; }
mkdir -p "$OUT"
PATH="$HOME/.ghcup/bin:$HOME/.local/bin:$PATH"

default_ghc() {
  if [ -n "${GHC_VERSION:-}" ]; then
    echo "$GHC_VERSION"
  else
    sed -n 's/^with-compiler: *ghc-//p' cabal.project | head -n 1
  fi
}

versions_from_file() {
  grep -v '^[[:space:]]*\(#\|$\)' scripts/ghc-versions
}

compat() {
  install=no; tests=no; list=""
  for a in "$@"; do
    case $a in
      --install) install=yes ;;
      --test) tests=yes ;;
      *) list="$list $a" ;;
    esac
  done
  [ -n "$list" ] || list=${GHC_VERSIONS:-$(versions_from_file)}
  failed=0; summary=""
  for v in $list; do
    if ! command -v "ghc-$v" >/dev/null 2>&1; then
      if [ "$install" = yes ] && command -v ghcup >/dev/null 2>&1; then
        ghcup install ghc "$v" || true
      fi
    fi
    if ! command -v "ghc-$v" >/dev/null 2>&1; then
      summary="$summary\n  $v  MISSING  (install it, or pass --install with ghcup available)"
      failed=1
      continue
    fi
    log="$OUT/compat-$v.log"
    echo "== GHC $v: building (log: $log)"
    if cabal build all --with-compiler="ghc-$v" --builddir="dist-newstyle/compat-$v" >"$log" 2>&1; then
      result=PASS
      if [ "$tests" = yes ]; then
        if cabal test all --with-compiler="ghc-$v" --builddir="dist-newstyle/compat-$v" >>"$log" 2>&1; then
          result="PASS (tests too)"
        else
          result="FAIL (tests)"
        fi
      fi
    else
      result="FAIL (build)"
    fi
    case $result in
      FAIL*)
        failed=1
        summary="$summary\n  $v  $result"
        grep -A6 -m 3 ' error:' "$log" | sed 's/^/        /' > "$OUT/compat-$v.first-errors"
        summary="$summary\n$(cat "$OUT/compat-$v.first-errors")"
        ;;
      *) summary="$summary\n  $v  $result" ;;
    esac
  done
  echo
  echo "Compatibility:"
  printf '%b\n' "$summary"
  return $failed
}

ensure_weeder() {
  command -v weeder >/dev/null 2>&1 && return 0
  echo "== installing weeder-$WEEDER_VERSION (once)"
  tmp=$(mktemp -d)
  # Outside the project so the project's freeze file does not constrain weeder.
  ( cd "$tmp" && cabal install "weeder-$WEEDER_VERSION" --with-compiler="ghc-$(default_ghc)" \
      --installdir="$HOME/.local/bin" --overwrite-policy=always ) || return 1
  command -v weeder >/dev/null 2>&1
}

dead() {
  echo "== building so HIE files are current"
  cabal build all || return 1
  args=""
  for d in $HIE_DIRS; do
    n=$(find "$d" -name '*.hie' 2>/dev/null | wc -l)
    if [ "$n" -eq 0 ]; then
      echo "no .hie files in $d: weeder would find nothing, so it would pass without looking"
      return 1
    fi
    args="$args --hie-directory $d"
  done
  ensure_weeder || { echo "weeder is not available"; return 1; }
  echo "== weeder"
  # shellcheck disable=SC2086
  weeder $args | tee "$OUT/weeder.txt"
  s=$?
  [ "$s" -eq 0 ] && echo "Dead code: none"
  return $s
}

# Rank a tasty-bench CSV; with a baseline, list the changes, worst first.
report() {
  python3 - "$1" "${2:-}" "$SLOWER" <<'PY'
import csv, sys
def load(path):
    out = {}
    with open(path, newline="") as h:
        for row in csv.reader(h):
            if len(row) < 3 or row[0] == "Name":
                continue
            out[row[0]] = (float(row[1]), float(row[2]))
    return out
def ns(ps):
    ns_ = ps / 1000.0
    for unit, k in (("s", 1e9), ("ms", 1e6), ("us", 1e3)):
        if ns_ >= k:
            return "%.2f %s" % (ns_ / k, unit)
    return "%.0f ns" % ns_
now = load(sys.argv[1])
base = load(sys.argv[2]) if sys.argv[2] else {}
limit = float(sys.argv[3])
print("Slowest benchmarks:")
for name, (mean, dev) in sorted(now.items(), key=lambda kv: -kv[1][0])[:10]:
    print("  %10s  (+-%s)  %s" % (ns(mean), ns(dev), name))
if base:
    rows = []
    for name, (mean, dev) in now.items():
        if name in base and base[name][0] > 0:
            rows.append(((mean / base[name][0] - 1) * 100, name, base[name][0], mean))
    rows.sort(reverse=True)
    print("\nChange against the baseline (positive is slower):")
    bad = 0
    for pct, name, before, after in rows:
        flag = ""
        if pct > limit:
            flag = "  <-- REGRESSION"
            bad += 1
        print("  %+7.1f%%  %s -> %s  %s%s" % (pct, ns(before), ns(after), name, flag))
    new = sorted(set(now) - set(base))
    gone = sorted(set(base) - set(now))
    for name in new:
        print("  new benchmark: " + name)
    for name in gone:
        print("  not run now:   " + name)
    sys.exit(1 if bad else 0)
PY
}

bench() {
  save=no
  [ "${1:-}" = save ] && save=yes
  csv="$OUT/bench.csv"
  echo "== running benchmarks"
  cabal bench hinsight-bench --benchmark-options="--stdev 20 --csv $csv" || return 1
  [ -f "$csv" ] || { echo "no results were written to $csv"; return 1; }
  base=""
  [ -f "$OUT/baseline.csv" ] && base="$OUT/baseline.csv"
  report "$csv" "$base"
  s=$?
  [ -n "$base" ] || echo "(no baseline yet: run 'sh scripts/check.sh bench save' on a good commit)"
  if [ "$save" = yes ]; then
    cp "$csv" "$OUT/baseline.csv"
    echo "baseline saved to $OUT/baseline.csv"
  fi
  return $s
}

profile() {
  echo "== building the benchmark with profiling (first run builds profiling libraries)"
  rm -f hinsight-bench.prof "$OUT/profile.prof"
  cabal bench hinsight-bench --with-compiler="ghc-$(default_ghc)" --enable-profiling \
    --builddir=dist-newstyle/profile --ghc-options="-fprof-auto-exported -rtsopts" \
    --benchmark-options='+RTS -p -RTS' || return 1
  prof=$(find . -name 'hinsight-bench.prof' -not -path './dist-newstyle/*' | head -n 1)
  [ -n "$prof" ] || { echo "no .prof file was written"; return 1; }
  mv "$prof" "$OUT/profile.prof"
  echo
  echo "Cost centres, most expensive first (full report: $OUT/profile.prof):"
  awk '/COST CENTRE/ {on=1} on && n<28 {print "  " $0; n++}' "$OUT/profile.prof"
}

cmd=${1:-}
[ $# -gt 0 ] && shift
case $cmd in
  compat) compat "$@" ;;
  dead) dead ;;
  bench) bench "$@" ;;
  profile) profile ;;
  all)
    rc=0
    compat "$@" || rc=1
    dead || rc=1
    bench || rc=1
    exit $rc
    ;;
  *) sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
