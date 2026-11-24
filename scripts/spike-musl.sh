#!/bin/sh
# Spike: does the program of spike-link.sh link and run on a musl system?
#
#   sh scripts/spike-musl.sh 2>&1 | tee spike-musl.out
#
# Runs scripts/spike-link.sh inside an Alpine Linux container, whose C library
# is musl, with the pinned GHC installed by ghcup. It needs Docker (or Podman,
# with DOCKER=podman) and a network connection, and it changes nothing in the
# repository or on the host. Inside the container the compiler is the first
# line of scripts/ghc-versions.
#
# Read the result for three things: whether the fully static link succeeds,
# whether the binary runs and typechecks a module, and whether it runs a
# Template Haskell splice. The platform line must say musl; if it does not, the
# run says nothing about musl.
set -u

if [ -f /etc/alpine-release ]; then
  VERSION=$(head -n 1 scripts/ghc-versions)
  apk add --no-cache bash curl gcc g++ make musl-dev gmp-dev ncurses-dev \
    ncurses-static libffi-dev xz perl git coreutils binutils >/dev/null || exit 2
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
  export BOOTSTRAP_HASKELL_MINIMAL=1
  export BOOTSTRAP_HASKELL_GHC_VERSION="$VERSION"
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh >/dev/null 2>&1
  PATH="$HOME/.ghcup/bin:$PATH"
  export PATH
  command -v ghcup >/dev/null 2>&1 || { echo "ghcup did not install"; exit 2; }
  ghcup install ghc "$VERSION" --set || { echo "ghcup could not install ghc $VERSION on this system"; exit 2; }
  exec sh scripts/spike-link.sh
fi

RUNNER=${DOCKER:-docker}
command -v "$RUNNER" >/dev/null 2>&1 || { echo "needs $RUNNER (set DOCKER=podman to use Podman)"; exit 2; }
[ -f scripts/spike-link.sh ] || { echo "run this from the repository root"; exit 2; }
exec "$RUNNER" run --rm -v "$PWD:/src:ro" -w /src alpine:3.20 sh scripts/spike-musl.sh
