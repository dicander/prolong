#!/usr/bin/env bash
set -euo pipefail

SRC=${1:-rlwrap.c}
OUT=${2:-librlwrap.so}

# Ensure raylib is available via pkg-config
if ! pkg-config --exists raylib; then
  echo "raylib not found via pkg-config." >&2
  echo "Install it (Ubuntu): sudo apt install libraylib-dev" >&2
  exit 1
fi

CFLAGS="$(pkg-config --cflags raylib)"
LIBS="$(pkg-config --libs raylib)"

echo "Compiling $SRC -> $OUT"
gcc -shared -fPIC -O2 -Wall -Wextra $CFLAGS "$SRC" -o "$OUT" $LIBS

echo "Built $OUT"
ls -lh "$OUT"
echo
echo "Linked against:"
ldd "$OUT" || true
