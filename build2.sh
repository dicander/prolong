#!/usr/bin/env bash
set -euo pipefail

SRC=${1:-rlwrap.c}
OUT=${2:-librlwrap.so}

# Where a from-source raylib lands after `sudo make install` (raylib/src): /usr/local
RAYLIB_PREFIX=${RAYLIB_PREFIX:-/usr/local}

if pkg-config --exists raylib; then
  CFLAGS="$(pkg-config --cflags raylib)"
  LIBS="$(pkg-config --libs raylib)"
elif [ -f "$RAYLIB_PREFIX/include/raylib.h" ]; then
  echo "raylib not registered with pkg-config; using source install at $RAYLIB_PREFIX" >&2
  CFLAGS="-I$RAYLIB_PREFIX/include"
  # -rpath so dlopen() from Scryer finds libraylib.so without ldconfig;
  # the trailing libs are what a STATIC libraylib.a needs (harmless if it's shared).
  LIBS="-L$RAYLIB_PREFIX/lib -Wl,-rpath,$RAYLIB_PREFIX/lib -lraylib -lGL -lm -lpthread -ldl -lrt -lX11"
else
  echo "raylib not found." >&2
  echo "  Ubuntu package:  sudo apt install libraylib-dev" >&2
  echo "  From source:     cd raylib/src && make && sudo make install   (lands in /usr/local)" >&2
  echo "  Other prefix:    RAYLIB_PREFIX=/opt/raylib ./build.sh" >&2
  exit 1
fi

echo "Compiling $SRC -> $OUT"
# --no-undefined: fail here, at link time, rather than inside use_foreign_module/2 at runtime
gcc -shared -fPIC -O2 -Wall -Wextra $CFLAGS "$SRC" -o "$OUT" $LIBS -Wl,--no-undefined

echo "Built $OUT"
ls -lh "$OUT"
echo
echo "Linked against:"
ldd "$OUT" || true
