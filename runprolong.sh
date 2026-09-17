#!/usr/bin/env bash
# Restart WSLg's compositor only if it fell back to copy mode (invisible windows)
if [ "$(grep -o 'use_gfxredir = [01]' /mnt/wslg/weston.log | tail -1)" = "use_gfxredir = 0" ]; then
  echo "WSLg is in copy mode; restarting weston..." >&2
  wsl.exe -d "$WSL_DISTRO_NAME" --system -- sh -c 'kill $(pgrep -x weston)'
  sleep 3
fi
cd "$(dirname "$0")" && exec scryer-prolog prolong.pl
