#!/usr/bin/env bash
# seq.sh NAME FRAMES TIMESCALE ["setup commands"]: a run of stills at a slowed clock (each still takes about a second of
# real time, so at timescale 0.05 they are about 0.05 game-seconds apart) and a GIF of them, to judge motion, not poses.
# Frames and NAME.gif go to $PG_OUT (Captures/playground). The playground's clock goes back to 1 after.
set -u
HERE="$(dirname "${BASH_SOURCE[0]}")"; P="bash $HERE/pg.sh"
OUT="${PG_OUT:-$(cd "$HERE/../.." && (pwd -W 2>/dev/null || pwd))/Captures/playground}"
N="$1"; F="$2"; TS="$3"
[ -n "${4:-}" ] && $P do "$4"
$P do "timescale $TS"
for i in $(seq -w 1 "$F"); do $P shot "${N}_f$i" 800 450 >/dev/null; done
$P do "timescale 1"
python - "$OUT" "$N" <<'PY'
import sys, glob
from PIL import Image
out, n = sys.argv[1], sys.argv[2]
fs = sorted(glob.glob(f"{out}/{n}_f*.png"))
ims = [Image.open(f).convert("RGB").quantize(128) for f in fs]
ims[0].save(f"{out}/{n}.gif", save_all=True, append_images=ims[1:], duration=80, loop=0)
print(f"SEQ {n}: {len(ims)} frames -> {out}/{n}.gif")
PY
