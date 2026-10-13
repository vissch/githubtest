#!/usr/bin/env bash
# bang.sh NAME KIND [W H] [STEP]: a shot taken on the EVENT, not the clock.  The playground's editor must be in Play.
# It queues the host's "bang", waits for the manifest (up to 40 s, as pg.sh shot does), then joins the five frames into
# the two-second loop: Captures/playground/NAME.gif.  KIND is one of burst, cook, muzzle, flame, dust, or any.
# No bang inside the host's wait: the manifest says "bang": false, this prints NO BANG NAME and exits 1.
set -u
HERE="$(dirname "${BASH_SOURCE[0]}")"; P="bash $HERE/pg.sh"
OUT="${PG_OUT:-$(cd "$HERE/../.." && (pwd -W 2>/dev/null || pwd))/Captures/playground}"; mkdir -p "$OUT"
N="$1"; K="${2:-any}"; W="${3:-800}"; H="${4:-450}"; STEP="${5:-0.2}"
S="$OUT/$N"
rm -f "$S.json" "$S.gif" "$S"_f*.png
$P do "bang $S.png $K $W $H $STEP" >/dev/null
for i in $(seq 1 80); do [ -f "$S.json" ] && break; sleep 0.5; done
if [ ! -f "$S.json" ]; then echo "NO BANG $N (no manifest)"; exit 1; fi
if grep -q '"bang": false' "$S.json"; then echo "NO BANG $N (no $K came)"; exit 1; fi
n="$(ls "$S"_f*.png 2>/dev/null | wc -l | tr -d ' ')"
python "$HERE/loop.py" "$S" >/dev/null || { echo "NO BANG $N (no loop)"; exit 1; }
echo "BANG $N: $n frames -> $S.gif"
