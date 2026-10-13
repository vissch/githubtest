#!/bin/bash
# Run the Windows player's PerfBench once and wait for its report. Usage: player_bench.sh <label> "<extra args>" [build dir]
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
R="${TW_PROJECT:-$(cd "$SP/../.." && pwd)}"   # this checkout's trench-warfare-3d unless TW_PROJECT says otherwise
LABEL="$1"; EXTRA="${2:-}"; DIR="${3:-$R/Builds/WinBench}"
EXE="$DIR/TrenchWarfare.exe"
D="${AOSA_DOCS:-$R/../docs/reference/aosa}"   # the same docs dir aosa.py uses
OUTDIR="${TW_BENCH_OUT:-$D/runs/manual}"     # hand runs stay out of a cycle's folder; Tools/runs is not git-ignored
mkdir -p "$OUTDIR"
OUTP="$OUTDIR/$LABEL.json"; LOGP="$OUTDIR/$LABEL.log"
OUT="$(cygpath -m "$OUTDIR")/$LABEL.json"; LOG="$(cygpath -w "$OUTDIR")\\$LABEL.log"
case "$LABEL$OUT$DIR" in *[" ,;"]*) echo "player_bench.sh: the label or the out path holds a space, comma or semicolon; PerfBench splits -twbench on those"; exit 1;; esac
[ -f "$EXE" ] || { echo "no player at $EXE"; exit 1; }
rm -f "$OUTP"
start=$(date +%s)
# windowed 1920x1080 at Ultra, vsync off, the same battle as the editor runs
timeout 1200 "$EXE" -screen-fullscreen 0 -screen-width 1920 -screen-height 1080 -logFile "$LOG" \
  -twbench "stress=1500 settle_ticks=1800 ticks=400 warm=120 ff=8 vsync=0 quality=5 $EXTRA label=$LABEL out=$OUT"
rc=$?
echo "$LABEL: player exit $rc after $(( $(date +%s) - start )) s"
if [ -f "$OUTP" ]; then
  echo "report written at $OUTP"
  exit 0
else
  echo "$LABEL NO REPORT at $OUTP; log tail:"
  tail -20 "$LOGP" 2>/dev/null || echo "(no log at $LOGP)"
  exit 1
fi
