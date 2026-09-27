#!/bin/bash
# Run the Windows player's PerfBench once and wait for its report. Usage: player_bench.sh <label> "<extra args>" [build dir]
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
R="/c/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d"
LABEL="$1"; EXTRA="${2:-}"; DIR="${3:-$R/Builds/WinBench}"
EXE="$DIR/TrenchWarfare.exe"
[ -f "$EXE" ] || { echo "no player at $EXE"; exit 1; }
OUT="$(cygpath -m "$SP/../runs")/$LABEL.json"; LOG="$(cygpath -w "$SP/../runs")\\$LABEL.log"
rm -f "$SP/../runs/$LABEL.json"
start=$(date +%s)
# windowed 1920x1080 at Ultra, vsync off, the same battle as the editor runs
timeout 1200 "$EXE" -screen-fullscreen 0 -screen-width 1920 -screen-height 1080 -logFile "$LOG" \
  -twbench "stress=1500 settle_ticks=1800 ticks=400 warm=120 ff=8 vsync=0 quality=5 $EXTRA label=$LABEL out=$OUT"
rc=$?
echo "$LABEL: player exit $rc after $(( $(date +%s) - start )) s"
[ -f "$SP/../runs/$LABEL.json" ] && echo "report written" || { echo "NO REPORT; log tail:"; tail -20 "$SP/../runs/$LABEL.log"; }
