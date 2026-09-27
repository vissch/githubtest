#!/bin/bash
# Run one editor PerfBench and wait for its report. Usage: bench.sh <label> "<extra args>"
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
R="/c/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d"
export PATH="$PATH:/c/Users/thomas.visscher_magi/AppData/Local/unity/bin"
LABEL="$1"; EXTRA="${2:-}"
OUTW="$(cygpath -m "$SP/../runs")/$LABEL.json"
rm -f "$SP/../runs/$LABEL.json"
cd "$R"
for i in $(seq 1 60); do p=$(unity command --timeout 10 eval "return UnityEditor.EditorApplication.isPlaying + \" \" + UnityEditor.EditorApplication.isCompiling;" 2>/dev/null | grep -o '"result":"[^"]*"'); [ "$p" = '"result":"False False"' ] && break; sleep 3; done
unity command --timeout 60 eval "return TW.Editor.CaptureRig.Bench(\"stress=1500 settle_ticks=1800 ticks=400 warm=120 ff=8 vsync=0 $EXTRA label=$LABEL out=$OUTW\");" 2>&1 | grep -o '"result":"[^"]*"' | cut -c1-40
for i in $(seq 1 240); do [ -f "$SP/../runs/$LABEL.json" ] && { echo "$LABEL done after $((i*5)) s"; exit 0; }; sleep 5; done
echo "$LABEL TIMED OUT"; exit 1
