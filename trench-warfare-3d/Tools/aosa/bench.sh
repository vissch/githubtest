#!/bin/bash
# Run one editor PerfBench and wait for its report. Usage: bench.sh <label> "<extra args>"
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
R="${TW_PROJECT:-$(cd "$SP/../.." && pwd)}"   # this checkout's trench-warfare-3d unless TW_PROJECT says otherwise
export PATH="$PATH:$(cygpath -u "$LOCALAPPDATA")/unity/bin"
export UNITY_PROJECT_PATH="$(cygpath -m "$R")"   # pin `unity command` to this checkout's editor, as Tools/tw does
LABEL="$1"; EXTRA="${2:-}"
D="${AOSA_DOCS:-$R/../docs/reference/aosa}"   # the same docs dir aosa.py uses
OUTDIR="${TW_BENCH_OUT:-$D/runs/manual}"     # hand runs stay out of a cycle's folder; Tools/runs is not git-ignored
mkdir -p "$OUTDIR"
OUT="$OUTDIR/$LABEL.json"
OUTW="$(cygpath -m "$OUTDIR")/$LABEL.json"
case "$LABEL$OUTW" in *[" ,;"]*) echo "bench.sh: the label or the out path holds a space, comma or semicolon; PerfBench splits -twbench on those"; exit 1;; esac
rm -f "$OUT"
cd "$R"
for i in $(seq 1 60); do p=$(unity command --timeout 10 eval "return UnityEditor.EditorApplication.isPlaying + \" \" + UnityEditor.EditorApplication.isCompiling;" 2>/dev/null | grep -o '"result":"[^"]*"'); [ "$p" = '"result":"False False"' ] && break; sleep 3; done
unity command --timeout 60 eval "return TW.Editor.CaptureRig.Bench(\"stress=1500 settle_ticks=1800 ticks=400 warm=120 ff=8 vsync=0 $EXTRA label=$LABEL out=$OUTW\");" 2>&1 | grep -o '"result":"[^"]*"' | cut -c1-40
for i in $(seq 1 240); do [ -f "$OUT" ] && { echo "$LABEL done after $((i*5)) s, report at $OUT"; exit 0; }; sleep 5; done
echo "$LABEL TIMED OUT"; exit 1
