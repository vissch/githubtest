#!/bin/bash
# GPU probe in the held frame: new, old, new, old, new (living _TW_LIMBCUT off/on; fallen on throughout).
export PATH="$PATH:/c/Users/thomas.visscher_magi/AppData/Local/unity/bin"
SP="$(cd "$(dirname "$0")" && pwd)"   # gpu_probe.cs and limb_toggle.cs sit next to this script
cd /c/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d
ev() { unity command --timeout 60 --result-only eval "$1" 2>/dev/null | grep -o '"result":"[^"]*"'; }
evf() { unity command --timeout 60 --result-only eval "$(cat "$1")" 2>/dev/null | grep -o '"result":"[^"]*"'; }
ev 'UnityEditor.EditorPrefs.SetString("tw.gpuprobe", ""); return "cleared";'
k=0
for living in 0 1 0 1 0; do
  k=$((k+1)); name=$([ $living = 1 ] && echo "old (living clip) #$k" || echo "new (no clip) #$k")
  ev "UnityEditor.EditorPrefs.SetInt(\"tw.limb.living\", $living); UnityEditor.EditorPrefs.SetInt(\"tw.limb.fallen\", 1); UnityEditor.EditorPrefs.SetString(\"tw.gpuprobe.label\", \"$name\"); UnityEditor.EditorPrefs.SetInt(\"tw.gpuprobe.frames\", 300); return \"set\";" >/dev/null
  evf "$SP/limb_toggle.cs"
  evf "$SP/gpu_probe.cs"
  for i in $(seq 1 60); do s=$(ev 'return UnityEditor.EditorPrefs.GetString("tw.gpuprobe.state");'); case "$s" in *done*|*lost*) break;; esac; done
  echo "$s"
done
unity command --timeout 20 --result-only eval 'return UnityEditor.EditorPrefs.GetString("tw.gpuprobe");' | python -c "import sys,json; print(json.load(sys.stdin)['result'])"
