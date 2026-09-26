#!/usr/bin/env bash
# Drive the asset playground (docs/22) in THIS checkout's editor, which must be in Play in Playground.unity.
#   pg.sh do "cmd; cmd; ..."      queue playground commands (PlaygroundHost.Do strings)
#   pg.sh shot NAME [W H]         capture the playground camera to Captures/playground/NAME.png (+ .json), wait for it
#   pg.sh report                  the host's JSON report (no image)
#   pg.sh wait SECONDS            real-time wait
set -u
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
OUT="${PG_OUT:-$(pwd -W 2>/dev/null || pwd)/Captures/playground}"; mkdir -p "$OUT"
q() {
  local code='var h = TW.Playground.PlaygroundHost.Instance; if (h == null) return "NO HOST";'
  IFS=';' read -ra A <<< "$1"
  for c in "${A[@]}"; do c="$(echo "$c" | sed 's/^ *//;s/ *$//')"; [ -n "$c" ] && code+=" h.Queue(\"$c\");"; done
  code+=' return "queued";'
  bash Tools/tw eval "$code" 2>&1 | grep -E '"result"|rror' | head -3
}
case "${1:-}" in
  do) q "$2" ;;
  shot)
    f="$OUT/$2.png"; rm -f "$f" "${f%.png}.json"
    q "shot $f ${3:-1600} ${4:-900}"
    for i in $(seq 1 40); do [ -f "${f%.png}.json" ] && break; sleep 0.5; done
    [ -f "$f" ] && echo "shot $f" || echo "NO SHOT $f"
    ;;
  report) bash Tools/tw eval 'var h = TW.Playground.PlaygroundHost.Instance; return h == null ? "NO HOST" : h.Report();' 2>&1 | grep '"result"' ;;
  wait) sleep "$2" ;;
  *) echo "usage: pg.sh do|shot|report|wait" ;;
esac
