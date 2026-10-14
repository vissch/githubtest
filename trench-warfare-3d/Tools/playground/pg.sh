#!/usr/bin/env bash
# Drive the asset playground (docs/22) in THIS checkout's editor, which must be in Play in Playground.unity.
#   pg.sh do "cmd; cmd; ..."      queue playground commands (PlaygroundHost.Do strings)
#   pg.sh shot NAME [W H]         capture the playground camera to Captures/playground/NAME.png (+ .json), wait for it
#   pg.sh report                  the host's JSON report (no image)
#   pg.sh wait SECONDS            real-time wait
set -u
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
OUT="${PG_OUT:-$(pwd -W 2>/dev/null || pwd)/Captures/playground}"; mkdir -p "$OUT"
# Queue the commands and wait for what the host made of them. Y6: this used to answer "queued" whatever happened, so
# a typo (biome Foo, cam with 4 args) scored a capture of something else. It now prints the host's own answers and
# returns 1 when any of them is not an "ok".
res() { grep -o '"result": *"[^"]*"' | head -1 | sed 's/.*"result": *"//;s/"$//'; }
q() {
  local code='var h = TW.Playground.PlaygroundHost.Instance; if (h == null) return "NO HOST"; var from = h.Ran;'
  local n=0
  IFS=';' read -ra A <<< "$1"
  for c in "${A[@]}"; do
    c="$(echo "$c" | sed 's/^ *//;s/ *$//')"
    [ -n "$c" ] && { code+=" h.Queue(\"$c\");"; n=$((n + 1)); }
  done
  [ "$n" = 0 ] && { echo "nothing to queue"; return 1; }
  code+=' return from + " " + '"$n"';'
  local at; at="$(bash Tools/tw eval "$code" 2>&1 | res)"
  case "$at" in
    [0-9]*) ;;
    *) echo "${at:-NO HOST}"; return 1 ;;
  esac
  local from="${at% *}" ans=""
  for i in $(seq 1 40); do
    ans="$(bash Tools/tw eval 'var h = TW.Playground.PlaygroundHost.Instance; return h == null ? "NO HOST" : h.Since('"$from"', '"$n"');' 2>&1 | res)"
    case "$ans" in PENDING|"") sleep 0.5 ;; *) break ;; esac
  done
  echo "${ans:-PENDING}"
  case "${ans:-PENDING}" in "ok"*) return 0 ;; *) return 1 ;; esac
}
case "${1:-}" in
  do) q "$2" ;;
  shot)
    f="$OUT/$2.png"; rm -f "$f" "${f%.png}.json"
    q "shot $f ${3:-1600} ${4:-900}" || { echo "NO SHOT $f"; exit 1; }   # Y6: a refused line must not score the last capture
    for i in $(seq 1 40); do [ -f "${f%.png}.json" ] && break; sleep 0.5; done
    [ -f "$f" ] && echo "shot $f" || echo "NO SHOT $f"
    ;;
  report) bash Tools/tw eval 'var h = TW.Playground.PlaygroundHost.Instance; return h == null ? "NO HOST" : h.Report();' 2>&1 | grep '"result"' ;;
  wait) sleep "$2" ;;
  *) echo "usage: pg.sh do|shot|report|wait" ;;
esac
