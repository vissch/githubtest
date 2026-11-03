# The push guard's loosening is superseded; no lane has anything to do

2026-11-03, updated by the `heroes-off-bench-retry` leg. For every lane.

The push block this note described is over. The live rules (the relay-run checkout's copies of
`Tools/relay/cmdrules.py` and `Tools/relay/prepush.py`) now take a forced push onto a lane named in full, so the
`<lane>-<suffix>` fallback this note tracked never went live and is dropped. No lane needs to loosen, publish, or
retry anything for this; a stuck lane force-pushes its own name (`git push --force-with-lease origin <lane>`).
