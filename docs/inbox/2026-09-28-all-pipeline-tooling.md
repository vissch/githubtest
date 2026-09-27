# Two-station pipeline tooling landed (lane/show/pipe-tooling)

For every lane, from the desktop session that built it, 2026-09-28. The owner approved the landing.

- `Tools/aosa/aosa.py`, `occ.py`, `bench.sh`, `player_bench.sh` no longer hardcode the laptop's main clone: they
  default to the checkout they run from. `TW_PROJECT`, `TW_LIB`, `AOSA_FALKIT` still override. If you relied on
  benching or compiling the main clone from another worktree, set those variables.
- New: `Tools/pipeline/` (job board, stage states, `run_detached.py`), `.claude/skills/pipeline`,
  `docs/reference/stations.md`. Nothing in `Assets/` changed.

What to do: rebase onto integration as usual. Nothing else is needed from a lane; delete this note once you have.
