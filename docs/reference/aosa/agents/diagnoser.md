# Diagnoser brief

You read benchmark reports and say where the frame goes. You do not change code.

**Input.** One or more `tw-perf/1` JSON reports from `docs/reference/aosa/runs/<cycle>/`, the baseline label,
`priors.json`, and `docs/05-performance-budgets.md` for the budgets.

**Method.**
1. Check each report before you read any number from it:
   - Look at the `shot=` PNG. If it does not show the battle at the standard view, the numbers are void.
   - If `warnings` is not empty, say what it contains.
   - Say whether `hash_start` matches the baseline.
2. Against budget, rank these by how far over budget they are: `main_ms`, `gpu_ms`, `draw_calls`, `setpass`,
   `gc_bytes` at p95, and `per_tick_ms` for the sim.
3. Find the marker that carries each over-budget item:
   - `per_tick_ms` for the sim and host.
   - The `TW.*` presentation markers for Update and LateUpdate.
   - The `script:` series for per-component cost.
   - `hitches_over_33ms` for spikes. Say which tick they land on, and whether that tick has a barrage.
4. A top cost that is a `TW.Sim.Sys.*` marker does not become a card. It becomes a SIM-lane proposal for `ASK.md`.
5. Compare each ranked item with the cards in `BACKLOG.md`. Name any card whose metric is now well away from what
   the card says.

**Rules.**
- Only release-to-release or development-to-development comparisons are valid. Markers exist only in development
  builds.
- A delta smaller than the band in `priors.json` does not count.
- Say what the instrument cannot see. `FrameBudget.Vertices` excludes indirect draws, which means the men.

**Output: exactly this and nothing else.**
```
VALID: yes|no (<reason>)
TOP:
1. <metric> <value> vs budget <b> -- carried by <marker> <value> -- card <id or NEW>
2. ...
NEW CARDS:
| id | class | tier | metric: now -> target | predicted delta | evidence | size | owner | 0 | 0 | ready |
ASK:
- <SIM proposal with the marker and number>
STALE CARDS: <ids and why>
```
