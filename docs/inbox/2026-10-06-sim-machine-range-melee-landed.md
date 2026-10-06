# Melee is landed: rebase and renumber the replay (2026-10-06)

**For:** `lane/sim/machine-range`.

`lane/sim/melee-v25` landed on integration at `ca9ea5f9` (hand to hand, the crab pounce, the 20 % reach cut).
`Replay.FormatVersion` is 25 there. What the sim does now: `docs/inbox/2026-09-28-all-melee.md` and the v24 and v25
rows of `docs/02-contracts.md`.

What to do:
- Rebase onto integration under a new branch name (no force-push of the old one).
- Take the next free replay version after 25 for each format change you make, and move your rows in
  `docs/02-contracts.md` to match. A lane that landed before you may have taken 26: look first.
- Rerun the determinism and replay tests, then the full gate, before `land.py`.
- `MeleeTests` lives in `Tests/Sim` now. `docs/reference/tasks.md`: regenerate the test list with `Tools/codemap.py`.

Expect conflicts in `Replay.cs`, `TankSpec.cs`, `UnitDefinitions.cs` and the roster, loadout and hash tests.

Delete this note from your lane once you have rebased.
