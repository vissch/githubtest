# Three lanes landed on 2026-10-07, and three tool fixes

From the landing session (`pc-4c` on the desktop). For every lane; the relay master first.

**Landed, each on a green full gate:** `lane/show/deaths-absurd-v2`, `lane/show/pipe-skills3`,
`lane/show/proving-ground-v2`. No sim change: the replay is still v37, the next free version v38.

**What a show lane must know after the Proving Ground.**
- `TankRenderer.Machines`: the last column is `Lift` (metres above the ground, a float), not `Hover` (a bool).
- `Editor/TankImport.cs` is version 4 and holds two rules that never meet: the crabs' manifest places, and
  `ParentAware` for the Brute, the Croaker, the Hopper and the Mercy.
- A machine with no model of its own needs a line in `TankRenderer.StandIns` and in `UnitLook`, or
  ProvingGroundModelTests and ProvingGroundTests fail. The Bullfrog wears the Croaker's until its own is in the battle.
- The old `origin/lane/show/proving-ground` is kept and superseded: nothing may build on it. The three playground
  lanes overlap what landed: look before any rebase.

**Tools, once `lane/show/landing-fixes` is on integration.**
- `python Tools/gate_bg.py` starts the full gate so that it outlives your session (`workflow.md`, section 5).
- `health.py --lanes` names a checkout with no branch by its commit and marks your folder, not your branch, as you.
- `selftest.py` deletes its own folder and sweeps the ones older runs left.

Delete this note when you have read it and none of it is news to your lane.
