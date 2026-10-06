# To every lane: wrecks break in stages (`lane/sim/wreck-decay`, not landed)

The owner (2026-09-28, `decisions.md`): a vehicle's carcass must take damage, break further and finally disappear,
and each stage changes the game. The SIM work is on `lane/sim/wreck-decay` (worktree `githubtest-wreck-sim`). Its
first commit is the seam, alone and inert; the steps after it make wrecks take harm.

| Surface | Change |
|---|---|
| Replay format | v25 -> **v26** (the seam), more with each step. If your lane bumps the format too, the later lane renumbers. |
| `PropKind` | `BrokenWreck` 6, `Scrap` 7, `Cleared` 8, appended. `PropRules.Next`: Wreck -> BrokenWreck -> Scrap -> Cleared. A cleared prop keeps its index in `MapData.Props`. |
| Cover and blocking | Wreck 50 % and blocks; BrokenWreck 35 % and blocks; Scrap 15 %, does not block; Cleared nothing. |
| Events | `PropWorn` appended after `RocketFired` (a = prop, b = 0 blast / 1 wear, scalar = share of the stage left). `PropChanged` carries each stage change (dir zero after the first). `VehicleCrushed.b = 3` = wreckage. |

For the SHOW lanes: every `switch` on `PropKind` must learn the new kinds before the steps that produce them land
(`BattlefieldComposer*.cs`, `GreyboxTerrainView.cs`, `TankRenderer.cs`'s wreck link, `CombatFx.cs`'s PropChanged
handler). `lane/show/wreck-stages` does that and draws the stages; land it the same day as the first behaviour step,
or a broken wreck is an invisible blocker.

Delete this note once the lane has landed and every lane has rebased over it.
