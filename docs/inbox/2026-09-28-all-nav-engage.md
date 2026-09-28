# To every lane: how units move and fight changed on `lane/sim/nav-engage` (not landed)

The owner, playing the game on 2026-09-28: "units are now walking in rows in seemingly defined paths, we need them to
spread over the map instead", and "units should be attacking each other, go out of their way to attack each other."
The SIM work is on `lane/sim/nav-engage` (worktree `githubtest-nav-sim`), which sits on `lane/sim/proving-ground`
(8eea308) because both change `TargetAcquisition`, `FlowFieldManager` and the replay format. It lands after that lane,
on the owner's word.

| Surface | Change |
|---|---|
| Replay format | v17 -> **v18** (`Sim/Core/Replay.cs`). If your lane bumps the format too, the later lane renumbers. |
| Hash chain | `1108 EngageSystem;` between `1105 LeapSystem;` and `1110 MovementSystem;` (SimHashTests' pinned string). |
| Flow fields | A cell points at the neighbour its cheapest way on goes through, straight before diagonal. Open ground before a wide goal used to point north-east everywhere: a test or a capture that relied on a machine driving at 45 degrees to the enemy HQ now sees it drive straight. |
| Trench walls | Infantry cross them anywhere (`FlowField.CanStepInfantry`), at `ParapetCost`. A ladder is no longer the only way in or out. |
| Lanes | `Lane.Of(slot, generation, width)`: every man and machine keeps to his own line across the field, and men on foot are deployed on it, across the whole width. |
| The fight | `EngageSystem` (order 1108): a man in the open closes on an enemy in the open within 70 m and holds to shoot at half his weapon's range. |
| Mud | `NavCosts.Mud` 4 -> 2. |

For the SHOW lanes:
- A man who holds to shoot stands still in `Stance.Crouch`, facing his target, with `MovementSystem.Engage[slot] ==
  EngageHold`; a man closing on an enemy sprints with `EngageClose`. Both are readable from `SimHost`'s world if the
  animation wants a kneeling-fire or a charge clip for them. Nothing in SHOW has to change for the sim to run.
- Men climb out of a trench wherever they stand (`Stance.Vault` for 0.8 s, as before, but no longer only at a ladder
  prop) and drop into one anywhere.
- `ScriptedEnemy` and the stress preset deploy through `SimCommand.Deploy`, so their men come up across the whole
  width too. Bench reports recorded before this lane are not comparable with ones after it (`hash_start` differs).
- Not looked at in Play yet: the sim lane has no editor of its own. `python Tools/tracks.py` draws the tracks of the
  test battle (`tasks.md`, Movement).

Delete this note once the lane has landed and every lane has rebased over it.
