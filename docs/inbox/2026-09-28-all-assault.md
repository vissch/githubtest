# To every lane: the assault can take a trench now (`lane/sim/assault-balance`)

The owner, 2026-09-28: "lets make it even better, use this to make the best game". Measured first
(`AssaultLadderTests`): a bare attack at three to one never took a trench and a failed attack cost the garrison
nobody, so the front could not move. The SIM work is on `lane/sim/assault-balance` (worktree `githubtest-nav-sim`).

| Surface | Change |
|---|---|
| Replay format | v18 -> **v19** (`Sim/Core/Replay.cs`). If your lane bumps the format too, the later lane renumbers. |
| Hash | `DirectFireSystem` now folds its per-slot bombs (same place in the chain, 700). |
| Hit chance | A running man in the open is a harder mark the farther off he is (`CombatTables.RunningTarget`). |
| Suppression | A miss at a man on the fire step suppresses him fully (the parapet). |
| Grenades | A man in the open throws one at the trench man he fights from 5-22 m: an `Impact` on `BlastSystem`, `Explosion.a == SourceId.Grenade` (2002). |
| Events | `SimEventType.GrenadeThrown` appended: a = thrower, b = target, pos = where it left his hand, dir = its flight (flat), scalar = metres. |

For the SHOW lanes:
- The bomb goes off the tick it is thrown, and today it is drawn as a shell burst (the blue splash in
  `Captures/nav-engage/play-grenade-std.png` on the sim lane's checkout). A small burst of its own, keyed on
  `SourceId.Grenade`, and a throw (an arm, an arc along `dir`) from `GrenadeThrown` would read far better. Nothing in
  SHOW has to change for the sim to run.
- **Done 2026-09-29** on `lane/show/grenade-look` (over `lane/sim/grenade-flight`, replay v21: the bomb now flies
  0.5-1.2 s and goes off where it lands): the throw, the bomb in the air with its fuse, and a burst of its own.
- Bench reports recorded before this lane are not comparable with ones after it.

Delete this note once the lane has landed and every lane has rebased over it.
