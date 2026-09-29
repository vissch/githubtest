# To every lane: machines drive with momentum on `lane/sim/drive-momentum` (not landed)

The owner asked for vehicles that each drive "with their own speed, rhythm, style" (2026-09-28) and, on 2026-09-29,
for `lane/show/drive-style` to be taken over and built on (`decisions.md`). Its SIM half is now one seam commit on
`lane/sim/drive-momentum` (worktree `githubtest-drive-sim`), off the integration branch at aa45916, **waiting for the
owner's word to land**. `lane/show/drive-style` and `lane/sim/drive-feel` are superseded and left as they were
(nothing force-pushed); deleting them is the owner's call.

What lands with it, and what you would have to rebase over:

| Surface | Change |
|---|---|
| Replay format | v19 -> **v20** (`Sim/Core/Replay.cs`); pins in SimHashTests, FactionRosterTests, LoadoutTests. `lane/sim/front-line` also claims v20-v22 and is not landed either: the later lane renumbers and re-pins. |
| Hash chain | unchanged (the profile table's bytes change, not the chain). |
| `VehicleProfile` | + `Accel`, `Brake` (m/s per second), `PivotSpeed` (share of speed kept through a sharp turn); 0 takes the defaults 1.2 / 2.4 / 0.06. The constant `VehicleKinematicsSystem.PivotSpeed` is gone (use `prof.PivotOr`). |
| Behaviour | `Velocity` carries a machine's way from tick to tick: it gathers way and runs on to a stop. It steers at where the field leads 3-6 cells on, bent toward its own lane (`Laned`). The Breaker charges where the men are and has `ChargeSeconds` 8. |
| Presentation | nothing yet: the picture's half (drive-style's ride, recoil by calibre, machine lights, heavy machines through props) is `lane/show/vehicle-weight`, which will be based on this lane. |

If your SHOW code reads a machine's speed to animate it, expect a ramp where there was a step.
