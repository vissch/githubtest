# The ground the flamethrower sets alight (2026-10-23)

Four Play captures at the game camera (`TacticalCamera.FrameFrom`, zoom 30, the zoom the game is played at),
framed on the nav cells the sim says are burning, 1.5 s after the jets are gone:

| file | tree | field |
|---|---|---|
| `before-day.png`, `before-night.png` | 5f10d4106 (the commit before the change) | Winter, NightMud |
| `after-day.png`, `after-night.png` | 36b3ede90 (BurningGround + CombatFx.BurningGround) | Winter, NightMud |

## How they were staged
GreyboxCorridor, in Play, through `Tools/tw eval`. The only thing in the game that lights a nav cell is a
flamethrower's jet (`DirectFire` → `BurningSystem.IgniteCell`), so the staging is a real one:

1. `TW.Perf.GymDirector.Attach(host)`, then `Spawn(1, InfantryArchetype.Flamethrower, …)` six times and
   `Hold(slot)` on each (`Hold` pins a unit's nav goal to its own cell, so he stands where he was put);
   eight `Rifle` of the other side the same way.
2. Everyone staged is given 99999 hp. A man dropped into this match on his own was dead in seconds and then no
   flamethrower had anything to burn.
3. Every two seconds the victims are teleported back to six metres in front of the jets: left alone they close to
   about a metre, and a man in melee does not use his weapon (measured: 1.0–1.7 m, suppression 96, not one burning
   cell in 40 s).
4. Wait for `BurningSystem.Cells` to report burning cells, frame the camera on their centre, despawn every
   flamethrower and every man alight, and take the still 1.5 s later. What is left in the picture is the ground.

`burn-report.txt` holds `CombatFx.BurnReport()` from the same session.

## What the pair does and does not prove
The probe is the proof: on the new tree every frame the sim burned a cell had a drawn flame within 0.5 m of its
centre (`covered=44971/44971 (100%) uncovered=0 late=0 scorch=62`), and on the old tree `BurnReport` does not
exist at all because nothing read `CellBurning`.

`Tools/flamecheck.py` does NOT tell this pair apart, and the plan's expectation that it would is wrong:

| capture | flamecheck (floor 15000) |
|---|---|
| `after-day.png` | OK 41546 px |
| `before-day.png` | OK 15184 px |
| `after-night.png` | OK 22275 px |
| `before-night.png` | OK 25805 px |

It counts warm and very bright pixels anywhere in the frame. In a live match that also counts other jets, men
alight elsewhere, muzzle flashes and star shells — and in the Winter field the snow itself (`lum ≥ 190`). It is a
check against a rig that framed empty ground, not a measure of this effect. Cropping to the framed cells did not
separate them either (centre 600×400: before-night 17832 px, after-night 6932 px). The EditMode test
(`TW.Tests.BurningGroundTests`, which cannot even compile against the old tree) and `BurnReport` are what tell the
two trees apart; the stills are there to be looked at.
