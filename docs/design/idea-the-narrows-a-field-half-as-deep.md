# The Narrows: a field half as deep

Design page for board item `idea-the-narrows-a-field-half-as-deep`, game-design stage, 2026-10-08. Code paths are under `trench-warfare-3d/Assets/_Project/`. Every "today" below names the file it was read in. Sketch: `evidence/idea-the-narrows-a-field-half-as-deep/game-design/shown.jpg`.

## 1. The rule
The Narrows is a battlefield as wide as the Shelled Wood (90 m) and half as long (120 m against 240 m). Nothing else about the game changes: same men, same weapons, same silver, same orders. Because the whole layout is drawn to the field's length, the two front trenches end up 50 m apart instead of 100 m, each reserve trench sits 20 m behind its front trench instead of 40 m, and a man who goes over the top is at the enemy's wire in the time it used to take him to reach the middle. It is an infantry field: no machines are brought to it.

## 2. What the player sees and does (standard view)
1. Match start: his men walk 30 m from the spawn to the front trench (64 m today). Both front lines, both wire belts and the ground between are in one view; no river.
2. He looks across: the enemy's parapet lamps are close. Rifles in the two front trenches are inside half their range of each other, so the lines trade fire from the first second.
3. He orders `>>`. Riflemen clear the gap in about 17 s, assault men in about 12 s. The first bombs fly as they pass the enemy's wire (11 m out, the bomb reaches 17.6 m).
4. He calls an HE barrage on the enemy trench: its 25 m disc now covers a large part of no man's land too, so he must hold his own wave back or lose it. A creeping barrage (60 m of lifts) started at his own parapet walks through the enemy's front line.
5. A front trench falls. The reserve trench is 20 m behind it: the attack carries on at once or is thrown back at once. Matches should end sooner here than on any other field.

## 3. Numbers (all placeholders; the balance simulator sweeps them)
| Number | Value | Reasoning |
|---|---|---|
| Length | 120 m | The front lines stand at `FrontAt` 140 of a 480 m drawing, scaled by Length / 480 (`Sim/Terrain/BattlefieldGenerator.cs`, lines 107-134): gap = Length x 200/480. 240 m gives 100 m (Shelled Wood, Winter Line), 170 m gives 71 m (Landing), 120 m gives 50 m. |
| Width | 90 m | The Shelled Wood's (same file, line 42), so only depth changes; lanes and garrison thinness stay as decided. |
| River, Sea | off, off | The river's half width never scales below 6 m and wanders to 1.45 times that (same file, lines 355-358): up to 17 m of water in a 50 m gap. `Sea = false` as on the Winter Line: no boats. |
| Forest, Shelling, Mud, WaterLevel, Wrecks | 0.35, 0.7, 0.5, 0.15, 1 | Shelled Wood's values, with less wood so the far trench reads, and one wreck (the wreck band is 35 m deep here). Salvos = Shelling x 0.00075 x W x L = 5 (11 on the Shelled Wood). |
| Crossing time | 17 s rifle, 12 s assault, 23 s machine gunner | 50 m at 3.0, 4.2 and 2.2 m/s (`Sim/Core/RosterEntry.cs`, lines 58-60), flat dry ground, my arithmetic; half of 33, 24 and 45 s. |
| Rifle fire trench to trench | falloff x1.0 (x0.54 today) | Rifle reach 130 x 0.8 = 104 m; falloff is 1 inside half range, 0.5 at full (`Sim/Combat/CombatTables.cs`, lines 18, 125, 133-138). 50 m is inside half range; 100 m is almost the end. |
| Silver | 300 start, 2 per second | Unchanged (`Presentation/Core/MatchLaunch.cs`, lines 49-50). The sweep may want less per second on a field this short. |

## 4. Edge cases
1. **Machines.** Spawn to the enemy's front trench is about 80 m here (spawns are a fixed 6 m from each end, generator lines 214-215). Every machine gun reaches 110 to 155 m (`Sim/Combat/TankSpec.cs`), so the decision of 2026-10-01 ("no machine reaches the enemy's front trench from its spawn") cannot hold. Hence no machines; see question 1.
2. **The spawn under fire.** A machine gunner (136 m) in his front trench reaches the enemy spawn at about 80 m with falloff x0.9. Today the spawn is 164 m away and out of reach. Reinforcements are shot as they arrive; decisions.md ("After dead ground") already records 17 of 23 men killed walking up by machine guns.
3. **Trenches that wander into each other.** Each line jogs up to 3 cells of 2 m either way (generator lines 148-158; `Sim/Terrain/MapData.cs` line 13), and none of that scales. Worst case: front lines 38 m apart, reserve 8 m behind front. The wire stands 8 to 18 m out and up to 3.9 m thick, unscaled (lines 272-275): two belts can come within 2 m of each other. The builder must check seeds and reject any where belts meet or two trenches touch.
4. **The rear.** The HQ objective line is at 20 x Length/480 = 5 m (line 210), one metre behind the spawn at 6 m; on the Shelled Wood it is at 10 m. Whether an HQ can still be taken and a deploy still lands: not verified, the builder checks in Play.
5. **Support fire is danger close.** HE barrage: disc radius 25 m, each shell 8 m; creeping barrage: 10 lifts of 6 m, safe 15 m behind (`Sim/Match/OffMapAbilities.cs`, lines 128-135). Nothing is changed; the player learns to hold his wave.
6. **3,000 units.** Half the ground for the same men: twice the density in four trenches 90 m long. The bench's `ground=` knows only `forest` and `winter` (`Perf/BenchOptions.cs`, lines 153-154); it needs this field before a perf claim is made.
7. **Replay.** A replay stores the params' values, not a preset's name (`Sim/Core/Replay.cs`, line 130), and the same params give the same map hash (generator, header comment). An old replay is untouched; a Narrows replay needs nothing new.
8. **The enemy script and the assault ladder** were measured over a 100 m crossing (tasks.md, the scripted enemy). At half the time under fire the three-to-one ratio will not hold; it is measured here, not assumed.

## 5. What it needs
- **Sim:** no rule, no event, no replay version (`ReplayRecorder.FormatVersion` stays 37), no archetype id, no ability id. Only the params above. `BattlefieldParams` has public fields, so the values can be set in `MatchLaunch.Field` on the show lane; a fourth preset `BattlefieldParams.Narrows` beside the three is tidier but touches a Sim file. The generator's constants are not touched, so the three existing maps keep their hashes.
- **Sim tests:** a BattlefieldTests case over several seeds for edge cases 3 and 4.
- **Balance:** the Narrows as a field in `Tools/sweep.py`, script against script, both seats; goal is 40 to 60 % and matches that end (goals.md).
- **Show:** `Ground.Narrows = 3`, appended (the enum is saved in mission cards; zero must stay the Shelled Wood, `MatchLaunch.cs` lines 11-22); its case in `MatchLaunch.Field`; a fourth tab in `UI/Shell/ProvingGroundLaunchScreen.cs` (`GroundNames`, `Grounds`); a `ground=` name in `Perf/BenchOptions.cs`. `BiomeProfile.ForGround` already gives every ground but the Winter Line the night mud.
- **Show, not found:** how the briefing stops the player from putting a machine in his ten for one field. `MatchLaunch.Request` has `LoadoutA` / `LoadoutB` and `PeerDeploysTanks` (false by default); a per-field lock I did not find.
- **Seam:** one, small: the `Ground` enum gains a value. No file format changes.

## 6. What it must not break
- Decision 2026-10-01: no machine duels from its spawn (kept by fielding none).
- Decision 2026-09-28: men deploy across the whole width (width unchanged), and no constant bombardment (`MatchLaunch.Apply` sets it to 0).
- Decision 2026-09-21: nothing ruled, fixed seed and params per mission, night by default.
- The hashes of the Shelled Wood, the Winter Line and the Landing; the six rules of "Spread and the fight".
- Readability: env-scoreboard.md's standard view (zoom 30) must still show men, lines and orders at a glance.

## 7. Open to the owner
1. **Machines on the Narrows?** Default: none, infantry only. Other answer: allow them and accept that they duel from the spawn on this one field.
2. **Squeeze the rear too?** Default: yes, the whole layout at half length (buildable now, no generator change). Other answer: keep each rear at today's depth and halve only no man's land (190 m long; needs a new generator dial first, which adds a field to the params a replay stores).

Cut: river, sea and boats, machines, a campaign mission, a new look, any rule or number change to units and abilities.
