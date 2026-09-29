# Handoff: absurd deaths and the wreck life cycle (2026-09-29)

To: the next session on `lane/show/deaths-absurd` (and its two sister lanes). From: the session that built it.
Delete this note when you have taken it over.

## Where things stand

| Lane | Worktree | Tip (pushed) | State |
|---|---|---|---|
| `lane/show/deaths-absurd` (A) | `githubtest-absurd-show` | `9702c1da` | infantry and machine death gags, gibs, blood; 24 commits, gate green on the tip |
| `lane/sim/wreck-decay` (B) | `githubtest-wreck-sim` | `559cc139` | wrecks take damage in stages; automatic targeting; replay v20 |
| `lane/show/wreck-stages` (C) | `githubtest-wreck-show` | `a61661da` | staged wrecks drawn |

All three are 23 commits behind origin's integration branch and need a rebase before landing. **Nothing lands without
the owner's word.** Landing order: B, then C, then A. After rebasing B, rerun the determinism, replay and hash tests
(the replay version is set at landing).

`fx.deathAbsurd` (a developer knob) **defaults to 0**, which is exactly today's deaths. It stays at 0 until the owner
has seen captures and says so. The owner's decisions are in `docs/reference/decisions.md` (2026-09-28): slapstick plus
gore, with the GORE slider turning it down; parts cut from existing models only; wrecks damaged by explosions,
ramming, gunfire and automatic targeting; salvage value kept.

## What is built (lane A), by file

- `Presentation/Core/DeathGags.cs`: pure gag choice per death (punt, fountain, jig, pancake, and so on).
  `DeathGags.Intensity` is the knob.
- `Presentation/Camera/GibPlan.cs`: pure choice of which limbs a man loses and what flies. `PartScale` 1.8 at 1;
  `PartTop` 6 m; fewer gore lumps.
- `CombatFx.Bodies.cs` (`OwnGibs`, `ThrowGibs`, `DueGibs`): each man's parts are thrown after his own gag delay, so a
  heap goes up a beat apart. At intensity 0 the legacy `Gibs` body is byte-identical to today.
- `Presentation/Camera/VehicleGags.cs`: pure numbers and arcs for the machines, including:

  | Piece | Value |
  |---|---|
  | turret leap | 15–18 m/s up, lands 0.75–0.93 of 1.5 hull lengths out |
  | gun | snaps off the turret: `GunKeeps`, `GunOut*` |
  | hull hop | 1.5 m |
  | wheels | roll 9–12 m (`RollFar` 12) |
  | fan | glide: up 2.5–3.5 m/s, back 12–14.5 m/s |
  | fizzers | corkscrew rockets |
  | track | pay-out (`Unspool`) |
  | walker | belly-flop and splay |
  | cook-off pop | `PopGlint` 0.1 |
  | walker dust ring | `FlopRing` 8 |
- `TankRenderer.Deaths.cs`, `TankRenderer.Fizzers.cs`: the machine death gags, hooked from `TankRenderer.cs`.
  - `ClearWay` steers the turret's landing away from props, cover, blocked cells and placed scenery.
  - `PayOut` lays the track on the side with open ground.
  - `PopFlash` stops cook-off pops washing the wreck cream, but only above intensity 0.
- `SceneHooks.Standing` (`Presentation/Core/RenderGround.cs`), filled by `BattlefieldProps.Standing`: the distance from
  x, z to placed scenery a metre tall or more. The sim's map doesn't hold ruins and walls, so the camera assembly asks
  this hook instead. That avoids an asmdef edge.
- `Editor/DeathLab.cs`: stages the scenes. `Tests/Stills/DeathStills.cs`: films them. `Tests/Stills/BenchRuns.cs`: the
  perf bench from batch mode.
- Tests:
  - `VehicleDeathTests` (11), `GibPlanTests` (5), `DeathGagTests`.
  - `FigurePartsTests` needs the engine; it fails in the offline runner by design.
- Docs: `docs/reference/tasks.md` and `docs/reference/aosa/JUICE.md` (moments J10–J14).

## How to see it: film, then a blind critic

Run everything from `trench-warfare-3d`. Use the Hub editor directly
(`C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe`); the `%LOCALAPPDATA%\unity\bin\unity.exe` wrapper
hangs in batch mode.

```powershell
$env:TW_STILLS_FIELD='Winter'        # the only daylight field
$env:TW_DEATH_ABSURD='1'             # 0 films today's deaths (the A baseline)
$env:TW_DEATH_SCENES='maw,salvo,skimmer,walker'   # launch 1; launch 2: 'machine,heap'
$env:TW_STILLS_DIR="<scratch>\stills-rN"
& 'C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe' -batchmode -projectPath "<checkout>\trench-warfare-3d" `
  -runTests -testPlatform EditMode -testFilter DeathStills -testResults <out.xml> -logFile <out.log>
```

- **Scene lists.** Only four open spots exist on the Winter map, so the scenes run in two launches. Keep the lists
  identical between the knob-0 and knob-1 runs, or the scenes land on different spots.
- **Output.** Each scene writes 16 stills (12 for the heap), a contact `_sheet.png`, and a `_standard.png` at play zoom.
- **Timing.**
  - Since `137f6c03` the game clock steps exactly 1/30 s per frame while filming (`Time.captureDeltaTime`). Before
    that, each still cost about 0.3 s of game time, which misled every critic about timing.
  - A machine is shelled at 1.5 s. Its first 12 stills are 0.16 s apart from 1.4 s; the last 4 are spread out to 8 s.
- **Baselines.** The current knob-0 baseline is round 17's `stills-r17a0a` and `stills-r17a0b`. Refilm it if the
  timing or framing changes.
- **Critic bundle.**
  - Make a folder holding the `RUBRIC.md` template, the `A_<scene>_{sheet,standard}.png` baselines and the
    `B_<scene>_{sheet,standard}.png` candidates.
  - Add crops: four full-resolution tiles side by side per scene.
  - Hand the folder to a fresh general-purpose agent. Tell it to read `RUBRIC.md`, read every PNG, judge only from the
    images, read no code, and answer in the rubric's shape.
- **The rubric.**
  - It scores J10 turret leap, J11 machine comes apart, J12 walker belly-flop and J13 heap goes up, 0–10 each.
  - The /100 is their mean × 10, capped at 49 if readability drops against A.
  - Keep its numbers in step with the constants above; it now says the wheels roll 9–12 m.
- **Scores.** 33 → 45 → 50 → 50 → 43 → 58 → 60 → 62 → 60 → 65 → 60 → 63 → 60 → 63. Readability has held since
  round 2. The last six rounds sit in the critic's noise band of about ±3, so small tuning no longer moves the score.

## Lessons that cost time

- **Probe before tuning.** Two "bugs" the critic raised were the camera, not the code:
  - The turret leap was fine: 15.2 m/s, a 14.3 m peak, 2.8 s in the air. The stills just weren't 0.16 s apart.
  - The Maw's wheels did roll 15 m, out of the shot and once off the map.
  - A temporary `Debug.Log` plus one filmed launch (grep the `-logFile`) settles such questions in minutes. Remove the
    log before committing.
- **Never edit the tree while a gate or a film is running.** The gate records only a tree that stayed unchanged, and
  a film's second launch recompiles whatever you just saved.
- **Line endings.** Python edit scripts must write `newline='\n'`: source-text tests fail on CRLF. PowerShell
  `.Replace` with multi-line anchors fails on CRLF files; use the Edit tool.
- **Elevated shell.** This shell runs elevated. If the gate "times out at 600 s", check elevation first. Don't route
  commands through scheduled tasks to drop elevation.
- **Knob 0 must stay exactly today.** Every new behaviour is gated on `DeathGags.Intensity > 0`. `AtIntensityZero…`
  tests guard the man's side; for the machine's side, check by reading that each hook returns early at 0.

## Open items, most useful first

1. **Owner review.** Show the owner the captures (the latest stills are `stills-r18a` and `stills-r18b`; refilm if the
   scratch folder is gone). Ask whether `fx.deathAbsurd` goes to 1 and whether to land the lanes (B, C, A).
2. **Salvo rockets read poorly.** They launch 0.5 s after the kill, straight into the fire's smoke column. Launch them
   later, or sideways out of the column, so the corkscrews and pops show.
3. **The Tusk has no road wheels to throw.** Its wheels are part of `Track_L` and `Track_R`. This needs a cut in
   Blender (the owner allowed parts cut from existing models), or a different gag such as the hatch and exhaust
   popping off.
4. **The heap reads as one clump.** Men are staggered 0.05–0.3 s, which is finer than the stills show; widening the
   spread is a tuning choice. Team-1 corpses are field grey and blend with snow; that is the uniform, not a bug.
5. **The critic can't judge motion from stills.** Flips, bounces, corkscrews and cartwheels need a short clip per
   scene (a GIF or video) given to the critic.
6. **Cook-off cream flicker in today's game (knob 0).** Every pop sets the wreck's `Flash` to 1 (`RunPops`, fixed
   above 0 only). Fixing it for everyone changes today's look, so it's the owner's call.
7. **Profile the knob's cost.** In the barrage bench at 1,000 men a side, knob 1 costs about 8% fps and 0.4 ms at the
   median frame, and death frames allocate about 2 KB more. The allocation wasn't found by reading the code; it needs
   a profiler capture. Draw calls don't record in batch mode.
8. **Smaller items:**
   - The Skimmer's fan lands pale grey; its material doesn't take the scorch.
   - The laid-out track reads as a plain plank.
   - The rocket pops are small.
