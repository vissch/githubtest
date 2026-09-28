---
name: tw-gym
description: The Trench Warfare 3D gym — play and test everything on integration one piece at a time in the battle's own rendering (every infantry clip on a real VAT figure, every unit, every off-map and unit ability, every death cause, every effect event), jump the camera through every zoom band, and run the whole catalogue unattended into contact sheets + JSON that the bug catcher and critic read. Use for "open the gym", "show me the jetpack", "play the kneel-fire clip", "run the gym", "what broke since the last gym run", "add X to the gym", and as the proving ground for every sim role's fix. NOT the Playground (its figures are not the battle's) and NOT perf benches (tw-optimizer).
---

# The gym

**The owner's words:** "Gym somewhere all the results in the merged build are playable and testable per animation
and special ability too." Deep reference: `references/gym-design.md` (every API the gym stands on, with file:line at
`c43b73f`, the staging recipe per entry, and the traps). The build plan is Brief 2 §B1 of the pipeline plan.

## Status: check before you promise anything
```bash
git -C <checkout> ls-tree -r --name-only origin/claude/trench-warfare-2d-3d-plan-idt7lf | grep -E "Gym|AnimationController.Pin"
```
- No hits: the gym is not landed yet. It is being built on `lane/show/gym` (phases G1 → G3 below). Say so, and use
  the stand-ins in the last section.
- Hits: use it as written here, and correct this page where the code differs. **The code wins.**

## Why not the Playground
The Playground's men are `UnitRig`/`Retarget` skinned figures (`tasks.md`, Playground rows). The battle draws men as
VAT figures through `AnimationController` → `VATRenderer`, machines through `TankRenderer`, and effects through
`CombatFx`/`FlipbookFx`/`DebrisRenderer`. A clip that looks right in the Playground can still be wrong in battle. **The
gym runs in the battle scene (`GreyboxCorridor`), on those same paths.**

## What it covers (enumerated from the enums, never a hand list)
| Tab | One entry per | Trigger |
|---|---|---|
| Clips | `Clip` value × VAT figure (Soldier, Sniper) | a presentation-only **pin** on one man: `AnimationController.Pin(slot, clip, rate)` |
| Units | roster archetype with Hp > 0 | spawned through `SimHost.WriteWorlds`, idle, then moving |
| Abilities | `OffMapAbilityId` | `SimHost.Issue`/`IssuePeer` (Brass-only abilities through the peer). Expected: accept, reject or faction-gated |
| Unit actions | Breaker charge, jetpack leap, medic, engineer repair, each weapon firing | **stage the conditions**: these systems run on their own, and `CommandType.UnitAbility` has no consumer |
| Deaths | `DeathCause` × `DeathKind` | a real system kills him inside a tick (see the reference's table) |
| Effects | `SimEventType` | a real trigger, or a **preview** (the event replayed into `EventPump.Frame`, labelled "preview") or excluded with a reason |
| Bands | T3 7.5 · T2 16 · T1 30 · overview 120 · 240 · far 600 | `TacticalCamera.FrameFrom` |

`GymCatalogueTests` fails when a new enum value has no entry or exclusion. When you add a unit, clip, ability, death
or event, add its gym entry in the same commit.

## Using it
- **By hand:** Play in `GreyboxCorridor`, then **TW > Gym**. Pick a tab and an entry, press Trigger, and use the band
  buttons. "Follow" traces the man (`Animation.Follow/TraceText`).
- **Unattended** (G2), on the desktop, one heavy job at a time:
  ```bash
  python trench-warfare-3d/Tools/pipeline/run_detached.py start gym --timeout 3600 --min-headroom-gb 10 -- \
    "<Unity.exe>" -batchmode -projectPath <checkout>/trench-warfare-3d -executeMethod TW.Editor.Gym.CommandLine -logFile <run>/unity.log
  ```
  Or inside an open editor: `Tools/tw eval 'TW.Editor.Gym.Run("tabs=clips,abilities bands=T3,T1"); return "ok";'`.
- **Output:** `%LOCALAPPDATA%\TrenchWarfare\gym\<date>-<sha>\` (`TW_GYM` overrides), **never inside a checkout**,
  because an untracked file changes the tree `land.py` checks. It holds:
  - one JPG contact sheet per entry, with the bands side by side;
  - `<entry>.json`: CaptureRig metrics, trigger verdict, event counts, log errors, desync, and pin pose numbers;
  - `summary.json`, which flags each entry.
- **Retention:** a run keeps itself plus the newest older run (`tw-housekeeping`). Raw PNGs are kept only for
  flagged entries.

## Reading a run (the loop)
1. `summary.json` flags:
   - log errors > 0;
   - an unexpected reject, or no expected event before the timeout;
   - desync;
   - `blown_frac` > 0.02, or no man in frame at T3;
   - NaN or stuck pose.

   Every flag goes to `tw-bug-catcher`, which files one card per signature.
2. `python trench-warfare-3d/Tools/gymscore.py <old run> <new run>` prints REGRESSED lines. It joins `clipcheck.py`'s
   foot-skate and floor numbers by clip.
3. `tw-critic` scores the sheets per band against the stage's look spec. The numbers come first, then the picture.
4. The owning role fixes, and the gym entry is the proof:
   - the same entry, before and after, at every band it names;
   - the unchanged build captured twice, for the noise floor.

## Traps
- **Events raised inside `WriteWorlds` are lost:** `SimWorld.Step` clears the event list first. So a gym death or
  effect must come from a system that steps inside the tick. Writing `Hp = 0` gives a corpse with no Death event.
- **Never use `TankCapture.Spawn` from gym code.** It writes `Local.World` and `Peer.World` directly, against the
  SHOW rule in `CLAUDE.md`. Spawn through `WriteWorlds` on both worlds.
- **The sim rejects five of the twelve off-map abilities** (MustardGas, BomberRun, MortarSalvo, ReconFlight,
  ReinforcementSurge: no stats). The gym expects the reject; that is not a bug. ParaDrop is Brass-only, so it goes
  through the peer.
- Turn off the scripted enemy, peer attacks and bombardment first (`SimHost` fields), or they stray into the frame.
- A pinned man whom the sim moves will slide. The sidecar records his drift, so stage in a quiet corner.
- The pin check sits on the per-tick hot path: allocation-free, guarded by `TickAllocationTests`.

## Before the gym lands (stand-ins)
| Need | Use |
|---|---|
| Spawn any unit in battle | **TW > Unit Sandbox** (`Editor/UnitSandbox.cs`) |
| Call abilities in battle | the in-battle test panel (`Presentation/Camera/TestPanel.cs`) |
| One shot or series per band | `CaptureRig.Shot`/`Series`/`Sheet` (`../pipeline/references/driving-and-evidence.md`) |
| A house, a vehicle or a figure alone | the Playground (`pg.sh`). Its figures are **not** the battle's |
