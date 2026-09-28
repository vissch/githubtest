---
name: tw-gym
description: The Trench Warfare 3D gym — play and test everything on integration one piece at a time in the battle's own rendering (every infantry clip on a real VAT figure, every unit, every off-map ability, every death kind, every sim event), jump the camera through the six zoom bands, and run the whole catalogue unattended into contact sheets + JSON that the bug catcher and critic read. Use for "open the gym", "show me the jetpack", "play the kneel-fire clip", "run the gym", "what broke since the last gym run", "add X to the gym", and as the proving ground for every sim role's fix. NOT the Playground (its figures are not the battle's) and NOT perf benches (tw-optimizer).
---

# The gym

**The owner's words, verbatim:** "Gym somewhere all the results in the merged build are playble and testable per
animation and special ability too."

Deep reference: `references/gym-design.md` has every API the gym stands on (file:line at `c43b73f`), the staging per
entry, and the traps. The build plan is Brief 2 §B1.

## Status: check before you promise anything
```bash
git -C <checkout> ls-tree -r --name-only origin/claude/trench-warfare-2d-3d-plan-idt7lf | grep -E "Editor/Gym.cs|AnimationController.Pin"
```
- **No hits:** it isn't landed yet. The code is on `lane/show/gym`, in the desktop's `githubtest-desk-show`. Use it
  there, or the stand-ins in the last section.
- **Hits:** use it as written here, and correct this page wherever the code differs. The code wins.
- **"In the merged build":** today the gym runs in the editor on integration. A development player with the gym
  (`-twgym`, phase G4) is planned, and the owner decides when (Q-B2).

## Why not the Playground
The Playground's men are `UnitRig`/`Retarget` skinned figures. The battle draws men as VAT figures through
`AnimationController` → `VATRenderer`, machines through `TankRenderer`, and effects through
`CombatFx`/`FlipbookFx`/`DebrisRenderer`. A clip can look right in the Playground and still be wrong in battle, so
**the gym runs in `GreyboxCorridor`, on the battle's own paths.**

## What it covers (read from the enums and the unit table, never a hand list)
| Tab | One entry per | How it's triggered | Bands captured |
|---|---|---|---|
| Clips | `Clip` value × VAT figure (Soldier; Sniper = archetype 3) | a presentation-only **pin**: `AnimationController.Pin(slot, clip, rate)` | T3, T2, T1 |
| Units | archetype with Hp > 0 in the match's unit table | spawned through `SimHost.WriteWorlds` | all six |
| Abilities | `OffMapAbilityId` | `SimHost.Issue` / `IssuePeer`; ParaDrop from the Brass seat. Expected: Fires, Rejected or FactionSeat | all six |
| Deaths | `DeathKind` (Shot, Blast, Gas, Crushed, Burning, Beam) | the cause is staged, and the sim kills inside a tick | all six |
| Events | `SimEventType` | Covered (by another tab), Preview, or Excluded with a reason. A preview is replayed into `EventPump.Frame`: **effects only, the men never see it** | all six (previews) |
| Scenes | `GymScene`: TrenchLine, BarrageOnTrench, GasOnTrench, CraterMen | men where a player has them (our trench, fresh craters), an enemy line 80 m out, and the thing that hits them. The sidecar's `consequence` counts hits, near misses, suppression and deaths among the watched men | all six |

The bands are T3 7.5 · T2 16 · T1 30 · O120 · O240 · Far 600, via `TacticalCamera.FrameFrom`.

`GymCatalogueTests` fails when a new event or ability has no row. When you add a clip, unit, ability, death kind or
event, give it its gym row in the same commit. Unit actions (Breaker charge, jetpack leap, medic, engineer) run on their
own in the sim, since `CommandType.UnitAbility` has no consumer. Their events are "Covered" by staging the unit
against an enemy line.

## Using it
**By hand.** Play in `GreyboxCorridor`, then **TW > Gym**:
1. "Quiet the battle".
2. A tab, then Play on an entry.
3. The band buttons.
4. The trace shows what the followed man is doing.

**One tab or part, from a session.** Claim the checkout's editor first (workflow.md §2):
```bash
cd trench-warfare-3d && python Tools/editor_lock.py claim gym --minutes 40 --why "gym run" || exit 1
Tools/tw eval 'return TW.Editor.Gym.Run("tabs=clips filter=Fire max=20");'    # options below
```

**Unattended, whole catalogue** (desktop, editor closed, one heavy job at a time; ask pc-e5 or other sessions first):
```bash
( cd trench-warfare-3d && python Tools/editor_lock.py claim gym --minutes 60 --why "gym run" ) || exit 1
SHA=$(git rev-parse --short HEAD); P="$(pwd)/trench-warfare-3d"; L="$LOCALAPPDATA/TrenchWarfare/runs/gym-$SHA"; mkdir -p "$L"
python trench-warfare-3d/Tools/pipeline/run_detached.py start gym-$SHA --timeout 3600 --min-headroom-gb 10 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode -projectPath "$P" \
  -executeMethod TW.Editor.Gym.CommandLine -twgym "tabs=scenes,clips,units,abilities,deaths,events" -logFile "$L/unity.log"
```
The editor exits by itself:
- **0** clean;
- **2** flagged entries;
- **1** could not run, or a guard stopped it (1 GB, 10 GB free, or the `minutes=` wall clock, 45 by default).

Don't add `-quit`. `Unity.exe` here is the 6000.0.50f1 **editor**. The `unity` CLI at
`%LOCALAPPDATA%\unity\bin\unity.exe` is a different program (`Tools/tw`, the gate). Afterwards run
`editor_lock.py release gym`.

**Options:**
- `tabs=` — a list of scenes, clips, units, abilities, deaths, events;
- `filter=<part of a name>`;
- `max=<n>`;
- `bands=all|close` — close is T3/T2/T1; Clips always use close;
- `out=<folder>`;
- `minutes=<limit>`;
- `quit=1` — CommandLine adds it.

**Output.** The run goes to `%LOCALAPPDATA%\TrenchWarfare\gym\<yyyyMMdd-HHmm>-<sha>\` (`TW_GYM` overrides). It is
**never inside a checkout**, because an untracked file changes the tree `land.py` checks. It holds:
- `<Tab>/<entry>.jpg`: the bands side by side;
- `<Tab>/<entry>.json`: expectation, events by type, errors, warnings, rejects, previews, `canary`, `desync` (`null`
  without the canary), the pinned man's clip, frame, rung and yaw, and flags;
- `summary.json`;
- `raw/`: PNGs, kept only for flagged entries.

**Guards.** A run keeps itself plus the newest older run, and deletes only other folders that hold `gym-run.txt`. It
stops at 1 GB, or at under 10 GB free.

## The loop (Brief 2 §B5)
1. **Run.** Then read `summary.json`. The flags are the ones `GymRun.Judge` raises (`Editor/Gym.cs`); this list must
   match it:
   - `N log errors`;
   - `desync` (canary only);
   - Clips: `pinned man is drawn in <clip>`, `no man in the T3 frame`;
   - Abilities: `expected the sim to refuse it; it did not`, `refused by the sim`, `no AbilityFired`;
   - Deaths: `he did not die` (only the victim's own Death counts);
   - Units: `not alive 4 s after spawning`;
   - Scenes: `no men staged`, `nothing reached the men`;
   - per still: `blown <frac>` (> 0.02), `camera off pose <m>` (≥ 0.5 m: an invalid still, not scored), `no capture`;
   - staging or judging: `not staged`, `staging threw`, `judging threw`, `sheet failed`.
2. **Bugs.** Every flag goes to `tw-bug-catcher`, which files one card per signature.
3. **Compare runs.** `Tools/gymscore.py <old run> <new run>` is **to build** (G3). Until it exists, diff the two
   `summary.json` files' flag lists by entry (`python -c` with json), and compare sheets side by side.
4. **Score.** `tw-critic` scores the sheets per band against the look spec: the numbers first, then the picture.
5. **Fix and prove.** The owning role fixes, and the proof is the same entry before and after, at every band it names,
   with the unchanged build run twice for the noise floor.
6. **Learn.** Recurring flags go to the board's `lessons/gym.md` (the learning loop in `../pipeline/SKILL.md`). A flaw seen in several entries becomes a proposed
   catalogue or flag change, which the owner approves.

## Traps
- **Events raised inside `WriteWorlds` are lost** (`SimWorld.Step` clears them first). A death or effect comes from a
  system that steps inside a tick. `Hp = 0` gives a corpse with no Death event.
- **Never use `TankCapture.Spawn` in gym code.** It writes both worlds by hand, against `CLAUDE.md`. Use
  `GymDirector.Spawn`, which goes through WriteWorlds.
- **`GymDirector` is compiled only under `UNITY_EDITOR || DEVELOPMENT_BUILD`**, because `TW.Perf` ships in release.
  Keep it that way.
- **Quiet** means `ScriptedPeer`, `PeerAttacks` and `PeerUsesSupport` off, plus `Bombardment.ShellsPerMinute = 0`
  written through WriteWorlds. `SimHost.BombardmentOverride` is read only when a match starts, so it's useless
  mid-match.
- **The sim refuses five abilities** (MustardGas, BomberRun, MortarSalvo, ReconFlight, ReinforcementSurge: no stats).
  The gym expects that, and it isn't a bug.
- **Burning uses `Burning.Ignite` inside WriteWorlds.** That's a tooling write against `Burning.cs`'s "presentation
  never calls into here", flagged to the SIM lane. If they object, stage Burning with the Beam instead.
- **Crushed is best effort:** there's no move order for one vehicle. A Maw is placed nose-on and may miss.
- **`Clear` removes only gym-spawned units.** Despawning still plays their fall, so the run settles 1.5 s between
  entries.
- **A pinned man that the sim moves slides.** The stage is kept quiet, and the sidecar records his yaw.

## Before the gym lands (stand-ins)
| Need | Use |
|---|---|
| Any unit in battle | **TW > Unit Sandbox** (`Editor/UnitSandbox.cs`) |
| Abilities in battle | the in-battle test panel (`Presentation/Camera/TestPanel.cs`) |
| One still or a series per band | `CaptureRig.Shot` / `Series` / `Sheet` (`../pipeline/references/driving-and-evidence.md`) |
| A house, vehicle or figure alone | the Playground (`pg.sh`). Its figures are **not** the battle's |
