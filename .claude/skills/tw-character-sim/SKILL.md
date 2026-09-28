---
name: tw-character-sim
description: Character simulator for Trench Warfare 3D infantry — walk each unit type through every relevant situation (trenches, craters, wire, cover, climbing, firing, reloading, melee, hits, deaths, crowds), find animation problems with measured metrics, and repair clips so each unit gets its own fix. Knows the VAT bake, the 205-clip manifest, clipcheck/animforge/make_missing_clips, the AnimationController ladder and its traces. Use for "check the riflemen's animations", "the MG man's feet slide", "add a wire-crossing clip", "character stage of item X". NOT for vehicles (tw-vehicle-sim) or unit stats (tw-balance-sim).
---

# Character simulator (infantry)

Deep reference: `references/infantry.md`. Driving and capture: `../pipeline/references/driving-and-evidence.md`.
Repo pages: `docs/reference/tasks.md` sections "Infantry rendering (VAT)" and "Animation"; `docs/15-character-controller.md`.

## Concepts first (the owner, 2026-10-06)
Before a new effect, animation or character is built, or the look of one is changed, put two to four concepts or
references to the owner and build the one he picks. A concept is a picture or a short film generated on the desktop's
GPU broker, a reference that was found, or a page drawn in HTML or SVG. Write them as one brief:
`python Tools/assetboard/briefs.py concepts --title ... --for ... --concept PATH="what it is" --concept ... --why ...`
(from `trench-warfare-3d/`, your own choice first). He picks on the board's Decide page. A fix that changes no look
(a bug, a number, a test) needs none.

## What the unit set really is (read this first)
- **Only two figures are baked: Soldier and Sniper.** `VATRenderer.FigureOfArchetype(a) => a == 3 ? 1 : 0`, so every
  infantry type except the sniper is drawn as Soldier, whatever its role.
- Repairing a clip changes it for **every** archetype on that figure. A unit-specific fix therefore needs either a
  per-archetype choice in the controller (SHOW lane) or a new figure and a rebake. A new figure is the owner's decision
  (about 19 MB of git history per atlas, both rebaked each bake, and no LFS).
- Per-unit proportions exist only in the Playground today (`UnitRig`, `Retarget`, `frogrig.py`).

## The situation matrix (a cell per unit type × situation × band)
Flat · slope · trench entry and exit · parapet · crater · wire · cover · climb/ladder · fire · reload · melee · hit
reaction · every death kind (Shot, Blast, Gas, Crushed, Burning, Beam) · carried weapon · crowd squeeze (garrison,
bays). Measure proportions first (height, reach, stride per unit), then run the matrix.

**Metrics and thresholds** (`Tools/clipcheck.py`, on source FBX):
| Metric | Fails when |
|---|---|
| foot skate | > 12 cm per plant |
| root drift | > 25 cm |
| foot below floor | < −4 cm |
| hand to helmet | < 14 cm |

In battle: the `AnimationController` trace (rung, clip, stance, speed, reason per tick) and time spent in each clip.

## How to run it
| Want | Use |
|---|---|
| Check clips offline | `python Tools/clipcheck.py "<download>/Made"` |
| List and measure clips | `python Tools/fbxscan.py "<folder of .fbx>" out.csv`; manifest `docs/reference/animation-clips.md` / `.csv` (205 clips, use/spare/ditch) |
| Make missing clips (15 recipes) | `python Tools/make_missing_clips.py "<Mixamo folder>"` writes `<folder>/Made/*.fbx`, a sheet and a csv |
| Edit a clip in code | `Tools/animforge.py`: `Clip.load`, `reverse`, `slice`, `retime`, `loopify`, `layer`, `offset`, `strip_root`, `Clip.concat`, `save(path, template)`, `solve_arm`, `contact_sheet` |
| Bake the figures | menu **TW/VAT/Bake Infantry** (`VATBaker.BakeInfantry`). The report is in `Library/vat-bake-report.txt`. About a minute. Delete `Library/BurstCache` after a job-struct change |
| Trace one man in Play | `Tools/tw eval 'var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); h.Animation.Follow(N); return "ok";'`, later `return h.Animation.TraceText();`. Reference traces are in `docs/reference/controller/` |
| Guard tests | DeathVarietyTests, BlastReactionTests, VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, TickAllocationTests, PlaygroundAssetTests (figure tests) |

## Repair loop (at most 3 rounds per failing cell; Brief 2 §B5)
1. Name the cell, the metric and its threshold before touching anything.
2. The cheapest fix first:
   - clip table flags and cuts in `Editor/InfantryClipTable.cs` (`L` loop, `O` once, `C` composed, `T` thrown, `D` death with `KeepRoot`);
   - then an animforge edit of the source clip;
   - then the controller's choice of clip (`Presentation/Core/AnimationController.cs`).
3. Rebake, rerun clipcheck and the guard tests, and re-trace the same man in the same situation.
4. The same clip is now on every Soldier-figure unit. Check the other archetypes still pass their cells.
5. After three rounds the cell is BLOCKED, with a note of the source animation needed.
6. **Critic** (`tw-critic` character rubric, rotating angles) on the gym sheets. Keep the best round.
7. **Learn:** the learning loop in `../pipeline/SKILL.md` (board `lessons.md`, step 0 first).

## Foxholes, trenches and craters (the owner: "fix the characters sitting in their fox hole and shooting")
**What happens today, at `c43b73f`:**
- **Stance.** A man at his garrison post gets `Stance.Crouch` (`Sim/Nav/MovementSystem.cs` ~352-357).
  `AnimationController` draws FireStep, Vault and Sprint as Standing (~461); only the sniper kneels (~462).
- **Firing.** The fire clip is picked at ~649: prone → FireProne / FireMG, low → FireKneel, otherwise FireStoop (MG),
  FireSnap (bolt rifle) or FireStand. **So a man on the fire step fires standing.**
- **Idle.** `TrenchRoutine` (~814-840) keeps him on KneelIdle, with these beats:
  - rise to AimedIdle;
  - StoopIdle;
  - KneelInspect / LookAround / FidgetRubEyes;
  - Shield when Suppression > 30.
- **What's missing.** There is **no seated-in-a-hole pose and no parapet lean**, and "foxhole" appears nowhere.
- **Craters.** They are designed (`docs/15-character-controller.md` ~191: kneel, go prone in the bowl, fire prone), but
  `NavLayer.Crater` has **no presentation reader except the map views** (HudMinimap, BattleHud, MapThumbnail), so a man
  in a crater plays his open-ground clips. The sim does read it (cover, blast protection, gas, vehicle speed:
  `DirectFire.cs` ~231, `Blast.cs` ~234).
- **Muzzle height.** The fire step changes the picture only: the muzzle is at +0.3 m for every man in a trench
  (`docs/14-organic-trenches.md` ~96-103). Changing that is **SIM**: put it in "Open" and don't build around it.

**The fix (SHOW lane), in order:**
1. **Sources.** Look in `Art/Characters/Clips/` (103 Mixamo FBX, `InfantryClipTable.Folder`) and the owner's
   `Downloads\mixamo animations\` (`pipelines.md`). You want:
   - a rifle aim and fire leaning forward on a wall (parapet lean);
   - a crouched or seated hole idle, and a fire from it.

   Measure each candidate with `clipcheck.py` before choosing. Anything missing is made with `animforge`
   (`layer`/`offset` on a kneel-fire, for example), like the `make_missing_clips.py` recipes.
2. **Clips.** For each new clip:
   - a new `Clip` value;
   - its `Clips.Table` row with a fallback;
   - an `InfantryClipTable.Build` source.

   **Batch every new clip into one bake.** A bake rebakes both figures and adds about **38 MB** to git history (two
   ~19 MB atlases).
3. **Choice.** FireStep → parapet lean-fire (and its idle). A man in a crater cell → the hole or prone set. Read
   `NavLayer.Crater` through the presentation's terrain view. Don't change the sim.
4. **Proof.**
   - The gym (`tw-gym`), at T3 and T1: the Clips tab for the clip itself, and a trench and a crater staged with a
     rifle line for the real choice.
   - clipcheck thresholds: skate, drift, foot below floor, hand to helmet (the table above). It also prints `handMin`
     (hand floor) with no threshold. It has no hand-to-weapon or penetration measure, so judge those from the gym's T3
     sheet.
   - The gym's **Scenes** tab: TrenchLine (men in our trench, an enemy line at 80 m) and CraterMen (men in fresh
     craters) at T1 first, then T3.
   - The trace showing the new rung.
   - Before and after sheets.

## Reactions to the battlefield (the owner: "feedback from the sim to the units is critical")
This role owns the unit column of the reaction matrix (`tw-destruction-vfx`, Brief 2 §B3):
- flinch and duck under near misses and suppression;
- knockdown and blast throws (`BlastReactionTests`);
- deaths per cause;
- a man carried or hit by debris.

Every reaction must read at T3 and T1. At far, it only has to change the squad's shape. A cell with no sim event is
flagged, never faked.

## Traps (tasks.md)
- Read a dead man through `TryDeath`, never `State[slot]`.
- Any clip or discard in the shader goes behind `_TW_LIMBCUT`.
- `VatInstance.Tint` is team + 2·step; never read it as the team in C#.
- `VatPad` is full at 24 bits.
- Six planned death clips (Burning, Gas, Crushed, Stagger, Walking2, BackHeadshot) are **not baked**; stand-ins are used.
- Root travel is stripped as a linear trend, except for deaths.

## Owner questions — never build around them
- A new figure per unit type, or per-unit proportions in battle.
- The atlas budget (54 MB vs 8 MB), sim magazines and melee, backward running, the officer rig (docs/15 §15).
- The frog's far LOD size (300 tris ships; 380/460 measured).
- Decisions to respect:
  - deaths are VAT launched bodies, never ragdolls;
  - unit budget is 1,200-1,500 vertices (max 2,000), far model 250-400 beyond 170 m;
  - infantry are 25 % shorter (`UnitScale` 1.125);
  - winter troops keep khaki.
