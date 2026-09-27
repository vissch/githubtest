# Infantry: reference (read at integration 558c667; the code wins where this differs)

## Figures and the VAT bake
- `FigureNames = { "Soldier", "Sniper" }`. Sources are `Art/Characters/Soldier.fbx` and `Sniper.fbx`. The bake is normalised to a 1.78 m man; `UnitScale` is 1.125 (`Presentation/Core/FigureMetrics.cs`).
- `Editor/VATBaker.cs` `BakeInfantry()` writes to `Assets/_Project/Resources/Units`: `Figure<Name>.asset`, `Figure<Name>Mesh.asset` and `Figure<Name>Atlas.bytes` (each atlas is about 19.5-20.1 MB). The report goes to `VATBaker.LastReport` and `Library/vat-bake-report.txt`, with a per-clip line (frames, seconds, loop/once, cm of travel kept, turn) and a summary.
- Batch `-executeMethod TW.Editor.VATBaker.BakeInfantry` is untested; only the menu is documented.
- `Editor/InfantryClipTable.cs` reads clips from `Assets/_Project/Art/Characters/Clips` (103 FBX). Builders: `L` loop, `O` once (cuts, rate, `StripYaw`), `C` composed upper over lower, `T` thrown, `D` death with `KeepRoot`.

## AnimationController (Presentation/Core/AnimationController.cs)
- Priority ladder, highest first: death, trench edge, reaction, action, stance change, turn, locomotion, fire, idle.
- `enum Rung { None, Death, Trench, Reaction, Action, StanceChange, Turn, Locomotion, Fire, Idle }`. `enum Clip` holds Idle … DeathThrown, with 12 deaths.
- Constants: `ThrownLands` 0.9, `DiveLead` 7, `HopSeconds` 0.5.
- Trace: `Follow(slot)` then `TraceText(max = 400)`. Columns: tick, t(s), rung, clip, stance, speed, tgt, supp, reason. It ends with time spent in each clip.
- Deaths (`AnimationController.Death.cs`): `DeathKind { Shot, Blast, Gas, Crushed, Burning, Beam }`, read with `TryDeath(slot, atTick, out record)`.
- Reference traces in `docs/reference/controller/`: rifleman advance (before and after hysteresis), garrison, assault over the top, sniper firing, the kneeling cycle, parapet then advance.
- Reference stills in `docs/reference/figures/`: aims, reloads, garrison at zoom 8, idles, actions, deaths, trench kneeling.

## Clip manifest (docs/reference/animation-clips.md, .csv)
- 205 clips measured by `fbxscan.py`: 131 use, 28 spare, 46 ditch. There are 274 s of "use".
- Hip height (cm) tells the stance: about 100 standing, 72-92 stooped, 39-46 kneeling, 13 prone.
- CSV columns: `clip, state, stance, kind, rigs, verdict, seconds, frames, loops, travel_cm, hips_min_cm, hips_max_cm, note`.
- Made clips: 15, all "use" (`docs/reference/made-clips/made-clips.csv`: clip, kind, seconds, frames, template, recipe).

## Tools
- **`clipcheck.py "<Made>" [template folder]`** prints skate, drift, footMin, handMin and hand-to-head.
  - A foot counts as planted while it is the lower foot and within 6 cm of its clip minimum.
  - Thresholds: skate > 12 cm per plant, drift > 25 cm, footMin < −4 cm, hand-to-head < 14 cm.
  - `handMin` has no threshold. The docstring says "per step", but the code sums per plant run.
- **`animforge.py`** is pure Python at 30 fps, writing by cloning a Mixamo template. `read_fbx`, `write_fbx`, `Rig`, `Clip.load / copy / reverse / slice / resample / retime / loopify(fade=6) / layer / offset / scale_motion / strip_root / wave / set_pose / hold / pose / repeat / concat / save`, `fk`, `solve_arm`, `Canvas`, `contact_sheet`.
- **`make_missing_clips.py "<Mixamo folder>" [out]`**: Wade Forward, Ladder Climb, Crawl Forward, Prone Crawl Forward Alt, Prone Death, Prone Flinch, Kneel Flinch, Get Up From Prone, Stumble Running, Mask Donning, Burning Run, Officer Point, Officer Whistle, MG Carry Walk, Wire Crossing.
- **`fbxscan.py "<folder>" out.csv`** measures seconds, frames, loop, root motion, hip height and yaw.
- **`frogrig.py -- <lod0> <lod1> <lod2> <out.fbx> <renderdir>`**: Mixamo bone names, with LOD1-3 weights transferred from LOD0. Flags: `TW_HEIGHT` 1.78, `TW_LOD3_TRIS`, `TW_DERIVE` 12, `TW_REBAKE`, `TW_LOD2_FROM_LOD1`, `TW_LOD3_KEEP`, `TW_LOD3_RIGID`, `TW_LOD3_TEXTURED`.
- **Playground:** `UnitRig` (one armature, all LODs, `LodPicker(0.20, 0.08, 0.03)`), `Retarget` (the game's clips carried to another skeleton, hips scaled by hip height). `pg.sh do "unit"`, `unit.compare`, `unit.squad`; `clip <name>`, `speed`, `face deg`, `kill`, `ignite`, `revive`. Figure LOD switch points: ~22 m, ~63 m, 78-167 m.

## Tests worth knowing
- **DeathVarietyTests:** a blast death is thrown the way and as hard as the sim says; the fifth man down in a bay is thrown further; a man alight drops mid-stride and lies charred; gas and a track have their own deaths; the record outlives the slot; the tumble ends upright and a heaped man lies tilted.
- **BlastReactionTests:** a man thrown hard is blown off his feet, then gets up dazed; the reaction runs out through the men; the nearer man wears more earth; the camera feels a burst when the sound arrives.

## Adding a unit type: the SHOW side (tasks.md "Adding a unit type", steps 3-6)
The SIM lane lands the id and the `UnitDef` first. Then SHOW adds:
1. The name, tooltip and portrait in `Presentation/Core/UnitLook.cs`.
2. The portrait stem in `UI/Skin/SkinSpec.cs` (`PortraitNames`), and the pictures.
3. A figure in `Editor/VATBaker.cs` plus a bake.
4. A check of the legacy `BattleHud` with F9.

Tests: FactionRosterTests, UnitDefinitionTests, HudTextTests, HudBindTests, UnitArtTests, then the full gate.
