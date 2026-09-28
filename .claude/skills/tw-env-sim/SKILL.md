---
name: tw-env-sim
description: Environment and level simulator for Trench Warfare 3D — generate and judge battlefields programmatically (seeded grounds ShelledForest / Landing / WinterLine, biomes NightMud / Winter / Lava), check they use the prop kit efficiently and effectively, look good against the owner's references at every zoom, and adapt across seeds and sizes. Knows BattlefieldGenerator, MatchLaunch.Field, BattlefieldComposer, ScatterField, the env atlas seam, PropLayout hand edits, the asset-scale audit and the env scoreboard. Use for "generate a new map", "is the winter line good", "score the environment", "env stage of item X". NOT for destruction looks (tw-destruction-vfx) or unit balance (tw-balance-sim).
---

# Environment simulator

**Judge:** `docs/reference/env-scoreboard.md`: ten criteria at tiers T1, T2 and T3, on three boards (NightMud, Winter, Coast). The bar is every line at least 8 and each tier at least 85/100. A change for one biome is checked against the other two. Every board is still "-": the first job is to score them.
Driving and capture: `../pipeline/references/driving-and-evidence.md`. Repo pages: tasks.md "Map generation (sim)", "Battlefield props and composition", "Terrain view, weather, night, biomes"; docs 13, 14, 18, 19.

## How a battlefield is made
1. **Ground (SIM, seeded):** `BattlefieldParams` (Seed, Width, Length, Forest, Shelling, Mud, WaterLevel, River, Wrecks, Bombardment, Sea, SeaMargin) → `BattlefieldGenerator`. Presets `ShelledForest(seed)`, `Landing`, `WinterLine`. `MatchLaunch.Field(Ground, seed)` is the one place a Ground becomes a battlefield.
2. **Look (SHOW):** `BiomeProfile.ForGround`: WinterLine → Winter, otherwise NightMud ("Landing is night mud on purpose"). `SimHost.Ground` picks the terrain, **not** the look; for a visual test set both, and put both back.
3. **Composition (SHOW):** `BattlefieldComposer` builds map props, the trench kit, debris/litter/clumps/margins, hamlets (only by water with a bridge), rear buildings, sites, landmarks, scatter, snow, backdrop.
   - `ScatterField` has five 2 m fields: Traffic, Vertical, Patch, Wet, Open. `ScatterLayers` has Grass, Accent, Flower, Interior, Rear.
4. **Owner's hand edits:** `Resources/Layouts/Battlefield1917.asset` (`PropLayout`, written by menu **TW/Env Props**).
   - `python Tools/looks.py --check` prints; `--apply` rewrites looks and hand edits.
   - Never overwrite the owner's edits without asking.

## The four measures
| Measure | How |
|---|---|
| Efficient | Reuse ratio (instances per unique asset), `FrameBudget.DrawCalls` / `Vertices` at T1, share of the kit used, T1 budget never rises (close detail behind `_TWClose`) |
| Effective | Paths between objectives, cover density in the range set in thresholds, a fair approach for both sides, walkers get about 10 m gaps between placed structures, props never block movement |
| Pretty | The env-scoreboard ten criteria against `docs/reference/battlefield-night.jpeg` (the default look), `battlefield-northstar.jpeg` and `battlefield-night-game.jpeg`; critic via tw-critic; readability never drops |
| Adaptable | One spec validates across 8 seeds × three grounds × sizes, and the asset-scale audit stays OK |

- **Scale audit:** menu **TW/Audit/Asset Scale**, or in batch `Unity.exe -batchmode -quit -projectPath <checkout>/trench-warfare-3d -executeMethod TW.Editor.AssetScaleAudit.Run -logFile audit.log`.
  - Writes `docs/reference/asset-scale.md` (`TW_AUDIT_OUT` overrides).
  - FAIL = out of bounds; CLAMPED on more than 10 % of instances also fails.
- **Captures:** `AgentScripts/aosa_tiers.cs` (T1, T2 and T3 at 1920×1080). On the bench, `ground=<ShelledForest|WinterLine|Landing>` with `shot_tick=N shot_hud=0`.
- **Tests:** BattlefieldTests, DynamicGroundTests, CoastTests, WinterMapTests, WinterLevelTests, LandingTests, EnvAtlasTests, AssetScaleTests, ScatterRulesTests, BiomeProfileTests.
- tasks.md trap: "a feature tested only on the playtest map is untested."

## Seams and traps
- **The env atlas grid is a seam:** `BattlefieldKit.EnvSets` / `EnvCols` / `EnvRows` must equal `SETS` / `COLS` / `ROWS` in `Tools/envatlas.py` (4×4, 4096², 7 cells spare). A new set appends to both.
- Clamp scale in `Emit` / `Placement`, not `Styled`. A new kit `Module` field needs an `AssetScaleTable` row.
- Put nothing in `ScatterLayers.Place` that reads the surface.
- New art: `envgrade.py` for textures; `envsplit.py` / `housesplit.py` in Blender 5.0 for splits (tw-vehicle-sim has the command form). Source sheets are the owner's: ask, do not substitute.
- `BattlefieldParams` serialisation appends new fields at the end.

## Owner decisions and questions
- **Decided:**
  - the standard view (fov 25, pitch 25, zoom 30, yaw 21);
  - night is the default look;
  - nothing ruled or evenly spread, and trenches bend;
  - a river with fords and a bridge;
  - bunkers and guns only at the back or fog side;
  - man-made props are true to the soldier;
  - the sea is beyond the enemy line;
  - coast and snow first, lava later;
  - the trench always stands;
  - buildings never block movement;
  - the ground is dynamic.
- **Open, never build around:**
  - shelter protection;
  - house sim cover;
  - where the ruins set goes;
  - the Forward+ renderer;
  - the house kits' tan at night;
  - docs/18's lava and snow questions;
  - coast beach obstacles and the defender's trench (docs/19).

## Learning loop (Brief 2 §B5, every role)
The loop, every time:
1. produce;
2. evidence from the gym (`tw-gym`) or a bench;
3. a `tw-critic` round, with angles rotated between rounds;
4. fix;
5. keep the **best** round, not the last;
6. write what worked and what failed to the board's `lessons/env-sim.md`.

A flaw that recurs across items becomes a proposed checklist line for this page, which the owner approves.
