---
name: tw-destruction-vfx
description: Destruction, death and VFX simulator for Trench Warfare 3D — make sure everything that can break breaks well, every death and every ability/weapon/vehicle event has an effect that reads at close, standard and far zoom, and stays inside the frame budget. Knows the sim → event → presentation path, CombatFx, FlipbookFx books, DebrisRenderer, PropDestruction/HouseKit chunks, TrenchSection, craters, the Playground BuildingRig/VehicleRig, the vfx/barrage/armour benches, flameshots/flamecheck and JUICE moments. Use for "the explosions look weak", "test the house collapse", "add an effect for the gas ability", "destruction stage of item X", "VFX pass". For making a NEW sheet on the desktop, hand off to tw-vfx-sheets. NOT for sim damage numbers (tw-balance-sim).
---

# Destruction, death and VFX

**The rule** (docs/16-destruction.md): the sim decides what breaks; presentation decides how the pieces fly; the
pieces cost the CPU nothing a frame. There is no PhysX, no GameObject per piece and no allocation after load.
Deep reference: `references/architecture.md` (every system, constant, handler and test, snapshot at 558c667).
Driving and capture: `../pipeline/references/driving-and-evidence.md`. New sheets: `../tw-vfx-sheets/SKILL.md`.

## Where things live (SHOW lane unless marked)
| Layer | Code | Notes |
|---|---|---|
| What breaks (SIM, hashed) | `Sim/Combat/Blast.cs` (`BlastShape`, `Impact`, `BlastRules`), `Sim/Match/Deformation.cs` (props, wrecks, craters), `Sim/Units/VehicleModules.cs`, `Sim/Terrain/CraterStamp.cs` | not yours: split the task, SIM lands first |
| Events | `Sim/Core/SimEvents.cs`: Explosion, CraterStamp, Death (+`DeathCause`), VehicleDestroyed, WireBreached, PropChanged, VehicleArmourHit/OnFire/KnockedOut/CookOff/Crushed/LegLost, UnitAlight, WreckRecorded | `EventPump` dispatches once per render frame |
| Effects | `Presentation/Camera/CombatFx*.cs` (Explosion, Death, PropChanged, Crushed; gas per 4 m cell; `ApplyTints` per biome), `Flamethrower.cs`, `NightLights` (flash, fire light, star shell), `CameraShake` | `Presentation/VFX/EventVfxRouter` is a stub: real effects live in `Presentation/Camera` |
| Flipbooks | `Presentation/Camera/FlipbookFx.cs`: `Book` enum + `Sheets` rows **indexed by ordinal**, `Add(...)`, `MaxCards` 1536; textures in `Resources/VFX/` | one missing PNG disables every flipbook (`Ready`) |
| Debris | `DebrisRenderer` + `Debris_URP`: 96-byte records, pools per piece kind, spend by distance (1 / 0.5 / 0.25 at <55 / <120 / beyond m), one indirect draw per pool | `Burst`, `Throw`, `Heave`, `Topple` |
| Props, houses, trench | `Presentation/Terrain/PropDestruction.cs` (rules table: HP per kind, pieces), `PropWear`, `HouseKit.ChunkMask` (96 chunks, seam), `TrenchSection` (Intact → Damaged → Gone), `ScorchTilePainter` | `AttachForTests` + `StrikeForTests` for tests |
| Deaths | `AnimationController.Death.cs` (`DeathKind`, `TryDeath`), `VATRenderer.Fallen.cs` (launched bodies) | owner: VAT launched bodies, never ragdolls |
| Wrecks | `TankRenderer` (`Wreckify`, cook-off, `SceneHooks.DrawnWreck`) | |

## The coverage job: every event gets a look at every band
1. **Catalogue** from code: every `OffMapAbilityId`, weapon per archetype, unit ability, `DeathCause`, vehicle module event, and prop kind in the rules table. List the handler that draws each today.
2. **Audit** each one as GOOD / WEAK / MISSING / WRONG, from captures with numbers, not from memory.
3. **Design** each at three bands:
   - T3 close (zoom 6-9): detail, only behind `_TWClose` / `SceneHooks.CloseUp`, so it costs nothing at T1.
   - T1 standard (zoom 30): the budget view.
   - Overview/far (120-600): silhouette, flash and column only.
4. **Source** each look: an existing book, then an unconverted pack sheet, then a new sheet (tw-vfx-sheets). Prefer reuse.
5. **Wire it:** the CombatFx handler, the FlipbookFx book, debris, light, shake. A `Book` enum entry and its `Sheets` row are added **together, at the same ordinal**, with the PNG. Check `books.Ready`.
6. **Prove it** (below). Keep a destructible inventory with a coverage percentage (props, house kits, trench sections, wrecks) on the board item.

**The current catalogue** is the VFX run's phase 1, on the board at `evidence/vfx-run/phase1-catalogue.md` (board
`9211aa9`): 86 events rated 27 GOOD, 31 WEAK, 23 MISSING and 5 WRONG. It includes a look per band, the wave-1 sheet
list and the implementation order. Start from it; don't redo it.

## The reaction matrix (Brief 2 §B3: "see your actions affect the battlefield")
Rows are events (the catalogue's 86). Columns are what an event touches: man, squad, vehicle, house, tree, wire,
trench, ground. Each cell names:
- the sim event that carries it;
- the reaction (flinch, knockdown, throw, body left, crater, scar, burn, collapse, debris);
- its fidelity per band: T3 the richest, T1 readable, far one clear mark.

Units' cells are shared with `tw-character-sim`. Each filled cell has a gym entry (`tw-gym`) as its proof. A cell with
no sim event is flagged for the SIM lane, never faked in SHOW.

**Blood (decided 2026-09-28, "we need blood"):**
- **Sheets:** the pack's `blood_spurt_1` / `blood_sniper_1`.
- **Placement:** cards along the round's direction on men hit.
- **Scaling:** by `DebrisRenderer.Gore` (0 means none).
- **Bands:** T3 and T1 only; far shows nothing.
- **Budget:** it counts against the draw budget like any book.

## Proving it
- **Benches, same battle:** `CaptureRig.Bench("scenario=vfx|barrage|armour stress=1500 settle_ticks=1800 ticks=400 shot=<png> shot_tick=N shot_hud=0 out=<abs json>")` in the editor. In the player: `-twbench "..."`. JUICE J01 recipe: `--scenario barrage --shot-tick 140 --shot-frames 16 --no-hud`.
- **Fire:** `bash Tools/flameshots <prefix>` gives jet, cook, stand and wall. Then `python Tools/flamecheck.py Tools/flame-shots/<prefix>_jet.png`; exit 1 means no fire.
- **Buildings and vehicles, isolated:** in the Playground, `pg.sh do "building"`, `set Houses`, `house House5`, `shell` and `shot NAME`, or `vehicle` with `ap` / `he` / `ko` / `cook`. `BuildingRig` exposes `Standing`, `Floating`, `OnEnd` and `CutFacesFound`.
- **Before and after:** `CaptureRig.Diff`. Capture the unchanged build twice first to know the noise.
- **Budget:**
  - `FrameBudget.DrawCalls` / `Vertices` at T1 must not rise unless the change pays for it.
  - Draw calls stay under 300 and GPU at or under 13 ms on the target machine (docs/05).
  - 0 B allocated per frame. Every draw goes through `FrameBudget` (FrameBudgetCoverageTests).
  - New mutable statics register with `SceneStatics` (StaticLifecycleTests).
- **Guard tests:** HouseKitTests, TrenchSectionTests, DebrisTests, PropWearTests, DeathEventContractTests, DeathVarietyTests, BlastReactionTests, PlaygroundAssetTests, FrameBudgetCoverageTests, StaticLifecycleTests, ShaderInclusionTests (any new `Shader.Find("TW/…")`).
- **Critic:** `tw-critic` with JUICE's readability check. A moment closes at 8, and any readability drop means revert.

## Traps
- The `Sheets` table is indexed by the `Book` ordinal. Rows 19-21 (Core, Head, Bloom) are out of order today; see the board finding. Three rows are named "Puff": the smoke book is the one with `Erode = true`.
- Six books' tints (Splash, Column, Wings, Spurt, Puff, Smoke) are overwritten every scene by `CombatFx.ApplyTints`. Change the biome profile, not the row.
- **`Explosion.Dir` is already filled** for barrage, creeping barrage, strafe, ship, ambient and tank HE shells, and for
  mines. It is zero only for the Kettle mortar, the jetpack landing and cook-offs (phase 1 corrected the old "ASK S05").
  `CombatFx` already leans the burst by it. What's missing is a drawing that leans. The decision says explosions are
  directional in look **and** damage.
- **`CombatFx` ignores who fired a burst.** Cook-offs, jetpack landings, tripwires, mines and the Kettle mortar all get
  the full shell recipe (earth column, rim clods, 22 s hot crater). The event carries the source, so per-weapon recipes
  are SHOW work.
- **Draw calls were 374 against the 300 ceiling** in the catalogue's barrage frame. A new book must replace cards, not
  add them.
- An `Explosion` with `Scalar < 0.3` does no prop harm. House HP has two scales: battle 1.4 / 1.0 and Playground 60 / 30 (open owner question).
- Brightness scales: CaptureRig is 0-1 Rec.709, `shotstats.py` is 0-255 Rec.601, and the Playground's `luma_mean` is 0-255 Rec.709. Never mix them.
- The non-fire books come from Asset Store packs: they may ship in the game but must not be redistributed on their own.

## Owner questions — ask, never decide
- **Decided on 2026-09-28** (decisions.md):
  - explosion batches B and C: go;
  - the Core/Head/Bloom ordinal fix: its own commit with a test, inside the VFX pass;
  - blood on hits.
- **Still open:**
  - house sim cover (a hash change);
  - where the ruins set goes;
  - shelter protection;
  - the house HP scale;
  - the 11 questions in phase 1 §8, of which Q1 (blood) is answered.
- The owner has decided: trench never caves in; buildings never block movement; deaths are VAT bodies; the house mask is 96 chunks.
