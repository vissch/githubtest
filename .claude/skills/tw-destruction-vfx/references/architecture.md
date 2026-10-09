> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

# Destruction, death and VFX in trench-warfare-3d: reference for a skill

This is a read-only pass. Every number and name below was read from the file cited as `file:line`. Four findings need attention first:
- **Probable flipbook bug:** `FlipbookFx.Book` and its `Sheets` table disagree at indexes 19–21 (§2.3).
- **Stale docs:** several design-doc numbers no longer match the code (§4.5).
- **A third brightness scale:** the playground's `luma_mean` is 0–255 Rec.709, which matches neither CaptureRig nor shotstats (§4.2).
- **Pack count differs:** `G:/My Drive/vfx/sheets` exists and holds 33 files matching the sheet pattern, not the 32 that `pipelines.md` says (§2.4).

**Path keys** (all absolute):
- R = `C:\Users\PC\Documents\GitHub\githubtest`
- P = `R\trench-warfare-3d\Assets\_Project`
- T = `R\trench-warfare-3d\Tools`
- D = `R\docs`

---

## 0. Rules that bind this work

**Lanes** (`R\CLAUDE.md:39-55`):
- **SIM lane** owns `Sim/**`: `BlastSystem`, `DeformationSystem`, `VehicleModulesSystem`, `CraterStamp`, `MapData`, `TerrainHashSystem`.
- **SHOW lane** owns `Presentation/**`, `Editor/**`, `Perf/**`, `Resources/**` and `Shaders/**`: every renderer, effect and capture tool.
- SHOW changes the sim "only through commands (`SimCommand`) or `SimHost.WriteWorlds(...)`" (`CLAUDE.md:50`).

**Seam:**
- `SimWorld.Hash()` is append-only; bump `FormatVersion` when it changes (`CLAUDE.md:56-...`).
- Seam item 4 is "the float4 house chunk mask in `Toon_URP.shader`" (`CLAUDE.md:63`).
- `FormatVersion = 9` (`P\Sim\Core\Replay.cs:33`).

**Gate** (`CLAUDE.md:87-88`), run from the repo root with the editor closed:
- `powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly` (before every commit)
- `... gate.ps1` (before landing, and after any sim change)

**Assemblies**, in dependency order for `python Tools/aosa/occ.py` (`workflow.md:123`): `TW.Presentation.Core` → `TW.Presentation.Units` → `TW.Presentation.Camera` (which holds `CombatFx`, `DebrisRenderer`, `FlipbookFx`, `TankRenderer`) → `TW.Presentation.Terrain` (which holds `PropDestruction`, `HouseKit`, `TrenchSection`). This order was read from each folder's `.asmdef` references.

---

## 1. Architecture: sim → events → presentation

### 1.1 The rule (`D\16-destruction.md:9-16`)

> "The sim decides what breaks. Presentation decides how the pieces fly. The pieces cost the CPU nothing a frame."

- What breaks is already in the hashed sim: `BlastSystem`, `DeformationSystem` (craters, tree → broken tree → stump, wire, wrecks) and `VehicleModulesSystem`.
- Debris is a 96-byte record written once into a `GraphicsBuffer`. There is "no PhysX (banned, docs/05), no GameObject, no allocation after load".

### 1.2 Sim systems, in step order (`D\reference\code-map.md:133-166`)

TerrainHash 50, Blast 720, Beam 723, Burning 725, VehicleModules 730, GasSmoke 900, Deformation 1000, Mines 1130.

**BlastSystem** (`P\Sim\Combat\Blast.cs`)
- `BlastShape { Shell=0, Masonry=1, CookOff=2, Incendiary=3, Beam=4, Mine=5, Strafe=6 }` (`:52`).
- `Impact` fields (`:54-70`): `Pos`, `Dir` (XZ flight direction; zero means no lean), `Damage`, `Radius`, `Suppression`, `CraterRadius`, `CraterDepth`, `Rubble`, `SafeBehind`, `Source`, `Player`, `Shape`.
- `BlastRules` (`:73-100`):
  - `TrenchBayFactor` 0.7, `TrenchTraverseFactor` 0.5, `TrenchOutsideFactor` 0.35, `BayMetres` 12
  - `FieldShadow` 0.45, `CraterFactor` 0.6, `ProneFactor` 0.7
  - `MasonryTrenchFactor` 0.5, `MasonryKnock` 0.5, `TerrainShadow` 0.55, `ShadowFrom` 0.4
  - `MaxRaycasts` 48, `DirBias` 0.3, `MinThrough` 0.08
- Knock constants: `KnockNear` 9, `KnockFar` 3, `KnockReach` 0.85, `KnockMax` 12 (`:105`).
- `Queue(Impact)` (`:131`). `Step` (`:133-167`):
  - emits `Explosion(a=Source, b=Player, pos, dir=(Dir.x, Shape, Dir.z), scalar=Radius)` (`:151-152`);
  - adds a Bowl `CraterStamp` if `CraterRadius > 0` and a Mound if `Rubble > 0` (`:154-157`);
  - calls `Despawn(slot, DeathCause.Blast, (kx,1,kz), speed)` for each man killed (`:161-165`).
- `Hash(h) => h`: its lists are not hashed, by design (`:303`).

**DeformationSystem** (`P\Sim\Match\Deformation.cs`)
- `MaxStampsPerTick = 4` (`:38`).
- Takes over `blast.Craters` (`:85-86`) and runs `Shake` on each resolved impact (`:87`).
- `Shake` (`:140-157`): prop `Hp -= Damage*(1-0.75 d/R)`; at 0 it calls `PropRules.Next` and emits `PropChanged(p, next)`. A breach emits `WireBreached(scalar = CraterRadius*2)` (`:158-166`).
- **Wrecks:** on `VehicleDestroyed` it calls `map.AddProp(PropKind.Wreck)`, falling back to the nearest free cell within 2 (`:92-99`). It then emits `PropChanged(index, Wreck, at, dir.x = slot+1)` (`:102`) and adds a `WreckRecord` plus a `WreckRecorded` event (`:103-116`).
- `WreckRecord` holds `Slot, Generation, Archetype, Team, Cause, Killer, PropIndex, Quality, Pos, Tick` (`:26-34`).
- Applies stamps with `ApplyDynamic` and emits `CraterStamp` with `dir.y` = signed depth, negative for a mound (`:119-129`).
- `Hash`: Wrecks, Applied, PropsChanged, WireOpened, checksum, Queue (`:171-180`).

**VehicleModulesSystem** (`P\Sim\Units\VehicleModules.cs`)
- Pipeline header (`:1-23`).
- Constants (`:35-62`):
  - `TrackFullAbove` 0.8, `TrackThrownBelow` 0.2, `TrackWorstFactor` 0.35, `TrackMismatch` 0.15
  - `EngineWorstFactor` 0.45, `LegLoss` 0.16, `LegFloor` 0.25
  - `FireGrowth` 0.035, `BailFire` 0.6, `CookOffMin/Max` 20/70 ticks, `BurnOutMin/Max` 240/480
  - `CookOffDamage` 380, `CookOffRadius` 9, `CookOffCrater` 2, `ObliterateShare` 0.5
- `Fire` array (`:76`); `StartFire` is private (`:336`).
- `KnockOut` (`:541-555`).
- `Blow` queues the cook-off `Impact` with crater depth 0.35 and emits `VehicleCookOff` (`:558-571`).
- `Wreck` calls `Despawn`, which emits `VehicleDestroyed` (`:646-651`).
- `Hash` covers every module array (`:653-673`).

**CraterStamp** (`P\Sim\Terrain\CraterStamp.cs`)
- `CraterKind { Bowl=0, Mound=1 }` (`:35-41`).
- `HoleRecord { Center, Radius, Depth, RimAt, Hits }` (`:45-54`).
- Constants (`:64-86`): `MaxRadius` 12, `MaxDepth` 4.5, `MergeShare` 0.75, `GrowShare` 0.6, `DepthShare` 1.2, `RimShare` 0.25, `MaxRim` 0.5, `RimWidth` 0.3, `TrenchKeep` 1, `TrenchGuard` 4, `MaxUnderWater` 1.8, `MaxHoles` 512.
- `Apply` is for the authored map; `ApplyDynamic` is the in-match path that grows holes, throws rims and obeys the guard band (`:89-93`). `Hard()` (`:96-100`).

**MapData** (`P\Sim\Terrain\MapData.cs`)
- `Version` (`:22`), `Props` (`:61`), `Holes` (`:67`).
- `CellTrenchDist` (`:72`) starts at 255 (`:100-101`). Zero would mean "every cell touches a trench" and nothing would dig.
- `Bedrock` (`:76`), `BedrockBelow = 6` (`:81`).
- `AddProp` (`:176`), `SetPropKind` (`:192`), `BuildTrenchDistance` (`:228`).
- `Hash` (`:281-303`) folds in: `Height.Cm`, `NavLayers`, `NavCost`, `CellTrenchId`, `CellCover`, `CellTrenchDist`, `Bedrock`, every Hole field, `WaterLevel`, the sea fields, and each prop's `Pos/Hp/Scale/Cell/Kind`.

**TerrainHashSystem** (`P\Sim\Match\TerrainHashSystem.cs:22-33`)
- Steps nothing; `Hash => map.Hash(h)`.
- About 105 KB of FNV per hashed tick. Single player sets `HashInterval = 0`, so only the canary, tests and replays pay it (`:15-17`).

**Hash seam, in one list**
- `SimWorld.Hash` folds in every system's hash (`P\Sim\Core\SimWorld.cs:424`).
- **In the hash:** ground, holes and props (through TerrainHashSystem), Deformation (wrecks), VehicleModules.
- **Not in the hash:** BlastSystem's lists, and everything in `Presentation`: prop damage, trench lining state, house chunks, debris, scorch.

**Event contract** (`P\Sim\Core\SimEvents.cs`)
- `Explosion` (`:14`), `CraterStamp` (`:15`), `Death` (`:16-17`), `VehicleDestroyed` (`:30`), `WireBreached` (`:31`), `PropChanged` (`:39`).
- `VehicleArmourHit` (`:42`), `VehicleOnFire` (`:44`), `VehicleKnockedOut` (`:47`), `VehicleCookOff` (`:48`), `VehicleCrushed` (`:51`), `VehicleLegLost` (`:53`), `UnitAlight` (`:64`), `WreckRecorded` (`:84`).
- `DeathCause { Blast=-1, Gas=-2, Crush=-3, Burning=-4, Beam=-5 }` (`:89`); `VehicleKillCause` (`:92`).
- `Explosion.a` source bands are in `D\reference\tasks.md:120-123`. Nothing in SHOW reads `Explosion.a` today.

### 1.3 How events reach presentation

`EventPump` (`P\Presentation\Core\EventPump.cs`):
- `Collect` (`:58-63`). `Dispatch` runs "once per render frame after all ticks" (`:65-81`).
- Each subscriber gets a marker `TW.Events.To.<Type>` when `ProfileSubscribers` is on (`:17,31`).
- Consequence: read a dead man through `Animation.TryDeath`, never `State[slot]`. The slot may already hold someone else (`tasks.md:220-221`).

Subscribers:
- `CombatFx.OnSimEvent` (`P\Presentation\Camera\CombatFx.cs:478`)
- `PropDestruction.OnSimEvent` (`P\Presentation\Terrain\PropDestruction.cs:327-349`)
- `TankRenderer.OnSimEvent` (`P\Presentation\Camera\TankRenderer.cs:912`)
- `BattlefieldProps` marks itself dirty on `PropChanged`, `WireBreached` and `CraterStamp` (`BattlefieldProps.cs:178`)
- `GreyboxTerrainView` handles `CraterStamp` and `WireBreached`, keeping at most 64 scorch marks (`GreyboxTerrainView.cs:661-667`). `MaxChunkRebuilds = 2` (`:64`).

### 1.4 Presentation components

**CombatFx** is one partial class in six files (`CombatFx.cs:6-11`). Its handlers:

| Event | What it draws | Where |
|---|---|---|
| Death | `OnDeath` (`CombatFx.cs:602`) → see below | `CombatFx.Deaths.cs:31-94` |
| UnitAlight | lights or douses the drawn torch (`CombatFx.cs:605`) | `CombatFx.Deaths.cs:24-29` |
| Explosion | burst, column, wings, smoke, clods, rim, shake | `CombatFx.cs:617-754` |
| PropChanged | splinters, a puff, `TreeBreaks` | `CombatFx.cs:770-778` |
| VehicleCrushed (b=2) | helmet, limb, lumps; skipped when Gore is 0 | `CombatFx.cs:786-795` |

- `OnDeath` details:
  - skips tank slots (`:36`);
  - reads the death record with `anim.TryDeath(e.A, e.Tick, out rec)` (`:43`);
  - throws `Gibs` when it was a blast, `scalar > 0` and `fly.y > 0.6` (`:72`);
  - a burning man gets `flames.Spill` and `AddSmoulder` (`:76-81`);
  - the body goes in with `units.AddFallen(...)` (`:82`);
  - `MaxSmoulders` 48, `SmoulderSeconds` 8, `SmoulderEvery` 0.45 (`:20-21`).
- `Explosion` details:
  - `LightBurst` filters strafe and beam first (`:621`);
  - wet, melt and damp are worked out (`:622-630`);
  - `lean` comes from `Dir.xz` (`:636-638`);
  - Flash, Column/Splash, Wings×2, Burst, then 7 smoke puffs (5 when close) (`:650-707`);
  - clods through `debris.Burst`/`Heave` (`:719-733`);
  - rim rests and a hot crater (`:735-749`);
  - `CameraShake.Add(p, scalar*1.5)` (`:752`).
- Other `CombatFx` pieces:
  - `DebrisRenderer.ZoomShare` is set every frame (`:830`).
  - Gas is drawn per 4 m cell where concentration is ≥ 0.8 (`:936-962`).
  - The `SceneHooks.CookOff` hook is wired at `:231` and drawn at `CombatFx.Chunks.cs:21-43`.
  - `Gibs` (`CombatFx.Bodies.cs:36-74`), `TreeBreaks` (`:81-107`).
  - Close-up-only work sits behind `if (!close) return;` (`CombatFx.Ground.cs:225`).

**PropDestruction** (`P\Presentation\Terrain\PropDestruction.cs`)

Constants (`:54-84`):
- `Quantum` 0.25, `MaxRemembered` 20000, `BlastReach` 1.15, `CrownFallSeconds` 4.8
- `RoundReach` 1.3, `RoundPower` 1.1, `CrushReach` 2.4, `CrushMinSpeed` 0.25
- `ShelterBags` 16, `BagsPerHit` 5, `FallDelay` 0.45
- `MaxLoose` 48 (knob `props.maxLoose`, clamped 17–1023 at `:150`), `LooseLie` 18, `LooseSink` 2.5
- `MaxFalling` 256, `CookDelay` 0.35, `CookReach` 2.6, `CookPower` 0.8, `KeptForChunks` 16

Test hooks:
- `public void AttachForTests(BattlefieldProps testProps)` (`:166-171`)
- `public void StrikeForTests(Vector3 at, float reach, float power) => Strike(at, reach, power, 1u, false)` (`:174`)
- `SectionPiecesForTests` (`:176`)

How an event becomes harm (`:334-347`):
- An `Explosion` with `Scalar < 0.3` is ignored.
- Otherwise it calls `Strike(pos, Scalar*1.15, Clamp(Scalar/6, 0.15, 1.6), tick*31, shake: Scalar >= 3)`.
- `VehicleFired` with scalar ≤ 0.5 (an AP round) strikes with reach 1.3 and power 1.1.
- A `Shot` event goes to `FireAt` (the gunfire wear in `PropWear`).

Methods, in call order:
- `Update` (`:186-204`), `BuildRules` (`:206-293`), `KeyOf`/`Key` (`:296-303`), `Suppress` (`:307-317`), `Replace` (`:321-325`)
- `Strike` (`:352-392`), `Section` (`:396-411`), `Break` (`:416`)
- **`Finish`** (`:447-457`) calls `Down`, then `Toss` if the rule is Kick, `Throw` for a house chunk, otherwise `Collapse`. After that it calls `SetOff` for a dud and `Shaken`.
- `Crush` (`:460-492`), `MaskOf` (`:496-504`), **`Down`** (`:506-510`), `Collapse` (`:530-549`), `Chip` (`:552-565`), `ShedBags` (`:569-587`), `Flatten` (`:590`)
- **`Shaken`** (`:601-614`) and **`Settle`** (`:618-639`): delay is `FallDelay*(1+0.25*depth)` plus jitter.
- `Cook` (`:660-671`), `Toss` (`:675-688`), **`Throw`** (`:694-715`), `Drop` (`:718-734`), `Fly` (`:747`), `DrawLoose` (`:819`)

**Rules table** (`BuildRules`). Each row is one rule; a building or prop set gets a separate rule for its stone and its timber chunks.

| Kind | Hp | Pieces | Line |
|---|---|---|---|
| Shelters (Sandbag, never collapse) | 1.0 | 5 | `:213-214` |
| ruin, well, wallStub, rebarSlab, barricade (Rubble) | 2.0 | 14 | `:216-217` |
| light timber (Plank, crushed by tanks) | 0.8 | 9 | `:219-221` |
| TrenchWalls lining | 1.2 | 8 | `:235` |
| TrenchFloors lining | 1.5 | 6 | `:236` |
| lone sandbag (crushed) | 0.9 | 8 | `:238` |
| sandbags, gabion | 0.9 | 8 | `:239` |
| TrenchBags lining | 1.0 | 10 | `:240` |
| corrugated, wireFence (Plate, crushed) | 1.0 | 6 | `:242` |
| fieldGun, limber, shellStack, tankTurret, biplane | 1.3 | 8 | `:243-244` |
| stumps, fork | 1.3 | 7 | `:246` |
| fallenLog (crushed) | 1.3 | 7 | `:247` |
| stones (crushed) | 1.2 | 8 | `:249` |
| boulder | 3.2 | 12 | `:250` |
| shellCases | 0.4 | 5 | `:252` |
| dudShell (Cook = true) | 0.5 | 5 | `:254` |
| dropped kit (Kick = true, thrown whole) | 0.2 | 3 | `:256-257` |
| scrub | 0.3 | 4 | `:259-260` |
| lantern | 0.8 | 5 | `:262` |
| Houses set, stone / timber | 1.4 / 1.0 | | `:265-266` |
| Military set, stone / timber | 2.2 / 1.4 | | `:268-269` |
| sliced props, stone / timber | 1.2 / 0.8 | | `:272-273` |
| biplane wing chunks | 0.7 | | `:274` |
| field-gun chunks | 1.1 | | `:275` |

The broken twin of a lining panel collapses to `max(2, Pieces/2)` pieces (`:232`).

**PropWear** is a partial of PropDestruction (`P\Presentation\Terrain\PropWear.cs`):
- `CellSize` 4 (`:35`), `WearTicks` 4 (`:38`), `WearPerRound` 0.03 (`:41`), `SpallBudget` 8 (`:50`)
- `WearPerBag` 0.7 (`:61`), `ShelterShellHp` 3, `ShelterShellErode` 0.08 (`:64`)
- `RoundsToWear`, `RoundsToStrip`, `RoundsToOpenShelter` (`:96-99`)
- `Home(module, page, slot, drawn)` is private (`:105`)
- Rates table: `D\16-destruction.md:293-303`

**TrenchSection** (`P\Presentation\Terrain\TrenchSection.cs`):
- `SectionState { Intact, Damaged, Gone }` (`:8`); `DamagedBelow` 0.5 (`:13`)
- `HeavyRadius` 6, `HeavyPower` 1.05, `HeavyInner` 0.5 (`:17`)
- `PieceLife => LiningLife/(1+LifeJitter)` (`:22`); `LiningLife` 12 (`:23`); `MaxSectionPiecesPerStrike` 120 (`:25`)
- `IsHeavy` (`:27-28`), `Apply` (`:32-39`)

**ScorchTilePainter** (`P\Presentation\Terrain\ScorchTilePainter.cs:1-14`) repaints one tile, bit-exact with the old loop. `Burnt` colour is at `:24`.

**DebrisRenderer + Debris_URP** (`P\Presentation\Camera\DebrisRenderer.cs`, `P\Shaders\Debris_URP.shader`)
- `DebrisMath` (`:30-105`): `BounceKeep` 0.45, `BounceUp` 0.30, `SinkSeconds` 3, `LifeJitter` 0.3.
- **Spend by distance:** `Share` is 1 under 55 m, 0.5 under 120 m, 0.25 beyond (`:96-97`).
- `Piece` enum: Clod, Shard, Plank, Rubble, Sandbag, Plate, Crown, Limb, Helmet, Rifle (`:131-144`).
- **Record layout** (`:148-156`): `P0, Born, V0, LandT, Axis, Spin, Rot0, Tint(a = burn), Scale, Life, Mode, LandY`. `RecordBytes = 96` (`:157`). The shader struct matches (`Debris_URP.shader:33`).
- Global statics: `Gore` (`:169`), `ZoomShare` (`:172`), `Biome` (`:178`), `LavaLevel` (`:182`).
- **Pool sizes**, one per piece kind: `{1024, 512, 384, 512, 256, 256, 64, 256, 128, 128}` (`:189`). Shadows only for Plank, Rubble, Sandbag, Plate, Crown (`:190`).
- Knobs: `debris.capacityScale`, `debris.shareNear`, `debris.shareFar` (`:210-211, 221`).
- API:
  - `Throw(Piece, at, velocity, scale, tint, ref DebrisRng, life=20, burn=0, pose=null)` (`:258`)
  - `Burst(Piece, at, count, speed, scale, tint, life=20, burn=0, up=1.6, lean=default, salt=0)` (`:278`). It applies `ZoomShare` then `Share` (`:280-282`).
  - `Heave(...)` (`:300`); `Topple(piece, pivot, pose, fallDirection, seconds, scale, tint, life=6, angle=1.5)` (`:324`)
- Draws with one `FrameBudget.DrawIndirect` per non-empty pool (`:349-382`).
- The shader reads records with `GetIndirectInstanceID_Base` (`Debris_URP.shader:61`).
- Design notes: `D\16-destruction.md:18-56`. The pool table is at `:30-43`: 3,520 records, 330 KB, at most 10 draws.

**FlipbookFx** (`P\Presentation\Camera\FlipbookFx.cs`)
- `Book` enum (`:12-39`), `Kind` flags `{Upright, Anchored, Mirror, HoldLast}` (`:41`), `Sheets[]` (`:73-132`).
- `MaxCards` 1536 (`:144`; knob `flipbook.maxCards` at `:536`).
- Textures load as `Resources.Load("VFX/" + Name)` (`:546`). `Ready = found == Sheets.Length` (`:570`).
- `Add(Book, at, width, life, kind, velocity, grow, roll, alpha, glow, height, pop, delay, startFrame, soil, soilCap, cut)` returns without drawing if not `Ready` (`:595-597`).
- `Tint(book, color)` (`:586`).
- Book → sheet mapping and the index bug are in §2.3.

**HouseKit chunk mask** (`P\Presentation\Terrain\HouseKit.cs`)
- `ChunkMask` has `BitsPerWord = 24` and `Words = 4` (`:45-90`); `MaxChunks = 96` (`:94`). The 24-bit warning is at `:41-44`.
- `Solve` (`:161`). `BuildWhole` logs an error above `MaxChunks` (`:203-205`). `Place`/`HouseOf` (`:225-227`). `GroundedBelow` 0.35 (`:20`).
- **Toon_URP `_CHUNKMASK`:** header `Toon_URP.shader:16-17`; instanced `float4 _ChunkMask` (`:65-68`); `TWChunk` (`:74`); `shader_feature_local_vertex` in all 4 passes (`:105, 317, 354, 393`).
- The mask is set from `BattlefieldProps.cs:439,457`, `PropDestruction.cs:842` (loose chunks) and `Meta\HomeFrontDiorama.cs:159`.
- `Resources/Env/Houses/HouseMask.mat` keeps the shader variant in a build (`BattlefieldKit.cs:225`).

**SceneHooks.DrawnWreck and tank wrecks**
- Hook declared at `P\Presentation\Core\RenderGround.cs:123-124`.
- `TankRenderer` sets it (`TankRenderer.cs:206-210`): true when a linked wreck is within 1 m.
- `BattlefieldComposer.cs:273` reads it and leaves the prop stand-in out.
- A hull is linked to its wreck prop by `PropChanged` Wreck with `dir.x = slot+1` (`TankRenderer.cs:917-924`).
- `TankRenderer` limits: `MaxWrecks` 48 (`:42`, knob `tank.maxWrecks`, oldest dropped at `:285`); `MaxLoose` 160 (`:775`, knob `tank.maxLoose`).
- `Scrap` plates go through DebrisRenderer (`:793-798`). `Wreckify` (`:827`).
- Cook-off fireball uses `DrawnForHalfLength` 2.5 (`:1019`); `VehicleDestroyed` triggers `Wreckify` (`:1037-1038`); `Blasted` (`:1092`).

**Deaths: VAT launched bodies**
- `P\Presentation\Core\AnimationController.Death.cs`:
  - header (`:1-11`), `DeathRecord` (`:20-31`)
  - `DeathKind { Shot, Blast, Gas, Crushed, Burning, Beam }` (`:34`)
  - `PileRadius` 4, `PileWindow` 12 (`:39`); `DensityPerMan` 0.35, `DensityCap` 5 (`:41`)
  - `DeathRingSize` 256 (`:48`)
  - `public bool TryDeath(int slot, uint atTick, out DeathRecord record)` (`:113`); `Density` (`:126`)
- `P\Presentation\Units\VATRenderer.Fallen.cs`:
  - `ThrowGravity` 14 (`:25`), `MaxFallen` 600 (`:27`), `FallenSeconds` 30 (`:28`)
  - `SinkSeconds` 2.5 (`:29`), `CharredLies` 14 (`:32`)
  - `PileStep` 0.28, `PileMax` 3 (`:36`); `PitchSteps` 32 (`:39`)
- Owner decision: `decisions.md:49`.

**EventVfxRouter** (`P\Presentation\VFX\EventVfxRouter.cs:8-16`) is a stub: `Start()` throws `NotImplementedException("Phase B5 ...")`, and so does `GasVolumeRenderer`. It is listed as not wired in `tasks.md:133-137`. The real effects live in `Presentation/Camera`.

**Design doc `D\16-destruction.md`, section by section:**
- The rule `:7`; renderer `:18`; what throws what `:58`
- PropDestruction `:83`; village houses `:133`; rest of the kit `:222`; gunfire wear `:272`
- Men and machines surviving a shell `:348`; Batch A `:393`; cost `:431`; tests `:443`; seen in Play `:449`; open items `:469`
- Buildings coming down a course at a time `:496` (mask widened `:528`)
- The ground remembers `:583`: growing holes `:595`, trench guard band `:624`, directional burst `:651`, tank failure by degrees `:687`, replay v4 `:706`, the lean `:713`
- The lining breaks in two steps `:739`

---

## 2. VFX

### 2.1 Where effects live
- **tasks.md "Combat effects"** (`:238-270`) names the files: `CombatFx.cs`, `.Mines`, `.Deaths`, `.Abilities`, `.Chunks`, `.Bodies`, `.Ambient`, `CameraShake.cs`, `FlipbookFx.cs` with `Shaders/Flipbook_URP.shader`.
- **Colour:** six books' tints (Splash, Column, Wings, Spurt, Puff, Smoke) are overwritten every scene by `CombatFx.ApplyTints` from `BiomeProfile`. At night the burst lights its smoke through `_TWBurst` / `TWBurstLight` in `TWAtmosphere.hlsl` (`:253-258`).
- **Flamethrower:** `Presentation/Camera/Flamethrower.cs` and `Shaders/Flame_URP.shader` (`tasks.md:280-291`).
- **Night light:** `NightLights` provides `SceneHooks.Flash` and `FireLight`, and the star shell (`NightLights.FireStarShell`, used by `BenchScenarios.cs:201-209`).

### 2.2 Flipbook textures (`P\Resources\VFX\`)
19 PNGs: Burst, Column, FireBall, FireBlast, FireBloom, FireBurst, FireColumn, FireCore, FireFan, FireHead, FireJet, FirePool, FireStand, Flash, Muzzle, Puff, Spurt, Star, Wings.
- The non-fire books come from the SrRubfish and Hun0FX Asset Store packs. They "may ship in the game but must not be redistributed on their own" (`D\reference\pipelines.md:81-82`).
- `Resources/ShaderKeep/` keeps shaders in the build: `KeepFlipbook.mat`, `KeepGroundMark.mat`, `KeepTank.mat`, `KeepTankDisc.mat`, plus URP keepers.

### 2.3 The Book enum and its trap

The trap as written (`tasks.md:289-291`):

> "`FlipbookFx` indexes its sheets by the `Book` enum's ordinal and is only `Ready` when every sheet loads. A missing PNG or a row out of order silently disables every flipbook in the game. Add the enum entry, the row and the PNG together, and check `books.Ready`."

Also: "three sheets are named 'Puff': count the rows, the smoke book is the one with `Erode = true`" (`tasks.md:257-258`).

**Mapping as it stands.** `Sheets[(int)b]` is indexed directly (`FlipbookFx.cs:137`).

| Index | Book (enum) | Sheet row | Match? |
|---|---|---|---|
| 0 | Burst | Burst | yes |
| 1 | Column | Column | yes |
| 2 | Splash | Column (second tint) | yes |
| 3 | Wings | Wings | yes |
| 4 | Spurt | Spurt | yes |
| 5 | Puff | Puff | yes |
| 6 | Smoke | Puff (`Erode = true`) | yes |
| 7 | Gas | Puff (yellow-green) | yes |
| 8 | Muzzle | Muzzle | yes |
| 9 | Star | Star | yes |
| 10 | Flash | Flash | yes |
| 11 | Fire | FireBall | yes |
| 12 | Pyre | FireColumn | yes |
| 13 | Fireball | FireBurst | yes |
| 14 | Jet | FireJet | yes |
| 15 | Blast | FireBlast | yes |
| 16 | Fan | FireFan | yes |
| 17 | Stand | FireStand | yes |
| 18 | Pool | FirePool | yes |
| **19** | **Core** | **FireHead** (`:129`) | **no** |
| **20** | **Head** | **FireBloom** (`:130`) | **no** |
| **21** | **Bloom** | **FireCore** (`:131`) | **no** |

The enum's own comments (`:35-37`) and `firebooks.py:61-76` say Head is the closed bolus (FireHead), Bloom is the ground-rooted terminus (FireBloom) and Core is the stream core (FireCore). Both the enum and these rows came in commit afc6fe8 (2026-09-25). `Flamethrower.cs` uses `Book.Core` at `:329, 733-784`, `Book.Head` at `:621, 668` and `Book.Bloom` at `:811-816`. No test checks the order. **This looks like a real mismatch; confirm it in Play before relying on it.**

### 2.4 firebooks.py (`T\firebooks.py`)
- **Usage** (`pipelines.md:79`): `python Tools/firebooks.py [packdir] [outdir]`, pack default `G:/My Drive/vfx/sheets` (`firebooks.py:25,30`).
- Output defaults to `Assets/_Project/Resources/VFX` (`:31`). Cells are `CELL = 256`, `COLS, ROWS = 8, 4`, and `ALPHA_FLOOR = 6` (`:32-34`).
- The `BOOKS` table maps name → (pack file, mode, root-left, keep) (`:42-77`).
- **Value modes** (`:99-102`): `"luma"` is 0.299R + 0.587G + 0.114B. `"light"` is (max+min)/2. Orange sheets use luma; saturated sheets use lightness (`pipelines.md:88-89`).
- Levels and bands are measured per book, off its own output. "A computed core cut of 1.00 means the band is switched off" (`pipelines.md:90-91`).
- **UNCHANGED / CHANGED check** (`firebooks.py:152-168`): it MD5-hashes only FireBall, FireColumn and FireBurst, then prints `UNCHANGED <name>.png` or `*** CHANGED *** <name>.png`. The other books are not checked: compare them with `git diff --stat` (`pipelines.md:93-95`). It writes no `.meta`.
- On this machine, `G:/My Drive/vfx/sheets` holds 35 files, 33 of them matching `_8x4_12fps_`. `pipelines.md:84` says 32.

### 2.5 flamecheck and flameshots
- `python Tools/flamecheck.py <png> [minpx]` (`T\flamecheck.py:1`).
  - Fire pixels are `(R-B ≥ 60 & luma ≥ 70) | (luma ≥ 190 & R-B ≥ 15)`, after an opening and closing pass. Default floor 15,000 px. Exit 1 means EMPTY (`:12-21`).
- `Tools/flameshots <prefix>` (`T\flameshots:2`):
  - refuses to run if the compile failed (`:15-19`);
  - restarts Play in GreyboxCorridor (`:20-23`) and checks the scene is `GreyboxCorridor/True` (`:28-33`);
  - seeds `Random.InitState(20250925)` (`:42`);
  - for each shot: pause, eval, `stepto`, `tw shot`, flamecheck (`:55-61`);
  - takes four shots, `jet`, `cook`, `stand` and `wall`, written to `Tools/flame-shots/<prefix>_*.png`.

### 2.6 Reference captures and juice moments
- `D\reference\vfx\`: `burst-close-0.2s`, `burst-close-0.6s`, `burst-standard-0.5s`, `burst-standard-1.0s`, `burst-standard-1.75s`, `gallery`, `gas-close`, `gas-standard` (PNG). They are cited only in `D\archive\handoff-2026-09-22.md:195,327`.
- **`D\reference\aosa\JUICE.md`**: a moment is scored 0–10 by the critic from a capture and closes at 8. A failed readability check means revert (`:3-8`).
  - **J01, shell burst at the standard view** (`:14`). Captured with `--scenario barrage --shot-tick 140 --shot-frames 16 --no-hud`. The look: the flash reads for 2–3 frames as a white core with an orange rim, the column leans with the shell's travel, and the smoke holds dark for 4–6 s. The check: men within 10 m stay countable.
  - **J02, tank cook-off** (`:15`): armour scenario; the wreck's silhouette stays readable.
  - **J05, walker leg blown off** (`:18`). **J06, star shell** (`:19`). **J09, gas into a trench** (`:22`): `scenario=vfx`.
  - Every score is still `-` except J03 at 5.

### 2.7 Gas, fire and explosions
- **Gas:** the sim field is `GasSmokeSystem` (order 900). It is drawn as the `Book.Gas` card per 4 m cell (`CombatFx.cs:936-962`). The translucent cubes are the fallback when the books are missing (`:963`).
- **Vehicle fire:** `VehicleModulesSystem.Fire` (tasks.md "Fire in the sim", `:139-144`). The staging helper is `TankCapture.Ignite(slot, fire=0.62f)`, which writes `Modules.Fire[slot]` in both worlds (`P\Editor\TankCapture.cs:95-101`).
- **Men and ground on fire:** `BurningSystem` (sim). The drawn torch is `Flamethrower`, driven by `UnitAlight`.
- **Explosion batches** (`D\16-destruction.md:393-429`, `decisions.md:78`): batch A shipped; B and C are parked.

---

## 3. Tools and rigs

### 3.1 Asset playground (never shipped)

**Scene and menus:**
- Scene `P\Playground\Playground.unity`; library `Playground/PlaygroundLibrary.asset` (`PlaygroundSetup.cs:19-20`).
- Menus: **TW/Playground/Build** (`P\Playground\Editor\PlaygroundSetup.cs:24-25`) and **TW/Playground/Open and Play** (`:84-85`).
- Driving it from code (`D\22-asset-playground.md:32-38`): `TW.Playground.PlaygroundHost.Instance.Queue("vehicle.compare; seq")`.
- Keys (`PlaygroundHost.cs:405-417`): F1 panel, Space freeze, H = `ap Hull 25`, G = fire. A click on a vehicle is an AP hit of 22.

**Every command `PlaygroundHost.Do` accepts** (`P\Playground\Runtime\PlaygroundHost.cs:267-376`). Anything else returns `unknown command <c>`; success returns `ok <line>`.

Scenes (`:274`, built at `:205-247`): `vehicle`, `vehicle.compare`, `unit`, `unit.compare`, `unit.squad`, `mixed`, `building`.

Buildings:
- `set <Set>` (default Ruins) (`:275`); `house <name>` (next one if no name) (`:276-281`)
- `shell [x y z [damage]]`: damage default 70; with no coordinates a golden-angle walk round the building (`:282-291`)
- `rebuild` (`:292`); `cutsdebug 0|1` (`:364`)

Vehicles:
- `lod <k|-1>` (`:293-296`); `seq` (`:297`) runs the timed script: 0.3 s `ap Plate_LF 30 1`, 1.4 s `ap Track_L 45 2`, 2.6 s `he right 38`, 3.8 s `ap Hull 50` (`:256-265`)
- `ap <part> <dmg=30> [tier]` (`:298-305`); `he [left|right] [dmg=35]` (`:306-316`)
- `ko`, `cook`, `fire`, `repair`, `traverse` (`:317-321`); `cookdelay <s=7>` (`:322`); `size <x=1.7>` (`:323`)
- `fly <mps=8> [alt]` (`:324`); `walk <mps=2> [inplace]` (`:325`)

Units:
- `clip <name>` (`:326-332`), `speed <x>` (`:333`), `face <deg>` (`:334`)
- `kill`, `ignite`, `revive` (`:335-337`)

Camera and look:
- `cam follow yaw pitch dist fov`, or `cam x y z yaw pitch dist fov`, or `cam <preset>` (presets close / standard / far / top / default) (`:338-349`)
- `freeze 1|0` (`:350`), `timescale <x>` (`:351`), `biome NightMud|Winter|Lava` (`:352`), `panel 0|1` (`:353`)
- `labels` (`:356`), `ground grid|mud` (`:357`), `lodtint` (`:359`), `model u|v <i>` (`:360-362`), `unitrings` (`:363`)
- `team -1|0|1|split` (`:365-373`)

Measurement:
- `lodfit [path]`, `lodpop [path]` (`:354-355`); `sidehue [path]` (`:358`)
- `shot [path] [w=1600] [h=900]`, default `<project>/Captures/playground.png`, with a `.json` beside it (`:374`)

`Report()` JSON fields (`:863-902`): `luma_mean`/`p95`/`blown_frac` (0–255 Rec.709; blown means above 0.9), `mode`, `fps`, `cards`, and per vehicle `lod`, `tris`, `stage`, `hp`, `fire`, `loose`, `moving`, `pose`. Buildings report `standing`, `chunks`, `floating`, `on_end`.

**pg.sh** (`T\playground\pg.sh:3-6, 17-28`):
- `bash Tools/playground/pg.sh do "cmd; cmd"`
- `pg.sh shot NAME [W H]` writes `$PG_OUT` or `Captures/playground/NAME.png` + `.json` and waits up to 20 s
- `pg.sh report`
- `pg.sh wait SECONDS`
- Needs this checkout's editor in Play in `Playground.unity`.

**round.sh TAG** (`T\playground\round.sh`):
- setup (`:8-9`); lodfit (`:11-12`)
- vehicle destruction: `v1_intact`, `v2_hits`, `v3_burning`, `v4_cookoff`, `v5_aftermath` (`:14-23`)
- close / standard / far (`:25-30`); units `u1`–`u5` (`:32-41`)
- ignite and charred: `u6_close`, `u7_burning`, `u8_charred` (`:43-48`)
- squad and mixed, with 9 frames of m3 (`:50-62`)
- **building** (`:64-75`): `set Ruins` → `b1_intact` → 3 shells → `b2_shelled3` → 5 more → `b3_shelled8` → `b4_standard` → `cutsdebug 1` → `b5_cutfaces`
- lodpop (`:77-80`)

**round2.sh TAG** (`T\playground\round2.sh`): library index order is Brute 0, Croaker 1, Hopper 2, Mercy 3, Skimmer 4 (`:4`). For each machine: `_1_compare`, `_2_hits` (2.0 s after `seq`), `_3_burning`, `_4_cookoff`, `_5_after`, close / moving shots, `_9_standard`, and `pop_<name>.json` (`:9-48`).

**score.py** (`T\playground\score.py`):
- Usage: `python Tools/playground/score.py TAG [PREV] [DIR]` writes `DIR/TAG_scores.json` and prints the table and REGRESSED lines (`:3`).
- **Noise floors** (`:4-7, 43-45`): pop IoU 0.006; block colour 0.6 (1.0 for croaker, hopper, mercy and skimmer); contrast 0.08 (m3 is the mean of 9 frames); side cross-talk 0.002; hue gap 5; fps is shown, never flagged.
- `"frog 2->3 block"` is never flagged (`:61`). The last line is `REGRESSIONS: ...` (`:64`).

**sheet.py:** `python Tools/playground/sheet.py TAG [DIR]` writes `TAG_sheet_{v,u,m,b}.jpg` and a stats table (`T\playground\sheet.py:2`).

**BuildingRig** (`P\Playground\Runtime\BuildingRig.cs`):
- `Kit(set)` (`:43`)
- `Build(set, house, fx, parent, at, yaw, seed)` (`:69-100`):
  - **HP is `(Timber ? 30 : 60) * (Grounded ? 3 : 1)`** (`:93`);
  - `AnchorCorners` gives two ground corners ×1.5 and up to two chunks stacked on each ×2 (`:284-312`);
  - `Stout`: at least 0.6 m thick, or no taller than 3× its thickness (`:315`).
- **`ShellLocal(Vector3 at, float damage, float reach = 4.5f)`** (`:341-362`): a grounded chunk is hit only within 1.5 m (`:351`), then `Settle` runs (`:365-397`).
- Constants: `StoreyDelay` 0.45 (`:29`), `Step` 1/120 (`:28`).
- Counters for tests: `Floating` (`:266-280`), `OnEnd` (`:319-334`), `Standing` (`:337`).
- `Rebuild` (`:426`); `Advance(dt)` (`:438-464`) runs on the building's own clock.
- `DebugCuts`, `ForgetCuts`, `CutFacesFound` (`:106-114`).

**VehicleRig** (`P\Playground\Runtime\VehicleRig.cs`):
- Damage model and tiers (`:8-13`): Intact → Damaged → Immobilised → KnockedOut → CookedOff. Tier 1 accessories, tier 2 running gear, tier 3 turret (only at the cook-off), tier 4 casemate plates (only at the cook-off). The hull never leaves.
- `Stage` (`:22`), `HitKind { AP, HE }` (`:23`); `CookDelay` 7 (below 0 means it never cooks off) (`:55`).
- `Find` (`:217`), `FirstOfTier` (`:221`), `HitPart` (`:323`), `Hit` (`:333`), `HitLocal` (`:338`), `Detach` (`:412`)
- `KnockOut` (`:433`), `CookOff` (`:442`), `FireGun` (`:493`), `Repair` (`:502`), `Advance` (`:547`), `PoseSignature` (`:737`)

**Tumble** (`P\Playground\Runtime\Tumble.cs`): `Step(Bounds box, float metres, float h[, float floor])` (`:32-36`). It works in the owner's local frame, so the copies at three LODs agree bit for bit (`:1-6`).

**PlaygroundFx** (`P\Playground\Runtime\PlaygroundFx.cs`):
- Wraps the game's own `FlipbookFx` and `DebrisRenderer` (`:15-16, 27-32`).
- `Burst` (`:173`), `Smoke` (`:200`), `CookOff` (`:218`).
- It calls `Graphics.RenderMeshInstanced` directly (`:102`). This is allowed only because the budget test scans Presentation and UI, not Playground (§3.4).

### 3.2 Battle bench

**PerfBench** (`P\Perf\PerfBench.cs`):
- The arg is `-twbench`; the env var is `TW_BENCH` (`:73`), consumed once per request (`:90-91`).
- Exit codes (`:77-78`): 0 ok, 2 bad option, 3 settle timed out, 4 desync, 5 write failed.
- **Where the JSON goes:** `Path.GetFullPath(Options.Out)`; the default is `"perf.json"` (`:651`; `BenchOptions.cs:39`). A relative `out=` resolves against the process working directory, so always pass an absolute path.
- `quit=1` leaves Play in the editor and quits the player (`:674-678`).

**How to run it:**
- **Editor:** `TW.Editor.CaptureRig.Bench(string args)` with GreyboxCorridor open and not in Play. It sets `TW_BENCH` and enters Play (`P\Editor\CaptureRig.cs:592-599`). Poll `TW.Perf.PerfBench.LastResultPath`.
- **Player** (`workflow.md:345-348`): `Builds/WinBench/TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 canary=0 shot=<png> out=<json>"`. Adding `-screen-fullscreen 0 -screen-width 1280 -screen-height 720` keeps it in a window.
- **AOSA driver:** `aosa.py bench <label> [--player/--editor] [--scenario S] [--shot-tick N] [--shot-frames N] [--no-hud] [--knobs k=v,...] [--repeats N] [--against L]` (`T\aosa\aosa.py:1491-1509`; `D\reference\aosa\README.md:210-212`).

**BenchOptions.Parse keys** (`P\Perf\BenchOptions.cs:81-128`):
- `stress` (default 1500), `settle_ticks`/`settle` (1800), `ticks` (400), `warm` (120), `ff` (8)
- `quality` (-1; always pass it), `vsync`, `w`/`width`, `h`/`height`, `weather` (120)
- `zoom` (30), `yaw` (21), `pitch` (25), `fx`/`fz` (focus in map metres)
- `subs`, `canary` (-1 / 0 / 1), `label`, `out`
- `shot`, `shot_tick` (-1), `shot_hud` (1), `shot_frames` (1), `quit` (1)
- `scenario`, `knobs` (`a=1|b=2`, and repeated tokens add up), `ground` (ShelledForest / forest / winter / WinterLine; unknown exits 2)
- Unknown keys land in `warnings` (`:124`).

**BenchScenarios** (`P\Perf\BenchScenarios.cs`). `scenario=` values (`BenchOptions.cs:131-140`) are `barrage`, `armour`/`armor`, `vfx`; anything else runs `none`. They are issued after `hash_start` (`BenchScenarios.cs:1-15`).
- **barrage:** each side fires an HE barrage on the enemy's front-trench cell nearest the view (`:58-66`).
- **armour:** every vehicle roster slot is deployed (`:68-90`).
- **vfx:** each side fires an HE barrage 8 m either side of the focus, chlorine 12 m upwind, and `NightLights.FireStarShell()` goes off (`:92-105`).
- Silver and cooldowns are topped up through `SimHost.WriteWorlds` (`:109-127`).
- A bench `shot=` includes the HUD. `shot_tick=N shot_hud=0` hides it and holds the clock, so those timings are not real time (`tasks.md:534-535`).

**Relevant run-time knobs** (via `knobs=`, `-twknob`, or `TW_KNOBS`):
- `debris.capacityScale`, `debris.shareNear`, `debris.shareFar`
- `flipbook.maxCards`
- `props.maxLoose`, `props.maxFalling`
- `tank.maxLoose`, `tank.maxWrecks`
- `fx.maxBodies`, `fx.maxChunks`, `fx.maxAmbientChunks`, `fx.maxMarks`, `fx.columnScale`, `fx.closeReach`, `fx.tracerSeconds`
- look knobs `fx.smokeAlpha`, `fx.burstGlow`, `fx.smokeNightSize`, `fx.smokeHard`, `fx.columnHard`, `fx.columnEarthSize`, `fx.columnSoil`, `fx.columnCap`, `fx.columnPlay` (`FlipbookFx.cs:155-497`), `fx.shotStagger`

### 3.3 CaptureRig, TankCapture and RiderLab (`tw eval` helpers)

**CaptureRig** (`P\Editor\CaptureRig.cs`):
- `Shot(path, x, z, zoom, yaw, pitch=25, w=1600, h=900, aimY=NaN)` (`:348`)
- `Pending()` (`:356`); `ShotCrowd(path, zoom, yaw, pitch=25, w, h, cell=20)` (`:386`)
- `Series(dir, stem, x, z, zoom, yaw, pitch=25, count=8, everyFrames=6, w, h, aimY)` (`:398`)
- `Sheet(dir, stem, outPath, cols=4, cellW=400)` (`:416`)
- `Hold(weatherClock=120)` (`:465`), `Release()` (`:474`)
- `Diff(pathA, pathB, outPath=null)` (`:490`): writes `changed_frac` and the luma deltas
- `LastReport()` (`:583`), `Bench(args)` (`:592`), `Profile(path, frames=300)` (`:608`)
- `Stress(unitsPerSide=1000, path="stress.json", frames=300, settleSeconds=120)` (`:721`): has no fixed tick and no hash, so never use it for A/B (`workflow.md:431-432`)

**TankCapture** (`P\Editor\TankCapture.cs`):
- `Spawn(team, archetype, x, z, yawDeg=-999)` (`:78-92`)
- `Ignite(slot, fire=0.62f)` (`:95-101`)
- `Status()` (`:103`), `Shot(path, w, h)` (`:65`), `Follow(slot, zoom=22, yaw=30)` (`:67`), `Silver(amount)` (`:69`)

**RiderLab** (`P\Editor\RiderLab.cs`):
- `Setup(archetype=6, riders=8, x, z, team, yawDeg, climb)` (`:44`)
- `Kill(slot)` calls `World.Despawn(slot)`, which emits Death and VehicleDestroyed, so a wreck forms (`:157-163`; `SimWorld.cs:217-225`)
- `Drive(slot, dz)` (`:142`), `Enemies(...)` (`:166`), `Freeze(on)` (`:212`), `Camera(...)` (`:222`), `Film(dir, seconds, fps, w, h)` (`:231`)
- `Fire(on)` only toggles `TankRenderer.RidersFire` (`:210`); it is not a weapon.

**Queuing a shell by hand:** `MatchSim.Blast` is public (`P\Sim\Match\MatchSim.cs:34`). An eval of `h.WriteWorlds(m => m.Blast.Queue(new TW.Sim.Combat.Impact{...}))` fits the pattern the tests use (`BattlefieldTests.cs:103`), but no helper exists and I did not run it.

**The rest of workflow §6** (`D\reference\workflow.md:187-331`):
- stills (`:193-195`), top-down walker (`:197-203`), luma numbers (`:204-214`)
- `tw shot` + `shotstats.py` (`:217-230`)
- timed effects: `tw pause true` → eval → `tw stepto 1.5` / `tw step 12` → `tw shot` → `tw pause false` (`:250-260`)
- repeatable captures with `Random.InitState` and `tw stepabs` (`:262-268`)
- the generated helper table (`:276-331`)

### 3.4 Blender splitters (they produce the destructible chunks)

- Blender runs headless: `"C:\Program Files\Blender Foundation\Blender 5.0\blender.exe" -b --factory-startup -P <script> -- <args>` (`pipelines.md:17`). Blender 5.0 is installed at that path on this machine.
- **UNRECORDED warning** (`pipelines.md:11-13`): "Parameters the last run used were not recorded for the Blender splits. Where a table below says UNRECORDED, re-derive the value from the current output (`houses.json`, the FBX sizes) before regenerating. A fresh run with defaults will not reproduce what ships."
- The committed chunks were cut before the cap fix and are not regenerated (`decisions.md:88-94`).

Splitter usage and flags:

- **housesplit.py** (`pipelines.md:22`; `T\housesplit.py:17-18`)
  - `TW_TEX=<basecolor.jpg> TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4 blender -b --factory-startup -P housesplit.py -- <fbx> <outdir> <renderdir>`
  - Script defaults: `TW_SCALE` 22.7, `TW_CUT` 1.8, `TW_SMALL` 0.7, `TW_PIVOT` house (`:26-31`)
  - Flags: `TW_NAMES`, `TW_TURNS`, `TW_LOOSE=1` (`:36`), `TW_ONE=1` + `TW_KEEP=1` (`:70-71`), `TW_TEX` (`:95`), `TW_OLDCAPS=1`, `TW_CAPLOG=1` (`:182-184`), `TW_FLOORS=1`, `TW_STOREY` 1.8, `TW_BAND` 3.2 (`:191-193`), `TW_MINTRIS` 24 (`:365`)
  - Recorded values: only Houses (`TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4`) and Biplane (`TW_CUT=1.9`)
  - Kit-prop slicing: `TW_LOOSE=1 TW_ONE=1 TW_KEEP=1 TW_SCALE=1` (`D\16-destruction.md:247`)
- **tank3split.py** (`pipelines.md:25`; `T\tank3split.py:18`)
  - `-- <Name> <lod0.fbx> <lod1.fbx> <lod2.fbx> <outdir> <renderdir>`
  - `TW_SCALE` 6.6 (`:28`); `TW_DERIVE` "12" (`:530`; lower LODs derived from LOD0 per `decisions.md:29`)
- **jeepsplit.py** (`pipelines.md:27`; `T\jeepsplit.py:18`)
  - `-- <name> <lod0.fbx> [<tripo lower lod.fbx>] <outdir> <renderdir>`
  - `TW_SCALE` 4.6 (`:29`), `TW_SYM` (`:264`), `TW_LOD2_TRIS` (`:377`), `TW_LOD2=tripo|derive` (`:383`), `TW_REBAKE` (`:405`)
- **mechsplit.py** (`pipelines.md:28`; `T\mechsplit.py:29`)
  - `-- <name> <lod0.fbx> [<lod1.fbx> [<lod2.fbx>]] <outdir> <renderdir>`
  - `TW_KIND` walker|flyer|hover (`:38`), `TW_SCALE` 6.6 / flyer 8.0 / hover 7.0 (`:40`), `TW_TURN` (`:149`), `TW_LOD2_TRIS` (`:315`), `TW_LOD2` (`:316`)
  - Break tiers (`:44-`)
- **crabsplit.py**: `pipelines.md:24` writes `-- <sheet...> <outdir> <renderdir>`; the script header says `-- <pincer> <kettle> <censer> <pavise> <outdir> <renderdir>` (`T\crabsplit.py:28`).

Silent splitter traps (`pipelines.md:30-37`):
- **Axis:** the export turns every part 180° first.
- **MAX_PATH:** copy sheets to a short path.
- **Parenting:** every part must be parented to the Body.
- **Re-slicing:** needs `TW_KEEP=1`.
- **Read/Write:** vehicle and chunk meshes must stay Read/Write enabled; the import postprocessor resets a manual setting.

### 3.5 Tests that guard this area

EditMode tests are in `P\Tests\EditMode\`. Run one class with `Tools/tw run run_tests -- --mode EditMode --filter TW.Tests.<Class> --timeout 300`, after `editor_stop` and a script reload (`workflow.md:139-141`).

- **HouseKitTests:**
  - `A_Mask_Holds_Every_Chunk_A_House_May_Have_And_Each_Word_Survives_A_Float` (`:17`): the 24-bit-per-word guard
  - `A_Wall_On_A_Wall_Rests_On_It...` (`:47`), `A_Piece_With_Nothing_Within_Reach...` (`:67`)
  - `A_Ruin_Is_Cut_Into_Courses_So_It_Comes_Down_From_The_Top` (`:82`)
  - `The_Imported_Houses_Stand_And_Every_Chunk_Is_Held_From_The_Ground` (`:133`)
  - `A_Chunk_Is_Drawn_At_Its_Offset_In_A_Turned_House` (`:161`)
  - `Each_Building_Of_Each_Set_Draws_As_One_Mesh_With_Every_Vertex_Tagged_By_Its_Chunk` (`:172`)
  - `A_Sliced_Kit_Prop_Puts_Every_Vertex_Of_The_Whole_Prop_Back_Where_It_Was` (`:204`)
- **TrenchSectionTests:**
  - `Damaged_At_Half_Gone_At_Nothing` (`:20`), `Heavy_Ordnance_Shatters_A_Section_Outright` (`:36`)
  - `A_Sections_Pieces_Are_Gone_In_Fifteen_Seconds` (`:49`)
  - `One_Strike_Leaves_A_Section_Damaged_And_The_Next_Finishes_It` (`:62`), which uses `AttachForTests` and `StrikeForTests`
  - `A_Barrage_Over_Forty_Sections_Spends_Exactly_The_Ration` (`:93`)
  - `Every_Lining_Panel_Has_A_Damaged_Twin_On_The_Same_Footprint` (`:138`)
  - `The_Lining_Is_Hit_Per_Panel_Not_Per_Sack` (`:149`), `The_Lining_Is_Sized_By_The_Composer_Not_The_Clamp` (`:163`)
- **DeathEventContractTests:** what `Death.b`, `dir` and `scalar` mean (header `:1-4`)
  - `ABlastDeathCarriesItsCauseAndTheWayHeWasThrown` (`:53`), `TheFarSideOfTheBurstThrowsTheDeadHarder` (`:74`)
  - `AManTheShellOnlyWoundedIsNotADeath` (`:93`), `AGasDeathSaysSo` (`:108`), `AShotDeathKeepsTheKillersSlot` (`:124`)
- **DeathVarietyTests:** the death ladder, the ring outliving the slot, the heap and density, VatPad char bits, pitch encode/decode, torch expiry.
  - Tests: `ABlastDeathIsThrownTheWayAndAsHardAsTheSimSays`, `TheFifthManDownInABayIsThrownFurtherThanTheSameManAlone`, `AManWhoDiesAlightDropsMidStrideAndLiesCharred`, `GasAndATrackHaveTheirOwnDeaths`, `TheRecordOutlivesTheSlot`, `ASlotFilledAgainInTheTickItWasFreedStillRecordsTheDeath`, `TheBodyCapTakesTheSoonestGoneAndATieTakesTheLater`, `AWalkersSlotRefilledInTheTickItDiedRecordsNoDeath`, `ABodyUnderAHeapIsNotGoneBeforeTheManOnTop`, `TwoMenShotStandingBesideEachOtherDoNotDieTheSameWay`, `VatPadCarriesTheCharBitsBesideEverythingElse`, `TheTumbleEndsUprightAndAHeapedManLiesTilted`, `AVehiclesMachineGunKillIsNotACrush`, `ACharredBodyOutlastsItsSmokeAndItsEmbers`, `TheShaderDecodesThePitchStepTheRendererEncodes`, `ATorchGoesOutAfterItsSecondsOfSimTime`, `AStrafedManDropsWhereHeStoodOnScreenToo`
- **BlastReactionTests:**
  - `AManThrownHardIsBlownOffHisFeet_ThenGetsUpDazed` (`:71`), `TheReactionRunsOutThroughTheMen` (`:102`)
  - `TheNearerAManIsTheMoreEarthHeWears` (`:123`), `ThePadCarriesTheLimbsTheGrimeAndTheSeed` (`:142`)
  - `AHullOnItsSpringsSettlesHoweverLongTheFrame` (`:157`), `TheCameraFeelsABurstWhenTheSoundArrives` (`:179`)
- **DebrisTests:**
  - `TimeToHeight...` (`:17`), `Landing_OnFlatGround...` (`:30`), `Landing_OnAStep...` (`:41`)
  - `PositionAt_NeverGoesBelowTheRestHeight...` (`:56`), `PositionAt_IsContinuousAtTheLanding` (`:77`)
  - `Share_SpendsThePoolsUnderTheEye` (`:87`), `Rng_IsSeededByPlace...` (`:96`)
  - `Record_IsTheShaderStride_AndEveryPieceHasAPool` (`:115`): 96 bytes, and every pool under 0.5 MB
- **PropWearTests:** 8 tests locking the 4–6 s machine-gun window, rifleman vs machine gun, concrete, dropped kit, shelters stripping first, stray fire never sweeping, the tick-tied pass and its debris ration, the knock size.
- **WreckRecordTests:** `AMachineThatDiesWhole...` (`:49`), `AShelledHullThatBurnsThrough...` (`:80`), `TheRecordSurvivesItsSlotBeingReUsed` (`:97`), `TheRecordIsInTheHashAndTheSectorSystemNamesTheGround` (`:116`)
- **DynamicGroundTests** (holes and guard band):
  - `EveryMapIsBornKnowingHowFarItsGroundIsFromATrench` (`:59`), `TheGroundDigsMoreTheFurtherItIsFromATrench` (`:77`)
  - `ATrenchStandsThroughABarrage...` (`:100`), `ASecondShellInTheSameHoleWidensIt...` (`:127`)
  - `AShelledSpotGrowsOneBigHoleThatStopsAtMaxRadius` (`:149`), `SpoilIsThrownUp...NeverBuildsAMountain` (`:167`)
  - `AHoleShelledTwentyTimesStillHasALip...` (`:189`), `AMoundRaisesTheGround...` (`:217`)
  - `NoBarrageEverDigsThroughTheBedrock...` (`:237`), `TheGroundIsInTheTickHash` (`:257`), `TwoMachinesShellingTheSameGroundAgreeOnIt` (`:267`)
- **DirectionalBlastTests:** 8 tests covering far side vs near side, no-flight bursts, bay factor 0.7, `FieldShadow`, the raycast cap, masonry, `MinThrough`, determinism.
- **ScorchTilePainterTests:** `Tiles_WithOverlappingScorch_AreTheOldLoopsTexels`, `Texel_RoundsAsSetPixelDoes_OnEveryBytesEdge`.
- **FrameBudgetCoverageTests** (`P\Tests\EditMode\FrameBudgetCoverageTests.cs`):
  - `EveryDrawGoesThroughFrameBudget` (`:43-64`): no `Graphics.Render*`/`Draw*` or CommandBuffer draw anywhere under `Presentation` or `UI` (`:17`), except `RenderGround.cs` and `DebugOverlay.cs` (`:18`)
  - `ThePatternSeesFrameBudgetsOwnCalls` (`:67`)
- **StaticLifecycleTests:**
  - `Every_Mutable_Static_Is_Reset_When_Play_Ends_Or_Explained` (`:87`): assemblies `TW.Presentation*`, `TW.UI`, `TW.Perf` (`:74`); DebrisRenderer's statics are explained at `:36`
  - `The_Camera_Forgets_The_Match_When_The_Session_Ends` (`:108`), `Scene_Hooks_Are_Empty_When_The_Session_Ends` (`:120`)
- **ShaderInclusionTests:**
  - `EveryShaderTheGameFindsByNameIsInTheBuild` (`:24`): every `Shader.Find` must be in Always Included Shaders or used by a Resources material
  - `TheUrpKeepersCarryTheVariantsTheCodeUses` (`:59`), `EveryAlwaysIncludedInstancedShaderHasAnInstancingKeeper` (`:76`)
- **Column tests:** ColumnPlayTests and ColumnLightTests guard the `fx.columnPlay`, `fx.columnCap` and `fx.columnBurstLit` knobs. TracerGlowTests and ShotStaggerTests cover tracers.
- **PlaygroundAssetTests** (`P\Playground\Tests\PlaygroundAssetTests.cs`, namespace `TW.Tests.Playground` at `:15`, an Editor-only asmdef):
  - `A_Shelled_Building_Leaves_Nothing_Floating_And_No_Piece_On_End` (`:236-260`): Ruins, seed 11, eight `ShellLocal(..., 70f)` with 45×1/60 s steps, then 1200 steps. It asserts `Standing < Pieces.Count`, `Standing > 0`, `Floating == 0`, `OnEnd == 0`, and that every anchored grounded piece is `Stout`.
  - Also `Three_Copies_At_Three_LODs_Fall_Apart_Identically`, `Every_Machine_Keeps_Its_Thrown_Parts_Near`, `A_Vehicle_That_Loses_A_Wheel_Sits_Down...`, `A_Flyer_Hovers_And_Falls_When_Knocked_Out`, and the LOD, part and track tests.
- **Other nearby tests:** BurningSystemTests, TankMobilityTests (track and engine curves; vehicle fire), MineTests, HollowRescanTests / DrainageTests (crater hollows, presentation), BattlefieldTests.

---

## 4. Traps

### 4.1 Trap lines in tasks.md for these areas (`D\reference\tasks.md`)
- `:45-46` `Hash()` is an ordered chain. Append, never insert; a new array must be hashed and must bump `FormatVersion`.
- `:56-57` Single player runs one world. Write through `SimHost.WriteWorlds(...)`.
- `:76` A feature tested only on the playtest map is untested.
- `:114-115` `MineSystem.Place` is a system call.
- `:119` "a trench never caves in, by owner decision."
- `:168` `VehicleSize` is baked into mesh vertices at load.
- `:207-210` Any clip or discard in `VAT_URP` goes behind `_TW_LIMBCUT`. Use `GetIndirectInstanceID_Base`. `Tint` = team + 2·pitch step, so never read `Tint` as the team. `VatPad` is 24 bits and full.
- `:220-221` Read a dead man via `Animation.TryDeath(slot, eventTick)`, never `State[slot]`.
- `:234` Vehicle meshes must import Read/Write enabled.
- `:269-270` Close-only effects sit behind `if (!close) return;` in `CombatFx.Ground.cs` `CloseLife`.
- `:289-291` The FlipbookFx Book ordinal trap (§2.3).
- `:307-310` A prop's key is its position at 0.25 m. Go through `PropWear.Home` first. A twin shares its panel's `KeyOf`.
- `:335-341` The env atlas grid must match `envatlas.py`. A building is "reported, never clamped". Scatter must not read the surface.
- `:356-357` Post-processing needs `TW-Renderer.asset` → `PostProcessData`.
- `:378` Setting `Camera.main.transform` does nothing; use `FrameFrom`.
- `:502-503` The playground is not in the build; assets must be moved into `Resources/` and the battle's renderers.
- `:512-513` Never clear `SceneHooks` from `SceneStatics.Reset`.
- `:531-535` The bench report carries the frame budget twice. `GC.GetAllocatedBytesForCurrentThread` reads 0. `shot=` includes the HUD.

### 4.2 workflow.md gotchas
- **Compile:** a failed compile keeps running the old assemblies. Check `compilationFailed` (`:116-118`).
- **False reds** (`:174-185`):
  - statics after Play: run `RequestScriptReload` and rerun;
  - pipeline noise in a batch run;
  - **the first run after a Burst job edit can run managed code**, so rerun before trusting one determinism failure (`:182-183`);
  - a stale `Library/BurstCache` shows as NullReference or IndexOutOfRange in Burst jobs.
- **Read numbers before eyes:** "Do not judge a picture by eye first. Read the numbers, then look." (`:189`; also `tasks.md:30-31`).
- **Brightness scales:** `CaptureRig` `luma_mean` is 0–1 Rec.709, blown above 0.90; `shotstats.py` is 0–255 Rec.601, blown at 250 and up. "Never compare a number from one with a number from the other." (`:204-208`)
  - The playground's `luma_mean` is a third scale: 0–255 Rec.709, blown above 0.9·255 (`PlaygroundHost.cs:870-872`).
- **Staging traps** (`:236-248`): `FrameFrom`; `ZoomMin` 6; spawned units get moved by the sim; freeze with `SimHost.TimeScale=0` and `Time.timeScale=0`; `RenderGround.Sample`; **do not diff paused frames**; let ticks run before `Hold()`; `AlignWorlds` may say "a tick apart", so retry.
- **Timed effects:** pause before creating the effect. This was last verified 2026-09-25 (`:250-260`).
- **Noise:** rain and wind move 1–3% of pixels; a ninefold spread was measured. Seed and use `stepabs` (`:228-230, 262-268`).

### 4.3 FrameBudget rule
- Every gameplay draw goes through `FrameBudget.Draw` or `FrameBudget.DrawIndirect` (`P\Presentation\Core\RenderGround.cs:174-232`), enforced by FrameBudgetCoverageTests (`tasks.md:528`).
- `Vertices` excludes indirect draws (the men and the debris) (`RenderGround.cs:190-198`).
- The comment at `RenderGround.cs:163-165`, saying PropDestruction and BattlefieldProps still draw directly, is stale: the test now forbids it, and a grep shows no such calls.

### 4.4 Budgets that matter here (`D\05-performance-budgets.md`)
- **Frame:** 60 fps at 1080p on a GTX 1050 (`:5-6`).
- **CPU per frame:** event pump 0.2 ms (`:27`); main thread ≤ 3 ms (`:32`).
- **GPU:** "VFX, decals, post … 2.0 ms" (`:42`); volumetric gas 1.5 ms (`:41`); total ≤ 13 ms (`:44`).
- **Sim tick:** Deformation + cost update 0.20 ms, ≤ 4 stamps a tick (`:18`).
- **Memory:** corpse instances ≤ 1,000 (`:53`); **0 B/frame allocation in play** (`:54`).
- **Draw calls:** < 300 (`:63`), measured at 374 (`:185`). Debris adds at most 10, each with its outline pass (`D\16-destruction.md:438-439`).
- **Banned** (`:65-76`): `Instantiate`/`Destroy` in combat; runtime mesh slicing for gore; PhysX (per docs/16).
- `docs/05:29` and `:73` still budget ragdolls (≤ 15 active, a 15-slot pool). That contradicts the owner decision: no ragdolls (`decisions.md:49`).

### 4.5 Stale or contradictory docs (the code wins, per `CLAUDE.md`)
- `D\16-destruction.md:600` says `DepthShare 0.6`; the code has 1.2 (`CraterStamp.cs:77`).
- `:620` says Bedrock is "less 1.5 m"; the code has `BedrockBelow` 6 (`MapData.cs:81`) and `MaxDepth` 4.5.
- `:211-212` says "at most 24 chunks"; now 96 (`HouseKit.cs:94`).
- `:708-710` says `FormatVersion` 3 → 4; it is now 9.
- `pipelines.md:17` says "All four run headless" while the table lists eight Blender scripts.
- `crabsplit.py` usage differs between `pipelines.md:24` and the script (§3.4).

---

## 5. tasks.md sections for this area (task → files → tests → how to see it)

| Heading | Files, tests, see it |
|---|---|
| `### Shells, barrages, gas, fire` (`:102-123`) | Blast.cs (BlastRules), OffMapAbilities, GasSmokeField, SmokeLos, Burning.cs, BeamSystem, Mines, Deformation. Tests: SupportAbilityTests, DirectionalBlastTests, BurningSystemTests, AbilityArgsTests, StrafeRunTests, BarragePatternTests, SmokeScreenTests, BeamTests, MineTests, AirDropTests. |
| `### Fire in the sim` (`:139-144`) | VehicleModules (Fire, StartFire, `UnitFlags.Burning`, VehicleOnFire); Burning.cs. Tests: TankMobilityTests, BurningSystemTests. |
| `### Vehicles in the sim` (`:146-155`) | The wreck is a map prop; `WreckRecord` lives in `DeformationSystem.Wrecks`. Tests include WreckRecordTests. |
| `### Map generation and ground` (`:69-76`) | CraterStamp, MapData; "Deformation.cs (the only thing that edits the map)". Tests: BattlefieldTests, DynamicGroundTests, CoastTests, WinterMapTests. |
| `### Infantry rendering (VAT)` (`:199-210`) | VATRenderer.Fallen. Tests: VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, DeathVarietyTests. |
| `### Animation` (`:212-221`) | AnimationController.Death. Tests: BlastReactionTests, DeathVarietyTests, TickAllocationTests. See it: `Animation.Follow(slot)` then `Animation.TraceText()`. |
| `### Tanks and walkers drawn` (`:223-236`) | See it: `TW.Editor.TankCapture.Spawn(team, archetype, x, z)`, read the position back, `SimHost.TimeScale = 0`. |
| `### Combat effects: tracers, bursts, smoke, camera shake` (`:238-270`) | Tests: BlastReactionTests, ComponentLookupAllocationTests, AbilityAimTests, ShotStaggerTests, TracerGlowTests, ColumnLightTests, ColumnPlayTests. |
| `### Ground marks and footfalls` (`:272-278`) | Tests: none. See it: a top-down capture in Play. |
| `### Flamethrower` (`:280-291`) | Tests: none. See it: `Tools/flameshots <prefix>` and `Tools/flamecheck.py`. |
| `### Debris and destruction of props and houses` (`:293-310`) | DebrisRenderer + Debris_URP, PropDestruction + PropWear, TrenchSection, HouseKit; design docs/16. "How harm works" `:299-305`. Tests: DebrisTests, PropWearTests, HouseKitTests, TrenchSectionTests. |
| `### Terrain view, weather, night, biomes` (`:343-360`) | ScorchTilePainterTests, HollowRescanTests, DrainageTests. |
| `### Asset playground` (`:489-503`) | Tests: PlaygroundAssetTests. |
| `### Statics that outlive a match` (`:507-513`) and `### Performance and allocations` (`:520-535`) | FrameBudgetCoverageTests, BenchOptionsTests, KnobsTests. |
| `### Windows build` (`:537-540`) | ShaderInclusionTests. |
| Generated SceneHooks table (`:546-564`) | `DrawnWreck`: TankRenderer → BattlefieldComposer. `CookOff`: CombatFx → PropDestruction. `Sparks`, `Flash`, `IsTankSlot`, `CloseUp`. |

---

## Owner decisions (verbatim rows, `D\reference\decisions.md`)

- `:25` "Battlefield: river is an obstacle with fords and a bridge; trees, stumps and wrecks give cover, block, and are destructible; fixed seed and params per mission."
- `:31` Man-made props are true to the soldier; machines stay giant (supersedes `:27`).
- `:42` "Shelters lose sandbags but stand, and reduce artillery damage."
- `:43` "**The trench always stands and always gives limited protection.** No trench cave-in. A shell in a man's own bay does 0.7 damage (`BlastRules.TrenchBayFactor`)."
- `:44` "Buildings go into the sim with cover, artillery protection and collapse damage, but **never block movement**."
- `:45` "Explosions are directional in look **and** damage. The ground is permanently dynamic and a hole grows with repeated hits."
- `:46` "Walkers limp as they lose legs; tank tracks degrade by degrees."
- `:47` "Shelters are not exempt from small-arms wear: bags go first (~1 min of one MG), then the shell at concrete's rate."
- `:48` "House chunk mask widened to 96 chunks (`HouseKit.MaxChunks`), so ruins come down a course at a time."
- `:49` "**Death physics are VAT launched bodies, not physics ragdolls.** … presentation only, seeded so replays agree."
- `:29` Lower LODs are derived from LOD0 (`TW_DERIVE=12`).

## Owner questions: never build without asking

These are the open items (`decisions.md:72-96`; ask the owner in the chat, in Claude Code with AskUserQuestion, then move the answer up):
- "**Sim protection from shelters:** map-generator shelter positions, or trench-bay protection? `NavLayer.Bunker` is never set today." (`:75`)
- "**Do houses give sim cover?** Needs a hash change." (`:76`)
- "**Where the ruins set goes.** It is cut, imported and tested, and nothing places it." (`:77`)
- "**Explosions batches B and C** (sky flash, shock ring, per-weapon recipes; scar layer, smouldering craters, haze). Batch A shipped; B and C wait for a go." (`:78`)
- Not yet scaled with the giant machines: trench cross width, slope limit, turn rates, speeds, `MaxGrow` (`:79`).
- **The house kits' tan in a night ruin:** re-cut the sets or leave them. Split settings are unrecorded, and re-cutting changes every building (`:88-94`).
- Whether a collapsed shelter stops giving cover in the sim, which is a hash bump (`D\16-destruction.md:489-490`).
- Playground items: whether flyers exist in the sim, the machines' scale, and repainting (`D\22-asset-playground.md:153, 193-200`).
- Record live matches? (`:81-82`). Per the overhaul, "explosions batches B/C and the destruction-v2 LOD/scar batches stay parked" (`D\21-overhaul-2026-09.md:22-23`).

## Things I could not confirm

1. Whether `Book.Core`/`Head`/`Bloom` → FireHead/FireBloom/FireCore (§2.3) is intentional. It looks like an ordinal mismatch; I did not check it in Play.
2. The working directory a relative `out=` resolves against. The code uses `Path.GetFullPath` (`PerfBench.cs:651`); I assume the project folder in the editor and the launch folder in the player, but did not verify either.
3. That `tw stepabs`, `tw stepto` and the timed-effects recipe still work. `workflow.md:250` says they were last verified 2026-09-25 and "not re-run here". I only confirmed the subcommands exist (`T\tw:55,83,101,108`).
4. Whether the gate's EditMode run includes `TW.Tests.Playground` (an Editor-only asmdef). `gate.ps1` has no Playground filter, so it probably does, but I did not verify.
5. An eval-only way to queue an arbitrary `Impact` (`h.WriteWorlds(m => m.Blast.Queue(...))`). The API exists (`MatchSim.cs:34`, `SimHost.cs:174`); I did not run it, and no helper wraps it.
6. The PropWear behaviour in Play ("Seen in Play: not yet", `D\16-destruction.md:344-346`) and the three-minute-barrage before/after bench for the lining ("Not yet", `:768-769`). Both are documented as not done.
7. That the Blender scripts other than housesplit, tank3split, jeepsplit, mechsplit and crabsplit run under 5.0. `pipelines.md` states it only generally.
