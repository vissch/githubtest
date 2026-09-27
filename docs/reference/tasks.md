# Where things are: task → files → tests → how to see it

Read this second, after `CLAUDE.md`. Find the area your task touches, open the files it names, run the tests it
names, and look at the result the way "See it" says. Paths are under `trench-warfare-3d/Assets/_Project/`, except
`Tools/...` (under `trench-warfare-3d/`) and `docs/...` (repo root). A test class lives in the file of the same name
in `Tests/EditMode/` or `Tests/PlayMode/`. Two lists at the bottom are generated from the code by `Tools/codemap.py`:
which component sets and reads each `SceneHooks` member, and every test class by mode.

A row that says **Tests: none** means nothing will go red if you break it. Look at it in Play.

## Worked examples

- **"Make the walkers leave deeper footprints."** Ground marks and footfalls, below. Three knobs: how dark and
  pressed-in the print looks is `_Shape` 2 in `Shaders/GroundMark_URP.shader`; its size is the pad passed to
  `SceneHooks.FootFall` in `TankRenderer.cs`; how long it lies is the `FootFall` lambda in `CombatFx.cs` `Start`
  (110 s mud, 300 s snow), which calls `AddMark` in `CombatFx.Ground.cs`. No test covers it. See it: spawn a
  walker (archetypes 6-11) and shoot it top-down, recipe in `workflow.md` section 6.
- **"Add a new unit type."** Adding a unit type, below: a checklist across sim, HUD, art and the capture tool,
  and the slot count is a seam change.
- **"An EditMode test fails."** Search this page for the test class name: the row that names it under **Tests**
  is its area, and that row's files are the code it guards. Before believing a red from an editor that has been in Play, read
  `workflow.md`, "False reds". A real red in the other lane's code is theirs to fix: write a note in `docs/inbox/`
  with the test name and the failure message, and do not patch their files.
- **"The game looks wrong after my change."** `workflow.md`, "See the game": capture with `CaptureRig`, then
  read the numbers with `python Tools/shotstats.py`. Do not judge brightness or colour by eye.

## Sim (SIM lane: `Sim/**`, `Net/**`, `Data/**`)

### Sim core: world state, commands, hash, replay
- **Files:** `Sim/Core/SimWorld.cs` (all per-slot arrays, `Hash()`), `Sim/Core/SimCommand.cs`, `Sim/Core/SimEvents.cs`,
  `Sim/Core/SimConfig.cs`, `Sim/Core/Replay.cs` (`FormatVersion`), `Sim/Match/MatchSim.cs` (system registration),
  `Sim/Core/ISimSystem.cs` (order constants). System order table: `code-map.md`. Helpers: `Sim/Core/SimHash.cs`
  (FNV-1a), `Sim/Core/SimRandom.cs` (every random draw from seed, tick, system and slot), `Sim/Core/SimMath.cs`
  (platform-independent maths), `Sim/Core/UnitPose.cs` (the pose stream to rendering), `Sim/Core/PerfMarkers.cs`
  (profiler markers), `Sim/Match/TerrainHashSystem.cs` (puts the ground in the hash). The cross-platform gate:
  `Editor/DeterminismPlatformReport.cs`.
- **Tests:** SimHashTests, DeterminismReplayTests, CommandValidationTests, HashIntervalTests.
- **Trap:** `Hash()` is an ordered chain. Append, never insert. A new array must be hashed and must bump
  `FormatVersion`. Contracts in `docs/02-contracts.md`, rules in `docs/03-determinism-rules.md`.

### Lockstep, one world, canary
- **Files:** `Net/LockstepDriver.cs`, `Net/LoopbackTransport.cs`, `Net/CommandSeat.cs` (SIM lane), and on the
  SHOW side of the seam `Presentation/Core/LockstepSession.cs`, `Presentation/Core/SimHost.cs`,
  `Presentation/Core/ScriptedEnemy.cs` (the AI). The folder decides the lane: those three are SHOW files.
  Interfaces `Net/ILockstepTransport.cs`, `Net/ICommandSink.cs`; `Net/UtpTransport.cs` is a stub (multiplayer is
  deferred).
- **Tests:** LockstepLoopbackTests, BattlefieldLockstepTests, CommandSeatTests, SinglePlayerEquivalenceTests,
  CanaryFixture (makes every PlayMode SimHost run the canary).
- **Trap:** single player runs ONE world, so `SimHost.Peer` is null in Play. Write through
  `SimHost.WriteWorlds(...)`, never into `Local.World` by hand, or the canary desyncs.

### The stress preset (thousands of men for perf work)
- **Files (SHOW):** `Presentation/Core/SimHost.cs` (`StressUnits`, `StressOverride`), `Presentation/Core/ScriptedEnemy.cs`
  (`StressSide` deploys both armies at their spawn points), `Perf/BenchOptions.cs` (`stress=`),
  `Editor/CaptureRig.cs` (`Bench`, and `Stress`: a rough 2,000-man footprint check).
- **Tests:** SinglePlayerEquivalenceTests (runs the preset with 60 men a side), BattlefieldLockstepTests.
- **See it:** numbers you can compare: `TW.Editor.CaptureRig.Bench("stress=1000 settle_ticks=1800 ticks=400
  quality=5 out=C:/abs/a.json")`, or `-twbench "..."` in a build, then `python Tools/perfcmp.py` (workflow section 7).
  `CaptureRig.Stress(1000, path)` runs 120 real seconds with no fixed tick or hash: never the same fight twice.

### Map generation and ground (sim side)
- **Files:** `Sim/Terrain/BattlefieldGenerator.cs` (`BattlefieldParams` presets: `ShelledForest`, `WinterLine`, `Landing`),
  `Sim/Terrain/MapData.cs`, `Sim/Terrain/CraterStamp.cs`, `Sim/Terrain/Heightfield.cs`, `Sim/Terrain/WireBelt.cs`,
  `Sim/Terrain/MudField.cs`, `Sim/Terrain/PropDef.cs`, `Sim/Match/Deformation.cs` (the only thing that edits the map).
  Contracts `Sim/Terrain/MapStructs.cs`, `Sim/Terrain/NavLayer.cs`; `Sim/Terrain/GreyboxMapGenerator.cs` is the
  flat corridor the determinism tests use.
- **Tests:** BattlefieldTests, DynamicGroundTests, CoastTests, WinterMapTests.
- **Trap:** a feature tested only on the playtest map is untested. BattlefieldTests runs the generated map.

### Movement, flow fields, garrison, trench orders
- **Files:** `Sim/Nav/FlowFieldManager.cs`, `Sim/Nav/FlowField.cs`, `Sim/Nav/MovementSystem.cs` (`MoveJob` decides
  stance), `Sim/Nav/SeparationJob.cs`, `Sim/Units/TrenchGarrison.cs` (class `TrenchGarrisonSystem`),
  `Sim/Units/TrenchOrders.cs`, `Sim/Core/TrenchPost.cs`, `Sim/Core/StanceRules.cs`,
  `Sim/Nav/SpatialHash.cs` (1 m buckets, built in slot order so neighbour order is deterministic).
- **Tests:** FlowFieldTests, FlowFieldManagerTests, GarrisonAndOrdersTests, GarrisonTests, TrenchSpreadTests,
  PlaytestMapTests.
- **Trap:** `StanceSystem` is a stub. Stance is written in `MovementSystem.cs` as `StanceOf[i]`.

### Infantry combat
- **Files:** `Sim/Combat/TargetAcquisition.cs`, `Sim/Combat/DirectFire.cs`, `Sim/Combat/Suppression.cs`,
  `Sim/Combat/CombatTables.cs` (placeholder weapon data), `Sim/Combat/HeightfieldRaycast.cs` (line of sight).
  Data structs `Sim/Combat/CombatStructs.cs`, `Sim/Units/UnitStats.cs`, baked from ScriptableObjects by
  `Data/DataBaker.cs` (the assets themselves are made by `Editor/SliceDefinitions.cs`).
- **Tests:** CombatTests, HeightfieldRaycastTests.

### Shells, barrages, gas
- **Files:** `Sim/Combat/Blast.cs` (`BlastRules`: trench bay 0.7, traverse 0.5), `Sim/Match/OffMapAbilities.cs`,
  `Sim/Match/AmbientBombardment.cs`, `Sim/Combat/GasSmokeField.cs`, `Sim/Match/Deformation.cs`.
- **Tests:** SupportAbilityTests, DirectionalBlastTests.
- **Trap:** a trench never caves in, by owner decision (`decisions.md`).

### Objectives, money, victory, the debrief
- **Files:** `Sim/Match/SectorControl.cs` (an objective flips when enough infantry hold it; sets `WinnerTeam` when a
  side holds them all), `Sim/Core/SimWorld.cs` (`Silver` income each tick, deploy cost, `CommandType.Surrender`),
  and on the SHOW side `UI/ObjectiveTracker.cs` (the objectives list and the centre banner),
  `Presentation/Core/MatchStats.cs` (what the debrief counts, from the event pump), `UI/Shell/DebriefScreen.cs`.
- **Tests:** BattlefieldLockstepTests, CombatTests and PlaytestMapTests reach `WinnerTeam`; ShellUxmlTests loads the
  debrief. Nothing tests `SectorControl`, `MatchStats` or `ObjectiveTracker` directly.

### Stubs: placeholders for planned phases, not wired
`Sim/Combat/IndirectFire.cs`, `Sim/Match/Logistics.cs`, `Sim/Match/MissionScript.cs`, `Sim/Match/WaveAi.cs`,
`Sim/Units/Grenades.cs`, `Sim/Units/SpecialAbilities.cs` (unregistered systems that throw, `code-map.md`),
`Net/HashExchange.cs`, `Net/Snapshot.cs`, `Presentation/Audio/EventAudioRouter.cs`, `Presentation/VFX/EventVfxRouter.cs`
(each its assembly's only file). Building one out is a feature, and for the sim ones a hash change.

### Fire in the sim
- **Files:** vehicles burn: `Sim/Units/VehicleModules.cs` (`Fire`, `StartFire`, `UnitFlags.Burning`, the
  `VehicleOnFire` event). Men and ground cells do not burn yet: `Sim/Combat/Burning.cs` is a stub that throws and is
  not registered (`code-map.md`, sim system order) (until "SIM: a recruit lit before the burning system steps"
  lands). Registering it is a hash and replay change (a seam commit).
- **Tests:** TankMobilityTests (vehicle fire). None for infantry fire.

### Vehicles in the sim: tanks and walkers
- **Files:** `Sim/Core/RosterEntry.cs` (`VehicleArchetype`, default roster, `SlotCount`), `Sim/Combat/TankSpec.cs`,
  `Sim/Combat/Armor.cs`, `Sim/Combat/TankGunnery.cs`, `Sim/Units/VehicleModules.cs` (legs, tracks, crew, fire,
  when a vehicle is destroyed), `Sim/Nav/VehicleKinematics.cs` (`VehicleSize`, `VehicleProfile`, trench crossing, crushing).
  The wreck itself is a map prop made in `Sim/Match/Deformation.cs` (`Sim/Terrain/PropDef.cs`, `MapData.AddProp`);
  which vehicle it was survives only in the `PropChanged` event (`dir.x` = dead slot + 1), not on the prop.
- **Tests:** TankTests, TankMobilityTests, CrabTests.
- **Trap:** `VehicleSize` is baked into mesh vertices at load. Every gap authored in the composer depends on it.

### Sea landing
- **Files:** `Sim/Match/SeaLanding.cs`, `Sim/Core/ISeaLift.cs`, drawn by `Presentation/Terrain/LandingCraftView.cs`,
  `Presentation/Terrain/Ocean.cs`, `Presentation/Terrain/Shore.cs`, `Shaders/Sea_URP.shader`.
- **Tests:** LandingTests, CoastTests.

### Adding a unit type (checklist)
This task spans both lanes. The SIM lane lands steps 1-2 as a seam commit first; the SHOW lane then does 3-6.
1. **Both rosters are full** (`RosterEntry.FillDefault` fills slots 0-7 for each side), so a new unit means
   replacing one or raising `RosterEntry.SlotCount`, a seam change. `lane/show/units-meta` already has 8 → 10 in
   flight (`docs/inbox/`): coordinate before starting.
2. Sim: a new archetype id in `VehicleArchetype` (`Sim/Core/RosterEntry.cs`; ids are a seam item), its roster
   entry, and for a vehicle a `TankSpec` and `VehicleProfile`. `IsWalker` is a range check (`Pincer` to `Redoubt`,
   6-11), so a walker with id 12 is silently not a walker until the range moves.
3. HUD: name, tooltip and icon in `UI/HudText.cs` and the unit art in `UI/UnitArt.cs`. HudTextTests,
   HudBindTests and UnitArtTests fail until every archetype has them.
4. Art: infantry needs a figure in `Editor/VATBaker.cs` and a bake. Vehicles need a `Resources/Vehicles/<Name>/`
   folder (`pipelines.md`) that `Presentation/Camera/TankModel.cs` loads; a walker's legs are solved by
   `Presentation/Camera/WalkerGait.cs` from the model, so check it stands and walks (GaitTests).
5. The legacy IMGUI `Presentation/Camera/BattleHud.cs` has fixed-size arrays and its own name switch. No test
   runs OnGUI, so check it in Play with F9.
6. Tests to run: CrabTests or TankTests, GaitTests, HudTextTests, HudBindTests, UnitArtTests, then the full gate.
   `TankCapture.Spawn` finds the new archetype in the live roster by itself.

## Presentation (SHOW lane)

### Infantry rendering (VAT)
- **Files:** `Presentation/Units/VATRenderer.cs` (LOD tiers; the figure scale is `UnitScale` in
  `Presentation/Core/FigureMetrics.cs`, shared with the picker), `Presentation/Core/IZoomSource.cs`, `Presentation/Units/VatCodec.cs`,
  `Presentation/Units/VatAssetData.cs`, `Presentation/Units/ProceduralSoldier.cs` (far tier and fallback),
  `Shaders/VAT_URP.shader`, bake: `Editor/VATBaker.cs` + `Editor/InfantryClipTable.cs` (menu TW/VAT/Bake Infantry).
- **Tests:** VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests.
- **Trap:** any clip/discard in `VAT_URP.shader` goes behind `_TW_LIMBCUT` (VatEarlyZTests). Index instance data
  with `GetIndirectInstanceID_Base`, not `GetIndirectInstanceID`.

### Animation (which clip a man plays)
- **Files:** `Presentation/Core/AnimationController.cs` (the per-man priority ladder, run every tick by SimHost),
  clip names in `Editor/InfantryClipTable.cs`. Design: `docs/15-character-controller.md`.
- **Tests:** BlastReactionTests (knockdown and daze), TickAllocationTests (no per-tick allocation).
- **See it:** `Animation.Follow(slot)` then `Animation.TraceText()` through eval, for one man's decisions.

### Tanks and walkers drawn
- **Files:** `Presentation/Camera/TankRenderer.cs`, `Presentation/Camera/TankModel.cs` (parts, sockets, leg rigs),
  `Presentation/Camera/WalkerGait.cs` (planted feet), `Shaders/Tank_URP.shader`, `Shaders/TankDisc_URP.shader`,
  import rules `Editor/TankImport.cs`.
- **Tests:** GaitTests (plus the sim tests above).
- **See it:** `TW.Editor.TankCapture.Spawn(team, archetype, x, z)`, then read `World.Position[slot]` back: the sim
  moves units to their deploy zone. Freeze with `SimHost.TimeScale = 0` before framing.
- **Trap:** vehicle meshes must import Read/Write enabled or scaling silently does nothing.
- **Already there, check before adding:** `WalkerGait` `Carry` sets the body's height, pitch and roll from the
  planted feet, and `Step` leads each step by the walker's velocity; `TankRenderer` applies them to the hull.

### Combat effects: tracers, bursts, smoke, camera shake
- **Files:** `Presentation/Camera/CombatFx.cs` (event dispatch `OnSimEvent`, tracers, bodies, bursts, materials),
  `Presentation/Camera/CombatFx.Chunks.cs` (thrown dirt, splinters, smoke balls, cook-offs),
  `Presentation/Camera/CombatFx.Bodies.cs` (gibs, tree breaks, muzzle and chest positions),
  `Presentation/Camera/CombatFx.Ambient.cs` (birds, ambient smoke), `Presentation/Camera/CameraShake.cs`,
  `Presentation/Camera/FlipbookFx.cs` + `Shaders/Flipbook_URP.shader` (painted flipbooks, textures in `Resources/VFX/`).
  `CombatFx` is one partial class: an event arrives in `CombatFx.cs` and is handed to the part that draws it.
  **Colours:** six books' tints (Splash, Column, Wings, Spurt, Puff, Smoke) are overwritten every scene by
  `CombatFx.ApplyTints` from
  `BiomeProfile` (`SmokeTint` and the other `*Tint` fields, through `SceneTints`), so change a colour there. At
  night a burst also lights its own smoke: `NightLights` sets `_TWBurst`, read by `TWBurstLight` in
  `Shaders/TWAtmosphere.hlsl`. `FlipbookFx.Book` maps to `Sheets` by position, and three
  sheets are named "Puff": count the rows, the smoke book is the one with `Erode = true`.
  `CombatFx.cs` also draws the gameplay overlays: called-strike target markers, the ability aiming circle (in
  `Update`, from `TestPanel.Armed`) and the IMGUI banner (`Banner`, `OnGUI`).
- **Tests:** BlastReactionTests (camera feels a burst), ComponentLookupAllocationTests.
- **Trap:** effects drawn only up close sit behind `if (!close) return;` in `CombatFx.Ground.cs` `CloseLife`. Keep
  that guard in front of anything close-only, or the standard view pays for it.

### Ground marks and footfalls
- **Files:** `Presentation/Camera/CombatFx.Ground.cs` (`AddMark`, `MaxMarks`, `MachineMarkReach`, the marks pool,
  `DrawClose`), `SceneHooks.FootFall` is wired in `CombatFx.cs` `Start`,
  `Shaders/GroundMark_URP.shader` (shape 0 boot, 1 rut, 2 walker pad; snow vs mud handled in the shader),
  `Presentation/Camera/TankRenderer.cs` (calls `FootFall` where a walker's foot lands).
- **Tests:** none.
- **See it:** a top-down capture in Play. A paused editor draws no marks, so do not diff paused frames.

### Flamethrower
- **Files:** `Presentation/Camera/Flamethrower.cs` (presentation only: men do not burn in the sim, see "Fire in
  the sim"), `Shaders/Flame_URP.shader`, books cut by `Tools/firebooks.py`. Its clock is `now = Time.time` in
  `CombatFx.cs` `Update`, passed to `flames.Update`: wall-clock, so it runs on while the sim is paused
  (until "Pools of fuel and pyres burn out by the sim's clock" lands). The torch drawn on a burning man is
  `Flamethrower` `torches`, also wall-clock; only his animation's `AlightUntil` runs on sim ticks. The `Death` case in
  `CombatFx.cs` does not call `flames.Douse`, so a dead man's torch burns on and the slot's next tenant can be drawn
  alight (until "Critique round 1: the fixes on the show lane" lands).
- **Tests:** none.
- **See it:** `Tools/flameshots <prefix>` captures the four reference shots deterministically, and
  `Tools/flamecheck.py` rejects a shot with no fire in it.
- **Trap:** `FlipbookFx` indexes its sheets by the `Book` enum's ordinal and is only `Ready` when every sheet loads.
  A missing PNG or a row out of order silently disables every flipbook in the game. Add the enum entry, the row and
  the PNG together, and check `books.Ready`.

### Debris and destruction of props and houses
- **Files:** `Presentation/Camera/DebrisRenderer.cs` + `Shaders/Debris_URP.shader` (GPU-flown pieces, ring buffers),
  `Presentation/Terrain/PropDestruction.cs` and `Presentation/Terrain/PropWear.cs` (one partial class),
  `Presentation/Terrain/HouseKit.cs` (chunked buildings, `MaxChunks`, `ChunkMask`). Design: `docs/16-destruction.md`.
- **How harm works:** `PropDestruction.Strike` applies each explosion to the props in reach, per `Rule` (hp,
  pieces, dust; built in `BuildRules`, one per kit list such as `kit.TrenchWalls`). A prop with hp left is only
  chipped (`Chip`: flying bits, no change of look); at zero it is `Finish`ed (`Collapse`/removed, debris thrown).
  A shelter (`Rule.Shelter`: dugouts, bunkers, MG nests) is never finished: `ShedBags` throws sandbags off it. There
  is no damaged-but-standing state. Budgets: `MaxLoose`, `MaxRemembered`, `PropDestruction.SpallBudget` (declared in
  `PropWear.cs`, a part of the same class), and the `DebrisRenderer` rings.
- **Tests:** DebrisTests, PropWearTests, HouseKitTests.
- **Trap:** a prop is identified by its position rounded to 0.25 m. Convert a drawn instance to a key through
  `PropWear.Home` first, or it forgets its damage.

### Battlefield props and composition
- **Files:** `Presentation/Terrain/BattlefieldComposer.cs` (seeded placement), `Presentation/Terrain/BattlefieldKit.cs`
  (modules, env atlas cells), `Presentation/Terrain/BattlefieldProps.cs` (instanced pages, `Generation`),
  `Presentation/Terrain/BattlefieldBlueprint.cs`, `Presentation/Terrain/PropLayout.cs` + `Resources/Layouts/`
  (owner's hand edits), `Editor/EnvPropEditor.cs` (with `Presentation/Terrain/PropHandle.cs`, the editor stand-in for
  one batched prop), `Editor/EnvKitImport.cs`. The procedural kit: `Presentation/Terrain/BattlefieldGeometry.cs` (worn
  solid primitives), `Presentation/Terrain/BattlefieldPigment.cs` (the painted surface sheet), and
  `Presentation/Terrain/BattlefieldBackdrop.cs` (what lies beyond the fought-over ground).
- **Tests:** EnvAtlasTests.
- **Trap:** `BattlefieldKit.EnvSets` / `EnvCols` / `EnvRows` must match `Tools/envatlas.py`. Walkers need ~10 m
  gaps between placed structures.

### Terrain view, weather, night, biomes
- **Files:** `Presentation/Terrain/GreyboxTerrainView.cs` (ground mesh, adds most environment components; owns the
  crater colour texture `colorTex` and uploads it in `Update` whenever a crater painted it),
  `Presentation/Terrain/BattlefieldSurface.cs` (surface kinds; `RefreshHollows` finds where water pools in craters,
  about 11 ms, whole-map and order-dependent, marker `TW.Terrain.Hollows`, run under `HeavyWork`; no test covers it),
  `Presentation/Terrain/Atmosphere.cs`, `Presentation/Terrain/NightLights.cs`,
  `Presentation/Terrain/Rain.cs`, `Presentation/Terrain/Storm.cs`, `Presentation/Terrain/QuietFog.cs`,
  `Presentation/Terrain/FogWisps.cs`, `Presentation/Terrain/SmallLife.cs`, `Presentation/Terrain/WaterRings.cs`,
  `Presentation/Terrain/BiomeProfile.cs`, `Presentation/Core/RenderGround.cs` (shared ground height, `SceneTints`),
  shaders `Toon_URP`, `Water_URP`, `TWAtmosphere.hlsl`, `TWLocalLights.hlsl`, `TWWater.hlsl`.
- **Tests:** BiomeProfileTests, PaintedHorizonCompressionTests, WinterLevelTests.
- **Trap:** post-processing only runs because `Settings/TW-Renderer.asset` references URP's `PostProcessData`; with
  it null the whole grade silently does nothing while the volume stack still reports its values.
- **Trap:** `SimHost.Ground` picks the terrain but not the look. For a winter test set the biome look too
  (`feature-flags.md`). `BiomeProfile.ForGround` pairs them: `WinterLine` is the only daylight look (overcast snow);
  every other ground is night.

### Render pipeline and quality settings
- **Files:** `Settings/TW-URP.asset` is the pipeline in effect at every quality level (shadow distance 220 m,
  cascades), with `Settings/TW-Renderer.asset`. `ProjectSettings/QualitySettings.asset` also lists shadow
  distances (15 to 150): URP ignores them. `Editor/BootstrapSceneBuilder.cs` rebuilds the pipeline asset with its
  own copy of 220. No runtime code sets pipeline values today; a runtime knob belongs in `Atmosphere.cs`, which owns
  the per-scene look. `Editor/InkLinesSetup.cs` installs the screen-space ink pass on the renderer.
- **Trap:** changing a pipeline asset's value from code in the editor writes the asset to disk. Restore it, or
  change a copy.
- **Tests:** none.

### Camera
- **Files:** `Presentation/Camera/TacticalCamera.cs` (standard view: fov 25, pitch 25, zoom 30; `FrameFrom`),
  `Presentation/Camera/CameraShake.cs`, `Editor/GameViewFit.cs`.
- **Trap:** setting `Camera.main.transform` does nothing; the controller overwrites it every frame. Use `FrameFrom`.

## Interface (SHOW lane)

### Battle HUD (UI Toolkit, the live one)
- **Files:** `UI/HudController.cs`, `UI/HudView.cs`, `UI/HudText.cs` (every word), `UI/HudLayout.cs`,
  `UI/HudMinimap.cs`, `UI/TrenchOrderCluster.cs`, `UI/HudBootstrap.cs`, `UI/Resources/Hud/BattleHud.uxml`,
  `UI/HudHotkeys.cs` (keys, through `KeyMap`), `UI/HudTooltip.cs`, `UI/HudDialogue.cs` (the speaker strip) and
  `UI/HudCommentary.cs` (what is said on it), `UI/ObjectiveTracker.cs`; `Presentation/Core/HudBridge.cs` is the
  seam between the camera assembly and UI (the camera may not reference UI).
- **Tests:** HudBindTests, HudStructureTests, HudLayoutPlayTests (PlayMode: the bar fits, `HudLayout.BarWidth`),
  UnitArtTests. `HudText` strings are tested in HudBindTests. HudTextTests checks that the unit names and tooltips
  match the sim's numbers. HudLayoutTests, despite its name, tests the legacy `BattleHud`.
- **Centre banner** (a trench taken, an incoming barrage, the match end): `UI/ObjectiveTracker.cs` `OnEvent` picks
  it, the words are in `HudText` (`CapturedBanner`, `IncomingBanner`, ...). `CombatFx.cs` still draws its own IMGUI
  copy of the same banners (`Banner`, `OnGUI`).
- **See it:** `TW.Editor.HudCapture.Shoot(path)`. The ordinary capture paths do not include the HUD.
- **Trap:** the support cards are `HudView.SupportAbilities` (HE barrage and chlorine only), but `HudLayout.SupportSlots` and `BattleHud` each keep
  their own `SupportSlots = 2` (until "The HUD's width model counts every support card" lands).

### Legacy IMGUI HUD and debug panel
- **Files:** `Presentation/Camera/BattleHud.cs` (F9 switches to it), `Presentation/Camera/TestPanel.cs`,
  `Presentation/Camera/DebugOverlay.cs`.
- **Tests:** HudLayoutTests, HudTextTests test its static helpers only. No test runs OnGUI.
- **See it:** in Play press F9 for the legacy HUD; the "Debug panel" button is top right. `Tools/tw shot` captures
  the game view with its IMGUI.

### Support fire: arming, aiming, calling it in
- **Files:** arming an ability is checked in three places that must agree: `TestPanel.Arm` / `Armed` (the debug
  panel, and the one the aiming circle reads), `UI/HudController.cs` `ToggleArm`, legacy `BattleHud.SupportSlot`.
  The range readout is `UI/Selection/AimReadout.cs` (`Radii`: hit and reach), the cursor `UI/Selection/SelectCursor.cs`,
  the circle and target markers `Presentation/Camera/CombatFx.cs` `Update`. Sim side (SIM lane):
  `Sim/Match/OffMapAbilities.cs` (`TryGetStats`: which abilities exist and their radius).
- **Tests:** SelectionTests (AimReadout), SupportAbilityTests (sim).
- **Trap:** the drawn circle uses the ability's radius, or 8 m when it has none; `AimReadout` has its own 8 m
  (`GasReticleM`). Change both (until "One reticle radius for the aim and the readout" lands).
- **See it:** in Play, arm with the HUD card or keys 9 and 0, then `TW.Editor.HudCapture.Shoot(path)`.
- **Adding a support ability (checklist).** SIM first: `OffMapAbilityId` and its stats in
  `Sim/Match/OffMapAbilities.cs`, the asset in `Editor/SliceDefinitions.cs`. Then SHOW: `UI/HudView.cs`
  `SupportAbilities`; the slot counts in `UI/HudLayout.cs` and `Presentation/Camera/BattleHud.cs`; a `GameAction`,
  key and label in `Presentation/Core/KeyMap.cs`; `UI/HudHotkeys.cs`; name and card text in `UI/HudText.cs`; its icon
  in `UI/Skin/SkinSpec.cs`; `Presentation/Camera/TestPanel.cs`; the aim circle and effects in `CombatFx.cs`; the AI's
  choice in `Presentation/Core/ScriptedEnemy.cs`. Tests: SupportAbilityTests, HudBindTests, HudStructureTests,
  KeyMapTests, SkinAssetTests.

### Selection
- **Files:** `UI/Selection/SelectionController.cs`, `UI/Selection/SelectionModel.cs`, `UI/Selection/UnitPicker.cs`
  (pick radius), `UI/Selection/SelectionMarkers.cs`, `UI/Selection/HoverCard.cs`, `UI/Selection/SelectionPanel.cs`,
  `UI/Selection/UnitStatus.cs`, `UI/Selection/GarrisonStats.cs`; aiming support fire is its own row above.
- **Tests:** SelectionTests.

### Menus, settings, keys, match launch
- **Files:** `UI/Shell/ShellRouter.cs`, `UI/Shell/ShellBoot.cs` (keeps the shell alive through scene loads),
  `UI/Shell/ShellScreen.cs` (one screen: a UXML bound to the router), `UI/Shell/ShellAssets.cs` (built by
  `Editor/UI/ShellAssetsBuilder.cs`), the screens `UI/Shell/MainMenuScreen.cs`, `UI/Shell/MissionSelectScreen.cs`
  (with `UI/Shell/MissionCatalog.cs`, `UI/Shell/MapThumbnail.cs`), `UI/Shell/ArmouryScreen.cs`,
  `UI/Shell/SettingsScreen.cs`, `UI/Shell/PauseMenuScreen.cs`, `UI/Shell/DebriefScreen.cs`; `UI/Shell/SettingsApplier.cs`,
  `Presentation/Core/AudioLevels.cs` (volume buses), `Presentation/Core/InputFocus.cs` (who owns the keyboard),
  `Presentation/Core/GameSettings.cs`, `Presentation/Core/SettingsStore.cs`, `Presentation/Core/KeyMap.cs`,
  `Presentation/Core/MatchLaunch.cs` (the `Request` a mission starts from; `UI/Shell/MissionCard.cs` `ToRequest` is
  the one place the game builds one), `Presentation/Core/MatchClock.cs` (owns `SimHost.TimeScale`). Factions and
  unlocks are data in `Data/Definitions.cs` (SIM lane), not yet read at runtime.
- **Tests:** ShellUxmlTests, ShellRouterPlayTests, GameSettingsTests, KeyMapTests, MatchClockTests, MatchLaunchPlayTests.
- **Settings sliders:** each slider's range is a row in `GameSettings.Sliders`; loading clamps to it and
  `SettingsScreen` sets the slider from it. A new slider needs a row, or GameSettingsTests fails.

### UI skin
- **Files:** `UI/Skin/SkinSpec.cs` (the sprite table), `Editor/UI/UiSkinGenerator.cs`, `Editor/UI/UiSkinImport.cs`,
  `Editor/UI/UiSkinVerifier.cs`, `Editor/UI/UiAssetBuilder.cs`. `docs/17-ui-art-spec.md` is generated by `Tools/gen_artspec.py`.
- **Tests:** SkinAssetTests.
- **Trap:** a USS imported before the PNG or font it names keeps "Invalid asset path" warnings until
  reimported (menu TW/UI/Reimport Skin Sheets). `-unity-slice-scale` needs a unit (`1px`). Unit portraits are USS
  classes `.tw-portrait-<Name>`, not Resources.

## Tooling

### Statics that outlive a match
- **Files:** `Presentation/Core/SceneStatics.cs` (`Reset` per scene load; `ResetSession` and `Register` when Play
  ends), `Editor/PlayModeStaticsReset.cs` (calls it on EnteredEditMode), `SceneHooks.Reset` in `Presentation/Core/RenderGround.cs`.
- **Tests:** StaticLifecycleTests (a new mutable static in presentation or UI fails until it registers a reset or
  is explained there), SceneStaticsTests (what a scene load keeps).
- **Trap:** never clear `SceneHooks` or `HudBridge`'s `PointerOverUi` / `WheelClaimed` from `SceneStatics.Reset`:
  it runs after the new scene has wired its hooks, and `HudBootstrap` may already have built the HUD.

### Performance and allocations
- **Files:** `Perf/PerfBench.cs`, `Perf/BenchOptions.cs` (every bench option is parsed in `Parse`: the list of
  keys), `Perf/AllocProbe.cs`, `Presentation/Core/HeavyWork.cs`, `FrameBudget` in
  `Presentation/Core/RenderGround.cs`. Budgets and past runs: `docs/05-performance-budgets.md`.
- **Tests:** AllocProbeSanityTests, TickAllocationTests, ComponentLookupAllocationTests, VatAtlasMemoryTests. None
  covers `FrameBudget`'s counts or `BenchOptions.Parse` (an unknown key is ignored without a word).
- **Trap:** the `shot=` still is taken when the warm-up counter passes 10 (`PerfBench` `Warm`), so a `warm=` under
  10 takes no still.
- **Trap:** `GC.GetAllocatedBytesForCurrentThread` reads 0 in Unity. Count allocations with `AllocProbe`.
- **Trap:** `FrameBudget` does not see every draw: `BattlefieldProps`, `PropDestruction` and `SelectionMarkers` call
  `Graphics.RenderMeshInstanced` directly (until "Every gameplay draw goes through FrameBudget" lands), and `PerfBench` reports the props'
  own `DrawCalls` rather than reading `FrameBudget`. A bench `shot=` still includes the HUD and IMGUI overlays.

### Windows build
- **Files:** `Editor/BuildWindows.cs`, `Resources/ShaderKeep/`.
- **Tests:** ShaderInclusionTests (every `Shader.Find("TW/...")` must be in Always Included Shaders or used by a
  material under `Resources/ShaderKeep/`, or it breaks the player).

## Generated indexes (do not edit: `python Tools/codemap.py`)

Which component sets, reads or calls each `SceneHooks` member (the hand rows above no longer repeat this):

<!-- gen:hooks -->
| SceneHooks member | Set by | Read / called by |
|---|---|---|
| `CloseUp` | CaptureRig, PerfBench, TacticalCamera | Atmosphere, BattlefieldProps, CombatFx, CombatFx.Chunks, CombatFx.Ground, NightLights, PropDestruction, SmallLife |
| `IsWater` | WaterRings | CombatFx, CombatFx.Ambient, CombatFx.Chunks, CombatFx.Ground, NightLights, TankRenderer |
| `AddRing` | WaterRings | CombatFx, CombatFx.Chunks, LandingCraftView, TankRenderer |
| `Sparks` | CombatFx | Flamethrower, NightLights, TankRenderer |
| `SmokeSources` | NightLights | CombatFx.Ambient |
| `TanksDrawn` | TankRenderer | CombatFx, CombatFx.Ground, VATRenderer |
| `VehicleTracks` | TankRenderer | CombatFx.Ground |
| `VehicleGunPort` | TankRenderer | CombatFx.Bodies |
| `DrawnWreck` | TankRenderer | BattlefieldComposer |
| `IsTankSlot` | TankRenderer | CombatFx, CombatFx.Ground |
| `Flash` | NightLights | CombatFx.Chunks, TankRenderer |
| `FireLight` | NightLights | Flamethrower |
| `CookOff` | CombatFx | PropDestruction |
| `FootFall` | CombatFx | TankRenderer |
<!-- /gen:hooks -->

<!-- gen:tests -->
- **EditMode:** AllocProbeSanityTests, BattlefieldLockstepTests, BattlefieldTests, BiomeProfileTests, BlastReactionTests, CoastTests, CombatTests, CommandSeatTests, CommandValidationTests, ComponentLookupAllocationTests, CrabTests, DebrisTests, DeterminismReplayTests, DirectionalBlastTests, DynamicGroundTests, EnvAtlasTests, FlowFieldManagerTests, FlowFieldTests, GaitTests, GameSettingsTests, GarrisonAndOrdersTests, GarrisonTests, HashIntervalTests, HeightfieldRaycastTests, HouseKitTests, HudBindTests, HudLayoutTests, HudStructureTests, HudTextTests, KeyMapTests, LandingTests, PaintedHorizonCompressionTests, PlaytestMapTests, PropWearTests, SceneStaticsTests, SelectionTests, ShaderInclusionTests, ShellUxmlTests, SimHashTests, SinglePlayerEquivalenceTests, SkinAssetTests, StaticLifecycleTests, SupportAbilityTests, TankMobilityTests, TankTests, TickAllocationTests, TrenchSpreadTests, UnitArtTests, VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, WinterLevelTests, WinterMapTests
- **PlayMode:** HudLayoutPlayTests, LockstepLoopbackTests, MatchClockTests, MatchLaunchPlayTests, ShellRouterPlayTests
<!-- /gen:tests -->
