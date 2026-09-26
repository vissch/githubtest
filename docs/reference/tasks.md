# Where things are: task → files → tests → how to see it

Read this second, after `CLAUDE.md`. Find the area your task touches, open the files it names, run the tests it
names, and look at the result the way "See it" says. Paths are under `trench-warfare-3d/Assets/_Project/`, except
`Tools/...` (under `trench-warfare-3d/`) and `docs/...` (repo root). A test class lives in the file of the same name
in `Tests/EditMode/` or `Tests/PlayMode/` (the Mode column below says which). The two tables at the bottom are generated from the code by `Tools/codemap.py`:
which component sets and reads each `SceneHooks` member, and which production types each test class touches.

A row that says **Tests: none** means nothing will go red if you break it. Look at it in Play.

## Worked examples

- **"Make the walkers leave deeper footprints."** Ground marks and footfalls, below. Three knobs: how dark and
  pressed-in the print looks is `_Shape` 2 in `Shaders/GroundMark_URP.shader`; its size is the pad passed to
  `SceneHooks.FootFall` in `TankRenderer.cs`; how long it lies is the `FootFall` lambda in `CombatFx.cs` `Start`
  (110 s mud, 300 s snow), which calls `AddMark` in `CombatFx.Ground.cs`. No test covers it. See it: spawn a
  walker (archetypes 6-11) and shoot it top-down, recipe in `workflow.md` section 6.
- **"Add a new unit type."** Adding a unit type, below: a checklist across sim, HUD, art and the capture tool,
  and the slot count is a seam change.
- **"An EditMode test fails."** Find the test class in the generated table at the bottom to see which code it
  exercises, then its area row. Before believing a red from an editor that has been in Play, read
  `workflow.md`, "False reds". A real red in the other lane's code is theirs to fix: write it in `inbox.md`
  with the test name and the failure message, and do not patch their files.
- **"The game looks wrong after my change."** `workflow.md`, "See the game": capture with `CaptureRig`, then
  read the numbers with `python Tools/shotstats.py`. Do not judge brightness or colour by eye.

## Sim (SIM lane: `Sim/**`, `Net/**`, `Data/**`)

### Sim core: world state, commands, hash, replay
- **Files:** `Sim/Core/SimWorld.cs` (all per-slot arrays, `Hash()`), `Sim/Core/SimCommand.cs`, `Sim/Core/SimEvents.cs`,
  `Sim/Core/SimConfig.cs`, `Sim/Core/Replay.cs` (`FormatVersion`), `Sim/Match/MatchSim.cs` (system registration),
  `Sim/Core/ISimSystem.cs` (order constants). System order table: `code-map.md`.
- **Tests:** SimHashTests, DeterminismReplayTests, CommandValidationTests, HashIntervalTests, DeathEventContractTests
  (what `Death.b`, `dir` and `scalar` mean).
- **Trap:** `Hash()` is an ordered chain. Append, never insert. A new array must be hashed and must bump
  `FormatVersion`. Contracts in `docs/02-contracts.md`, rules in `docs/03-determinism-rules.md`.

### Lockstep, one world, canary
- **Files:** `Net/LockstepDriver.cs`, `Net/LoopbackTransport.cs`, `Net/CommandSeat.cs`,
  `Presentation/Core/LockstepSession.cs`, `Presentation/Core/SimHost.cs`, `Presentation/Core/ScriptedEnemy.cs` (the AI).
- **Tests:** LockstepLoopbackTests, BattlefieldLockstepTests, CommandSeatTests, SinglePlayerEquivalenceTests,
  CanaryFixture (makes every PlayMode SimHost run the canary).
- **Trap:** single player runs ONE world, so `SimHost.Peer` is null in Play. Write through
  `SimHost.WriteWorlds(...)`, never into `Local.World` by hand, or the canary desyncs.

### Map generation and ground (sim side)
- **Files:** `Sim/Terrain/BattlefieldGenerator.cs` (`BattlefieldParams` presets: ShelledForest, WinterLine, sea),
  `Sim/Terrain/MapData.cs`, `Sim/Terrain/CraterStamp.cs`, `Sim/Terrain/Heightfield.cs`, `Sim/Terrain/WireBelt.cs`,
  `Sim/Terrain/MudField.cs`, `Sim/Terrain/PropDef.cs`, `Sim/Match/Deformation.cs` (the only thing that edits the map).
- **Tests:** BattlefieldTests, DynamicGroundTests, CoastTests, WinterMapTests.
- **Trap:** a feature tested only on the playtest map is untested. BattlefieldTests runs the generated map.

### Movement, flow fields, garrison, trench orders
- **Files:** `Sim/Nav/FlowFieldManager.cs`, `Sim/Nav/FlowField.cs`, `Sim/Nav/MovementSystem.cs` (`MoveJob` decides
  stance), `Sim/Nav/SeparationJob.cs`, `Sim/Units/TrenchGarrison.cs` (class `TrenchGarrisonSystem`),
  `Sim/Units/TrenchOrders.cs`, `Sim/Core/TrenchPost.cs`, `Sim/Core/StanceRules.cs`.
- **Tests:** FlowFieldTests, FlowFieldManagerTests, GarrisonAndOrdersTests, GarrisonTests, TrenchSpreadTests,
  PlaytestMapTests.
- **Trap:** `StanceSystem` is a stub. Stance is written in `MovementSystem.cs` as `StanceOf[i]`.

### Infantry combat
- **Files:** `Sim/Combat/TargetAcquisition.cs`, `Sim/Combat/DirectFire.cs`, `Sim/Combat/Suppression.cs`,
  `Sim/Combat/CombatTables.cs` (placeholder weapon data), `Sim/Combat/HeightfieldRaycast.cs` (line of sight).
- **Tests:** CombatTests, HeightfieldRaycastTests.

### Shells, barrages, gas, fire
- **Files:** `Sim/Combat/Blast.cs` (`BlastRules`: trench bay 0.7, traverse 0.5; `BlastShape`; `Impact.SafeBehind`),
  `Sim/Match/OffMapAbilities.cs` (HE disc / line / box, creeping barrage, chlorine point / creeping, smoke screen,
  strafe run; `ScheduledPayload`, `PayloadKind`), `Sim/Core/AbilityArgs.cs` (heading, pattern, length in
  `SimCommand.B`), `Sim/Match/AmbientBombardment.cs`, `Sim/Combat/GasSmokeField.cs` (the gas field and the smoke
  field), `Sim/Combat/SmokeLos.cs` (metres of thick cloud on a line; read by `TargetAcquisition` and `DirectFire`),
  `Sim/Combat/Burning.cs` (`BurningSystem`: men and ground alight, reads `Blast.Resolved` for `BlastShape.Incendiary`),
  `Sim/Combat/BeamSystem.cs` (`BeamSystem`: the sweeping beam, started by the Beam ability; men, hulls, fire, the
  scorch of `BlastShape.Beam`), `Sim/Combat/Mines.cs` (`MineSystem`: mines and tripwires, `BlastShape.Mine`, laid by
  a system call until the sapper lands; a crater cooks them off), `Sim/Match/Deformation.cs`.
- **Tests:** SupportAbilityTests, DirectionalBlastTests, BurningSystemTests, AbilityArgsTests, StrafeRunTests,
  BarragePatternTests, SmokeScreenTests, BeamTests, MineTests.
- **Trap:** `MineSystem.Place` is a system call: no command lays a mine until the sapper (docs/21 SIM-D, units-meta),
  so the replay script fires none and `MineTests` carries the determinism check for the system.
- **Trap:** a line starts at `pos` and runs along the heading (0 = +Z, 90 = +X) for the length; `B = 0` is the plain
  ability at its own length, so every older caller still works. Add a pattern only to `AbilityStats.Patterns`, or the
  command is rejected as one the ability does not offer.
- **Trap:** a trench never caves in, by owner decision (`decisions.md`).

### Vehicles in the sim: tanks and walkers
- **Files:** `Sim/Core/RosterEntry.cs` (`VehicleArchetype`, default roster, `SlotCount`), `Sim/Combat/TankSpec.cs`,
  `Sim/Combat/Armor.cs`, `Sim/Combat/TankGunnery.cs`, `Sim/Units/VehicleModules.cs` (legs, tracks, crew, fire,
  wrecks), `Sim/Nav/VehicleKinematics.cs` (`VehicleSize`, `VehicleProfile`, trench crossing, crushing).
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
   flight (`inbox.md`): coordinate before starting.
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
- **Files:** `Presentation/Units/VATRenderer.cs` (`UnitScale`, LOD tiers, the living), `Presentation/Units/VATRenderer.Fallen.cs`
  (the dead: the throw arc, the tumble, the heap per 2 m cell, the charred), `Presentation/Units/VatCodec.cs`,
  `Presentation/Units/VatAssetData.cs`, `Presentation/Units/ProceduralSoldier.cs` (far tier and fallback),
  `Shaders/VAT_URP.shader`, bake: `Editor/VATBaker.cs` + `Editor/InfantryClipTable.cs` (menu TW/VAT/Bake Infantry).
- **Hooks:** reads `TanksDrawn`.
- **Tests:** VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, DeathVarietyTests (VatPad char bits, the tumble's pitch).
- **Trap:** any clip/discard in `VAT_URP.shader` goes behind `_TW_LIMBCUT` (VatEarlyZTests). Index instance data
  with `GetIndirectInstanceID_Base`, not `GetIndirectInstanceID`. `VatInstance.Tint` is the team in its low bit and
  a fallen man's tumble pitch above it (`team + 2 * step`, 32 steps a turn): the shader decodes `fmod(tint, 2)`, so
  never read `Tint` as the team on the C# side. `VatPad` packs limbs, grime, seed and char into 24 bits: nothing more fits.

### Animation (which clip a man plays)
- **Files:** `Presentation/Core/AnimationController.cs` (the per-man priority ladder, run every tick by SimHost),
  `Presentation/Core/AnimationController.Death.cs` (how a man dies: the Death event's cause and knock, density in the
  heap, the death ring `TryDeath` the effects read, `Char`), clip names in `Editor/InfantryClipTable.cs`.
  Design: `docs/15-character-controller.md` (section 9 is the death ladder).
- **Tests:** BlastReactionTests (knockdown and daze), DeathVarietyTests (the death ladder and its records),
  TickAllocationTests (no per-tick allocation).
- **See it:** `Animation.Follow(slot)` then `Animation.TraceText()` through eval, for one man's decisions.
- **Trap:** read a dead man through `Animation.TryDeath(slot, eventTick)`, never `State[slot]`: events are
  dispatched once per frame after every tick ran, and the slot may hold another man by then.

### Tanks and walkers drawn
- **Files:** `Presentation/Camera/TankRenderer.cs`, `Presentation/Camera/TankModel.cs` (parts, sockets, leg rigs),
  `Presentation/Camera/WalkerGait.cs` (planted feet), `Shaders/Tank_URP.shader`, `Shaders/TankDisc_URP.shader`,
  import rules `Editor/TankImport.cs`.
- **Hooks:** sets `TanksDrawn`, `VehicleTracks`, `VehicleGunPort`, `DrawnWreck`, `IsTankSlot`; calls `FootFall`.
- **Tests:** GaitTests (plus the sim tests above).
- **See it:** `TW.Editor.TankCapture.Spawn(team, archetype, x, z)`, then read `World.Position[slot]` back: the sim
  moves units to their deploy zone. Freeze with `SimHost.TimeScale = 0` before framing.
- **Trap:** vehicle meshes must import Read/Write enabled or scaling silently does nothing.

### Combat effects: tracers, bursts, smoke, camera shake
- **Files:** `Presentation/Camera/CombatFx.cs` (event dispatch `OnSimEvent`, tracers, bodies, bursts, materials),
  `Presentation/Camera/CombatFx.Mines.cs` (our mines and tripwires marked on the ground until they go off or a
  crater cooks them; a trigger's flash; the burst is the Explosion's), `Presentation/Camera/CombatFx.Deaths.cs` (a Death: the body from the controller's record, gibs by density, a burning
  man's pool and smoulder; `UnitAlight` lights and douses the drawn torch),
  `Presentation/Camera/CombatFx.Abilities.cs` (the aim's disc or corridor, the strafe's aircraft and tracers, the
  beam's charge and sweep, the scorch, the smoke screen's cards), `Presentation/Core/SimClock.cs` (the sim's clock in seconds for everything the picture times against the sim: the
  aircraft, the beam, the fires), `Presentation/Core/AimShape.cs` (the shape the aim describes and `SceneHooks.AimPreview`, the delegate the aim's owner sets
  so the effects draw it without knowing the panel), `Presentation/Camera/AbilityAim.cs` (the aim
  state machine: point, or press-drag-release with patterns and snapping; `TestPanel` drives it),
  `Presentation/Camera/CombatFx.Chunks.cs` (thrown dirt, splinters, smoke balls, cook-offs),
  `Presentation/Camera/CombatFx.Bodies.cs` (gibs, tree breaks, muzzle and chest positions),
  `Presentation/Camera/CombatFx.Ambient.cs` (birds, ambient smoke), `Presentation/Camera/CameraShake.cs`,
  `Presentation/Camera/FlipbookFx.cs` + `Shaders/Flipbook_URP.shader` (painted flipbooks, textures in `Resources/VFX/`).
  `CombatFx` is one partial class: an event arrives in `CombatFx.cs` and is handed to the part that draws it.
- **Hooks:** sets `Sparks`, `CookOff`, `FootFall`; reads `IsWater`, `AddRing`, `Flash`, `SmokeSources`, `IsTankSlot`,
  `VehicleTracks`, `VehicleGunPort`, `CloseUp`, `Biplane`.
- **Tests:** BlastReactionTests (camera feels a burst), ComponentLookupAllocationTests, AbilityAimTests (the aim).
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
- **Files:** `Presentation/Camera/Flamethrower.cs` (presentation only: the sim has no fire yet),
  `Shaders/Flame_URP.shader`, books cut by `Tools/firebooks.py`.
- **Hooks:** calls `FireLight`, `Sparks`.
- **Tests:** none.
- **See it:** `Tools/flameshots <prefix>` captures the four reference shots deterministically, and
  `Tools/flamecheck.py` rejects a shot with no fire in it.
- **Trap:** `FlipbookFx` indexes its sheets by the `Book` enum's ordinal and is only `Ready` when every sheet loads.
  A missing PNG or a row out of order silently disables every flipbook in the game. Add the enum entry, the row and
  the PNG together, and check `books.Ready`.

### Debris and destruction of props and houses
- **Files:** `Presentation/Camera/DebrisRenderer.cs` + `Shaders/Debris_URP.shader` (GPU-flown pieces, ring buffers,
  `ZoomShare`), `Presentation/Terrain/PropDestruction.cs` and `Presentation/Terrain/PropWear.cs` (one partial class),
  `Presentation/Terrain/TrenchSection.cs` (`TrenchSectionRules`: a lining section intact -> damaged -> gone, heavy
  ordnance, the pieces' life), `Presentation/Terrain/HouseKit.cs` (chunked buildings, `MaxChunks`, `ChunkMask`).
  Design: `docs/16-destruction.md` ("The lining breaks in two steps").
- **Hooks:** `PropDestruction` calls `CookOff`.
- **Tests:** DebrisTests, PropWearTests, HouseKitTests, TrenchSectionTests.
- **Trap:** a prop is identified by its position rounded to 0.25 m. Convert a drawn instance to a key through
  `PropWear.Home` first, or it forgets its damage. A lining panel and its broken twin share the panel's key
  (`KeyOf`): key a twin by itself and it becomes a second, undamaged thing. The twin is put where the panel stood by
  `BattlefieldProps.AddInstance` and kept there by `Replace`; the composer keeps emitting the whole panel.

### Battlefield props and composition
- **Files:** `Presentation/Terrain/BattlefieldComposer.cs` (seeded placement: the steps, the sites, the map's props; one
  class in six files), `Presentation/Terrain/BattlefieldComposer.Trench.cs` (the lining),
  `Presentation/Terrain/BattlefieldComposer.Ground.cs` (debris, graves, gatherings, margins, the winter ground),
  `Presentation/Terrain/BattlefieldComposer.Landmarks.cs` (landmarks, horizon),
  `Presentation/Terrain/BattlefieldComposer.Buildings.cs` (village, rear), `Presentation/Terrain/BattlefieldComposer.Scatter.cs`
  (the scatter step: builds the rules' input from the map and the layout, emits the placements),
  `Presentation/Terrain/BattlefieldKit.cs` (modules, env atlas cells) + `Presentation/Terrain/BattlefieldKit.Keys.cs`
  (module keys and scale rules),
  `Presentation/Terrain/BattlefieldProps.cs` (instanced pages, `Generation`, `Enforce`),
  `Presentation/Terrain/BattlefieldBlueprint.cs`, `Presentation/Terrain/PropLayout.cs` (`Style`) + `Resources/Layouts/`
  (owner's hand edits), `Editor/EnvPropEditor.cs`, `Editor/EnvKitImport.cs`.
- **Scale (docs/21 phase 1):** `Presentation/Terrain/AssetScaleTable.cs` (the soldier unit and every module's class and
  bounds), `Presentation/Terrain/AssetScaleReport.cs` (measures the composed field), `Editor/AssetScaleAudit.cs`
  (writes the report: menu TW/Audit/Asset Scale or `-executeMethod TW.Editor.AssetScaleAudit.Run`), `Tools/looks.py`
  (rescales the looks in the layout asset).
- **Scatter (docs/21 phase 2):** `Presentation/Terrain/ScatterField.cs` (`ScatterInput`: cells, banks, what stands up,
  footprints, ladders, roads, season, coast, rear bands; `ScatterField`: Traffic, Vertical, Patch, Wet, Open) and
  `Presentation/Terrain/ScatterLayers.cs` (grass, accents, flowers, frost, the camp's kit in trenches, dugouts and the
  rear; the caps). Design: `docs/13-environment-system-rebuild.md`, "Rule-based scatter".
- **Hooks:** reads `DrawnWreck`; sets `Biplane` (the kit's aircraft for the strafe's flyover).
- **Tests:** EnvAtlasTests, AssetScaleTests, ScatterRulesTests.
- **Trap:** `BattlefieldKit.EnvSets` / `EnvCols` / `EnvRows` must match `Tools/envatlas.py`. Walkers need ~10 m
  gaps between placed structures. The scale clamp lives in `BattlefieldProps.Emit` and `Placement`, not in
  `Styled` (which returns early without a look, and hand edits never pass through it); a building is reported,
  never clamped, because its chunks are placed by their own matrices. A new public `Module` field on the kit needs a
  row in `AssetScaleTable` (AssetScaleTests keys every field). The scatter's placements are computed once a map and
  re-emitted every pass: put nothing in `ScatterLayers.Place` that reads the surface (a crater), that filter is the
  emit loop's in `BattlefieldComposer.Scatter.cs`.

### Terrain view, weather, night, biomes
- **Files:** `Presentation/Terrain/GreyboxTerrainView.cs` (ground mesh, adds most environment components),
  `Presentation/Terrain/BattlefieldSurface.cs`, `Presentation/Terrain/Atmosphere.cs`, `Presentation/Terrain/NightLights.cs`,
  `Presentation/Terrain/Rain.cs`, `Presentation/Terrain/Storm.cs`, `Presentation/Terrain/QuietFog.cs`,
  `Presentation/Terrain/FogWisps.cs`, `Presentation/Terrain/SmallLife.cs`, `Presentation/Terrain/WaterRings.cs`,
  `Presentation/Terrain/BiomeProfile.cs`, `Presentation/Core/RenderGround.cs` (shared ground height, `SceneTints`),
  shaders `Toon_URP`, `Water_URP`, `TWAtmosphere.hlsl`, `TWLocalLights.hlsl`, `TWWater.hlsl`.
- **Hooks:** `WaterRings` sets `IsWater` and `AddRing`; `NightLights` sets `Flash`, `FireLight` and fills
  `SmokeSources`; `SmallLife` and `NightLights` read `CloseUp`.
- **Tests:** BiomeProfileTests, PaintedHorizonCompressionTests, WinterLevelTests.
- **Trap:** post-processing only runs because `Settings/TW-Renderer.asset` references URP's `PostProcessData`; with
  it null the whole grade silently does nothing while the volume stack still reports its values.
- **Trap:** `SimHost.Ground` picks the terrain but not the look. For a winter test set the biome look too
  (`feature-flags.md`).

### Camera
- **Files:** `Presentation/Camera/TacticalCamera.cs` (standard view: fov 25, pitch 25, zoom 30; `FrameFrom`),
  `Presentation/Camera/CameraShake.cs`, `Editor/GameViewFit.cs`.
- **Hooks:** sets `CloseUp` (so do `CaptureRig` and `PerfBench` while they shoot).
- **Trap:** setting `Camera.main.transform` does nothing; the controller overwrites it every frame. Use `FrameFrom`.

## Interface (SHOW lane)

### Battle HUD (UI Toolkit, the live one)
- **Files:** `UI/HudController.cs`, `UI/HudView.cs`, `UI/HudText.cs` (every word), `UI/HudLayout.cs`,
  `UI/HudMinimap.cs`, `UI/TrenchOrderCluster.cs`, `UI/HudBootstrap.cs`, `UI/Resources/Hud/BattleHud.uxml`.
- **Tests:** HudBindTests, HudStructureTests, HudLayoutPlayTests, UnitArtTests.
- **See it:** `TW.Editor.HudCapture.Shoot(path)`. The ordinary capture paths do not include the HUD.

### Legacy IMGUI HUD and debug panel
- **Files:** `Presentation/Camera/BattleHud.cs` (F9 switches to it), `Presentation/Camera/TestPanel.cs`,
  `Presentation/Camera/DebugOverlay.cs`.
- **Tests:** HudLayoutTests, HudTextTests test its static helpers only. No test runs OnGUI.

### Selection
- **Files:** `UI/Selection/SelectionController.cs`, `UI/Selection/SelectionModel.cs`, `UI/Selection/UnitPicker.cs`
  (pick radius), `UI/Selection/SelectionMarkers.cs`, `UI/Selection/HoverCard.cs`, `UI/Selection/SelectionPanel.cs`.
- **Tests:** SelectionTests.

### Menus, settings, keys, match launch
- **Files:** `UI/Shell/ShellRouter.cs`, the `UI/Shell/` screens, `UI/Shell/SettingsApplier.cs`,
  `Presentation/Core/GameSettings.cs`, `Presentation/Core/SettingsStore.cs`, `Presentation/Core/KeyMap.cs`,
  `Presentation/Core/MatchLaunch.cs`, `Presentation/Core/MatchClock.cs` (owns `SimHost.TimeScale`).
- **Tests:** ShellUxmlTests, ShellRouterPlayTests, GameSettingsTests, KeyMapTests, MatchClockTests, MatchLaunchPlayTests.

### Campaign shell
- **Files:** `UI/Campaign/HomeFrontScreen.cs`, `UI/Campaign/StrategicMapScreen.cs`, `UI/Campaign/StagingScreen.cs`
  (the three campaign screens; their UXML is hand-written under `UI/Resources/Shell/`, loaded by name like the
  Armoury), `UI/Campaign/ShellPick.cs` (is the mouse over a plate, for the 3D views' picking),
  `UI/Campaign/CampaignGraph.cs` (the country nodes, their missions in order, prerequisites and gold: a
  code table, no asset), `UI/Campaign/FactionBuildings.cs` (the Home Front's buildings, their stages and upgrade
  lines per faction, priced and capped against the profile), `Presentation/Core/CampaignProfile.cs` +
  `ProfileStore.cs` (profile.json beside settings.json, versioned, written through a .tmp swap),
  `Presentation/Core/CampaignSession.cs` (the mission in flight across the scene load),
  `Presentation/Core/MetaViews.cs` (the seam to the 3D views: `IHomeFrontView`, `IStrategicMapView`,
  `MetaServices`), `Presentation/Meta/` (its own assembly, `TW.Presentation.Meta`: `HomeFrontDiorama.cs` and
  `HomeFrontStages.cs` (sliced houses shown to a stage by chunk mask), `StrategicMapView.cs` and
  `ContinentMesh.cs` (the generated continent, pins, the front line, the fog sheet), `MapFog.cs` (the fog mask:
  clear round every reachable node, dense elsewhere), `MetaCamera.cs` (orbit / map),
  `MetaMeshes.cs`, `MetaBoot.cs` (installs the view factories at start-up)).
- **Tests:** CampaignGraphTests, CampaignProfileTests, FactionBuildingsTests, HomeFrontDioramaTests,
  StrategicMapMeshTests.
- **Trap:** `CampaignSession` must not register with `SceneStatics` (the router resets those on every scene load,
  which is exactly when the session has to survive); `MatchLaunch.QuitToMenu` clears it. Unit tiers, armour
  plate and the ability mask are stored in the profile but reach the sim only once the upgrade seam lands
  (docs/21 B1); today only the depot's silver and income go into the launch request, and the HUD fields only the
  cards the request's `AbilityMaskA` allows (`HudView.Offered`: cards and keys; the sim itself does not refuse a
  masked ability yet). The debrief pays a
  campaign win through `ProfileStore.Current`: a test that binds it sets `ProfileStore.Persist = false` and
  `ProfileStore.Use(profile)` first, or it writes the player's profile.json.

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
  is explained there).
- **Trap:** never clear `SceneHooks` from `SceneStatics.Reset`: it runs after the new scene has wired its hooks.

### Performance and allocations
- **Files:** `Perf/PerfBench.cs`, `Perf/AllocProbe.cs`, `Presentation/Core/HeavyWork.cs`, `FrameBudget` in
  `Presentation/Core/RenderGround.cs`. Budgets and past runs: `docs/05-performance-budgets.md`.
- **Tests:** AllocProbeSanityTests, TickAllocationTests, ComponentLookupAllocationTests, VatAtlasMemoryTests.
- **Trap:** `GC.GetAllocatedBytesForCurrentThread` reads 0 in Unity. Count allocations with `AllocProbe`.

### Windows build
- **Files:** `Editor/BuildWindows.cs`, `Resources/ShaderKeep/`.
- **Tests:** ShaderInclusionTests (every `Shader.Find("TW/...")` must be Always Included or it breaks the player).

## Generated indexes (do not edit: `python Tools/codemap.py`)

<!-- gen:hooks -->
| SceneHooks member | Set by | Read / called by |
|---|---|---|
| `CloseUp` | CaptureRig, PerfBench, TacticalCamera | Atmosphere, BattlefieldProps, CombatFx, CombatFx.Chunks, CombatFx.Ground, NightLights, PropDestruction, SmallLife |
| `IsWater` | WaterRings | CombatFx, CombatFx.Ambient, CombatFx.Chunks, CombatFx.Ground, CombatFx.Mines, NightLights, TankRenderer |
| `AddRing` | WaterRings | CombatFx, CombatFx.Chunks, LandingCraftView, TankRenderer |
| `Sparks` | CombatFx | CombatFx.Abilities, Flamethrower, NightLights, TankRenderer |
| `AimPreview` | TestPanel | CombatFx.Abilities |
| `SmokeSources` | NightLights | CombatFx.Ambient |
| `TanksDrawn` | TankRenderer | CombatFx, CombatFx.Ground, VATRenderer |
| `VehicleTracks` | TankRenderer | CombatFx.Ground |
| `VehicleGunPort` | TankRenderer | CombatFx.Bodies |
| `DrawnWreck` | TankRenderer | BattlefieldComposer |
| `IsTankSlot` | TankRenderer | CombatFx, CombatFx.Deaths, CombatFx.Ground |
| `Flash` | NightLights | CombatFx.Chunks, TankRenderer |
| `FireLight` | NightLights | Flamethrower |
| `CookOff` | CombatFx | PropDestruction |
| `FootFall` | CombatFx | TankRenderer |
<!-- /gen:hooks -->

<!-- gen:tests -->
| Test class | Mode | Tests | Production types it touches most |
|---|---|---|---|
| `AbilityAimTests` | EditMode | 14 | OffMapAbilityId, CombatFx, AbilityAim, AimReadout, Line, ScreenUnit |
| `AbilityArgsTests` | EditMode | 4 | AbilityArgs |
| `AllocProbeSanityTests` | EditMode | 5 | AllocProbe |
| `AssetScaleTests` | EditMode | 7 | AssetScaleTable, Rule, PropLayout, BattlefieldKit, Module, ScaleAxis |
| `BarragePatternTests` | EditMode | 7 | OffMapAbilityId, SimEventType, OffMapAbilitySystem, SimEvent, AbilityPattern, SimCommand |
| `BattlefieldLockstepTests` | EditMode | 6 | SimHash, MatchSim, SimCommand, BattlefieldParams, SimConfig, SimWorld |
| `BattlefieldTests` | EditMode | 10 | NavLayer, PropKind, BattlefieldParams, BattlefieldGenerator, Kind, MatchSim |
| `BeamTests` | EditMode | 8 | Sweep, SimEventType, OffMapAbilityId, SimCommand, Kind, BeamSystem |
| `BiomeProfileTests` | EditMode | 3 | SceneTints, BiomeProfile, Atmosphere, Biome |
| `BlastReactionTests` | EditMode | 6 | Clip, VatPad, CameraShake, Burst, AnimationController, MatchSim |
| `BurningSystemTests` | EditMode | 12 | SimEventType, BurningSystem, DeathCause, Impact, MatchSim, UnitFlags |
| `CampaignGraphTests` | EditMode | 8 | CampaignGraph, NodeState, CampaignProfile, Difficulty, Ground, CampaignDifficulty |
| `CampaignProfileTests` | EditMode | 5 | CampaignProfile, ProfileStore, Faction, Sample, Building, BuildingState |
| `CoastTests` | EditMode | 6 | BattlefieldGenerator, BattlefieldParams, SeaLandingSystem, Sample, MapData |
| `CombatTests` | EditMode | 9 | MatchSim, SimCommand, SimEventType, Stance, CommandType, GoalKey |
| `CommandSeatTests` | EditMode | 1 | CommandSeat, LockstepDriver, LoopbackNetwork, MatchSim, SimCommand, SimConfig |
| `CommandValidationTests` | EditMode | 5 | SimCommand, MatchSim, SimConfig, SimEventType, SimWorld |
| `ComponentLookupAllocationTests` | EditMode | 1 | IZoomSource, CombatFx, AllocProbe, Shot |
| `CrabTests` | EditMode | 13 | VehicleArchetype, RosterEntry, VehicleProfile, NavLayer, TankSpec, MatchSim |
| `DeathEventContractTests` | EditMode | 5 | DeathCause, SimEvent, SimEventType, Impact, MatchSim, AmbientBombardmentSystem |
| `DeathVarietyTests` | EditMode | 12 | Clip, VatPad, VATRenderer, DeathCause, DeathKind, AnimationController |
| `DebrisTests` | EditMode | 8 | DebrisMath, DebrisRenderer, DebrisRng, Piece, Debris, Record |
| `DeterminismReplayTests` | EditMode | 3 | SimCommand, OffMapAbilityId, MatchSim, SimConfig, AbilityPattern, ReplayRecorder |
| `DirectionalBlastTests` | EditMode | 8 | BlastRules, Impact, UnitFlags, MatchSim, AmbientBombardmentSystem, BlastShape |
| `DynamicGroundTests` | EditMode | 11 | NavLayer, CraterStamp, Snapshot, MapData, MatchSim, CraterKind |
| `EnvAtlasTests` | EditMode | 2 | BattlefieldKit |
| `FactionBuildingsTests` | EditMode | 8 | FactionBuildings, VehicleArchetype, OffMapAbilityId, CampaignProfile, Kind, LineKind |
| `FlowFieldManagerTests` | EditMode | 4 | GoalKey, FlowField, MatchSim, NavLayer, SimCommand, NavMode |
| `FlowFieldTests` | EditMode | 2 | FlowField, NavLayer, GreyboxMapGenerator, Kind, MapData, ObjectiveKind |
| `GaitTests` | EditMode | 12 | WalkerGait, VehicleArchetype, TankModel, Body, Foot, Rest |
| `GameSettingsTests` | EditMode | 10 | GameSettings, SettingsStore, SettingsApplier, Bindings, AudioLevels, GameAction |
| `GarrisonAndOrdersTests` | EditMode | 9 | CommandType, SimCommand, NavLayer, MatchSim, UnitFlags, SimEventType |
| `GarrisonTests` | EditMode | 5 | MatchSim, SimMath, Stance, SimCommand, SimConfig |
| `HashIntervalTests` | EditMode | 2 | SimCommand, MatchSim, SimConfig, LockstepDriver, LoopbackNetwork, ReplayRecorder |
| `HeightfieldRaycastTests` | EditMode | 4 | HeightfieldRaycast, Sample, Look, Stance, Heightfield, BattlefieldGenerator |
| `HomeFrontDioramaTests` | EditMode | 6 | HomeFrontStages, MetaServices, Chunk, HouseKit, House, MetaBoot |
| `HouseKitTests` | EditMode | 8 | HouseKit, ChunkMask, House, Module, BattlefieldKit, Chunk |
| `HudBindTests` | EditMode | 11 | HudView, HudText, VehicleArchetype, RosterEntry, OffMapAbilityId, IntText |
| `HudLayoutTests` | EditMode | 6 | HudLayout, BattleHud, HudView, RosterEntry |
| `HudStructureTests` | EditMode | 6 | RosterEntry, HudView, BattleHud, HudText |
| `HudTextTests` | EditMode | 11 | BattleHud, VehicleArchetype, TankSpec, RosterEntry, OffMapAbilityId, OffMapAbilitySystem |
| `KeyMapTests` | EditMode | 9 | KeyMap, GameAction, Bindings |
| `LandingTests` | EditMode | 8 | SimCommand, MatchSim, Sample, LandingState, BattlefieldGenerator, BattlefieldParams |
| `MineTests` | EditMode | 13 | MineKind, MineSystem, SimEventType, Mine, MineState, MapData |
| `PaintedHorizonCompressionTests` | EditMode | 3 | GreyboxTerrainView |
| `PlaytestMapTests` | EditMode | 4 | SimCommand, MatchSim, CommandType, GoalKey, SimConfig, UnitFlags |
| `PropWearTests` | EditMode | 8 | PropDestruction, CombatTables, DebrisRenderer, Piece, SimConfig |
| `ScatterRulesTests` | EditMode | 8 | Kind, ScatterKind, ScatterField, ScatterInput, ScatterLayers, NavLayer |
| `SelectionTests` | EditMode | 15 | UnitState, UnitStatus, UnitPicker, ScreenUnit, Stance, GarrisonStats |
| `ShaderInclusionTests` | EditMode | 2 | CombatFx |
| `ShellUxmlTests` | EditMode | 15 | DebriefScreen, MatchLaunch, MatchReport, CampaignProfile, CampaignSession, ProfileStore |
| `SimHashTests` | EditMode | 3 | SimHash, SimRandom, SimMath, SystemId |
| `SinglePlayerEquivalenceTests` | EditMode | 1 | LockstepSession, MatchSim, ScriptedEnemy, SimCommand, SimConfig |
| `SkinAssetTests` | EditMode | 8 | HudLayout, SkinSpec, Kind, SkinKind, UiSkinVerifier |
| `SmokeScreenTests` | EditMode | 5 | SmokeLos, OffMapAbilityId, OffMapAbilitySystem, SimEventType, SimCommand, CombatTables |
| `StaticLifecycleTests` | EditMode | 3 | SceneHooks, SceneStatics, Atmosphere, CameraShake, MatchLaunch, CombatFx |
| `StrafeRunTests` | EditMode | 5 | OffMapAbilityId, SimEventType, SimCommand, SimEvent, MatchSim, OffMapAbilitySystem |
| `StrategicMapMeshTests` | EditMode | 7 | ContinentMesh, MapFog, StrategicMapView, CampaignGraph, Cell, Sheet |
| `SupportAbilityTests` | EditMode | 5 | OffMapAbilityId, SimCommand, MatchSim, CommandType, SimEventType, Impact |
| `TankMobilityTests` | EditMode | 7 | VehicleModulesSystem |
| `TankTests` | EditMode | 13 | SimEventType, VehicleArchetype, Armor, Kind, SimCommand, PropKind |
| `TickAllocationTests` | EditMode | 2 | LockstepDriver, AnimationController, EventPump, MatchSim, SimPresenter, SimCommand |
| `TrenchSectionTests` | EditMode | 6 | SectionState, TrenchSectionRules, BattlefieldKit, AssetScaleTable, Module, DebrisMath |
| `TrenchSpreadTests` | EditMode | 12 | TrenchPost, MatchSim, BattlefieldParams, MapData, BattlefieldGenerator, SeparationJob |
| `UnitArtTests` | EditMode | 8 | UnitArt, Mood, HudDialogue, SimEvent, SimEventType, ArmouryScreen |
| `VatAssetTests` | EditMode | 3 | Clip, VatAsset, Socket, VatCodec, AnimRow, Clips |
| `VatAtlasMemoryTests` | EditMode | 5 | Figure, VATRenderer, VatAsset, VatAssetData, VatCodec, ProceduralSoldier |
| `VatEarlyZTests` | EditMode | 2 | VATRenderer |
| `WinterLevelTests` | EditMode | 6 | Ground, Biome, BiomeProfile, BattlefieldGenerator, BattlefieldParams, MatchLaunch |
| `WinterMapTests` | EditMode | 7 | BattlefieldParams, BattlefieldGenerator, MapData |
| `CanaryFixture` | PlayMode | 0 | SimHost |
| `HudLayoutPlayTests` | PlayMode | 3 | HudBootstrap, HudView, RosterEntry, HudLayout, AllocProbe, BattleHud |
| `LockstepLoopbackTests` | PlayMode | 3 | SimCommand, LockstepDriver, MatchSim, ReplayRecorder, LoopbackNetwork, SimConfig |
| `MatchClockTests` | PlayMode | 5 | MatchClock, Hold, SimHost |
| `MatchLaunchPlayTests` | PlayMode | 4 | FactionBuildings, MatchLaunch, SimHost, AudioLevels, CampaignGraph, HudBootstrap |
| `ShellRouterPlayTests` | PlayMode | 1 | ShellBoot, ShellAssets, ShellRouter |
<!-- /gen:tests -->
