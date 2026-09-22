# Handoff: Trench Warfare 1917 → 3D

Branch `claude/trench-warfare-2d-3d-plan-idt7lf` holds the roadmap, its 2026-09-20 review, and a Unity 6000.0.50f1
project with Phase 0 implemented and a true, tested baseline (P0.5).

## 1. Where things are
| Path | Content |
|---|---|
| `docs/11-plan-review.md` | **Plan of record.** Owner decisions, what was wrong with the first skeleton, re-cut milestones, Unity skill map, open follow-ups |
| `docs/PLAN.md`, `docs/00-…10-*.md` | The original plan; catalogs (units, abilities, missions) still authoritative, architecture/milestones partly superseded by 11 |
| `trench-warfare-3d/` | Unity project: 18 assembly definitions, Phase 0 code, stubs for every later phase, tests, generated `ProjectSettings/` and `.meta` files |
| `trench-warfare-3d/validate.py` | Offline checks (JSON, assembly graph, sim purity, phase headers). Not a compile check |
| `.github/workflows/unity.yml` | CI from `unity ci init`; needs the secrets named in its header before it goes green |
| `github-test1/` | Unrelated older test project. Do not touch |

## 2. State of the work
- **M1 is done.** A1 is implemented: `FlowFieldManager` (goals, lazy time-sliced fields, tracked mode), garrison stop,
  trench orders (`TrenchOrdersSystem`, initial), stance/terrain speed, `VehicleKinematicsSystem`. The M1 acceptance run
  (`Stress_ThreeThousandUnits_AdvanceAcrossCorridor_StayInSync`) holds 2,000 units hash-identical over lossy loopback for
  5,000 ticks at 0.31 ms/tick. In the greybox scene the **test panel** (right side, `TestPanel`) has everything:
  deploy buttons, per-trench `>>` `↩` lock hold-fire for both sides (so you can stage an enemy assault), speed 0-8x,
  camera presets, restart. Hotkeys still work (1-5 deploy, Space `>>`, Backspace `↩`, L lock, F3 field view);
  `SimHost.StressUnits` deploys N per side and sends both over the top.
- **M1.5 is code complete; the two-developer playtest is what remains.** Landed: A2 core (`TargetAcquisitionSystem`, `DirectFireSystem` with near-miss suppression and
  deaths, `SuppressionSystem` decay, prone / pinned / fire-step stances in the move job, hold-fire that hides a garrison
  below the rim), garrison spacing along the trench, wall sliding, A3 core (`SectorControlSystem`: objectives fall in
  order, a captured trench changes owner, an HQ ends the match), tracers / bodies / banners (`CombatFx`), a scripted
  enemy that attacks on its own and calls support. Support fire (`OffMapAbilitySystem`): HE barrage (12 shells, 25 m,
  4 s warning, `BlastSystem`, craters through `DeformationSystem` that are cover and show on the ground) and chlorine
  (`GasSmokeSystem`: wind drift, pools in trenches, a gassed garrison runs for its HQ and `fall back` brings it home).
  Weapons and abilities are placeholder tables (`CombatTables.WeaponFor`, `OffMapAbilitySystem.TryGetStats`). Known
  balance state, for the playtest to judge: a defended trench beats an unsupported assault about 30 to 0, so an attack
  needs a barrage or gas first. The scene now plays on the **playtest map** (`SimHost.PlaytestMap`, 300 x 480 m, reserve
  and front trench per side, 200 m of no man's land): reinforcements stop at the rear trench, `>>` moves them up the
  chain, a locked trench passes them on, the lines fall front, reserve, HQ. Small arms still cannot hurt the tank, but
  infantry within 8 m close-assault it with grenades. The playing interface is `BattleHud` (silver, bottom deploy bar,
  order buttons anchored to each owned trench); `TestPanel` is the debug drawer behind the top-right button.
- The project compiles under 6000.0.50f1 and `unity test` passes EditMode and PlayMode. Entities / Entities Graphics
  were removed; the replay format is v2 (hash content changed again at M1, see docs/02 change log).
- **Standard view (owner, 2026-09-21): everything on screen is built for it.** 25 degree lens, 25 degrees above the
  horizon, zoom 30 (camera about 78 m from its focus, 33 m up), looking across the front and turned 21 degrees towards
  the enemy. `TacticalCamera` follows the battle along Z: behind your men +21 degrees, between the sides square-on and
  17 degrees flat, beyond the enemy -21 degrees; far zoom lifts to 62 degrees. This replaces the earlier near-top-down
  "75 % like the 2D game" view. Men are about 60-90 pixels tall in it, so unit art budget is now: full model
  1,200-1,500 vertices (ceiling 2,000), far model 250-400 (used beyond `VATRenderer.LodDistance` = 170 m; the box
  soldier stands in). Measured worst case, 2,900 units with 650 in view: 1.2 M unit vertices with shadows, under the
  1.5 M budget. The battlefield is 90 x 240 m (owner: 300 wide was too wide to fill, then halved both ways); the
  generator's layout scales with the params, no man's land is 100 m, so rifles (130 m) now reach trench to trench.
  Trenches are bent, not ruled (owner, 2026-09-21): 20 m fire bays set up to 4 m forward or back, joined by traverses;
  **Placement rule (owner, 2026-09-21): nothing is ruled or evenly spread.** Lines (trenches, wire, river) wander on
  noise; point features cluster (craters in salvos, trees in Poisson-spaced clumps) and every clump has one big member,
  a few medium and many small (`PropDef.Scale`). The dressing follows the same rule: `BattlefieldComposer.Clumps`
  gathers scrub, grass and stones round every big shape, litter gathers by a density noise, knife-rests are shoved,
  turned and sometimes knocked over, and the land beyond the map lets edge features drift and fade instead of
  extruding them. Ladders stand at uneven intervals, each in a short straight piece of trench.
- **Visual standard (owner, 2026-09-21): painted cartoon mudfield**.
  Organic trench pass: shared dressing contours now sway at roughly 40 m wavelength, ease around bay corners and
  taper back to fixed ladder openings. Bank mass, revetment and parapets follow the same contour. Three timber,
  sandbag and duckboard variants provide repaired sections, uneven courses and small wear; trench lamps follow
  the reshaped walls. Tight notches locally reduce sway to keep joins connected. Simulation routes and trench/link
  floor heights are preserved. See `docs/14-organic-trenches.md` and its standard-view before/after captures.
  Northstar: `docs/reference/battlefield-northstar.jpeg`. The rebuilt generation contract and three-round critique are in
  `docs/13-environment-system-rebuild.md` (supersedes the original generator details in document 12).
  `BattlefieldProps` now renders spatial instance pages; `BattlefieldKit`, `BattlefieldGeometry` and
  `BattlefieldPigment` build reusable parts and materials. `BattlefieldBlueprint` describes named sockets and
  mesh-derived footprints. `BattlefieldComposer` places them from seeded trench-boundary candidates, with
  terrain/prop/link clearance and connected approaches. New content uses `Kit.CreateModule` and `UseBlueprints`.
  `BattlefieldSurface` drives continuous earth shoulders and crater interpretation on a half-metre presentation
  mesh. Shared `RenderGround` heights keep units and combat effects aligned; simulation data and floor heights
  in trenches/links are unchanged. Terrain repaint tiles coalesce and spread work across frames.
  Standard sample: 118,923 base prop vertices and 100 instanced submissions, excluding terrain and other passes.
  Later the same day: presentation-only mounds and crater lips (`BattlefieldSurface.Mound`), dark reflective water,
  ground mist and a colour grade (`Atmosphere`), and a screen-space ink pass for lines inside shapes
  (`Shaders/InkLines_URP.shader`, a Full Screen Pass feature on `Settings/TW-Renderer.asset`, depth only, 5 taps a
  pixel; unmeasured on the GTX 1050). The scored critique log is `docs/reference/visual-score.md` (46 to 57 of 80).
  Fog, mud and water (owner request, 2026-09-21): a fog bank closes the view round the battlefield
  (`Shaders/TWAtmosphere.hlsl`, shared by TW/Toon, TW/Water and the ink pass: a function of world position, no
  geometry, no overdraw; `Atmosphere` Bank* fields). The ground's close-up surface is a code-made 512 px detail map
  (`GreyboxTerrainView.BuildMudDetail`: tone in R, slope in GB lit as a hard-edged relief, broad tone in A) that fades
  to the broad tone beyond 90 m. The river and flooded shell holes use `Shaders/Water_URP.shader`: one opaque quad,
  three texture reads a pixel, no scene-depth read or grab pass; depth bands, shoreline and lapping rings come from a
  depth map baked off the drawn ground (one texel a ground vertex, repainted with the chunks a crater touches).
  Same day, second pass: `QuietFog` (a thin low fog inside the field that lifts within 55 m of the player's men,
  round their HQ and briefly at shell bursts; a 4 m presence map read by every world shader; presentation only, it
  hides nothing from the sim or HUD; team 0 is assumed local), `FogWisps` (28 drifting soft cards at most, one mesh
  and draw call, `Shaders/FogWisp_URP.shader`), `WaterRings` (16 shader rings for wading men and shell bursts in
  water, `Shaders/TWWater.hlsl`), painted puddles drawn with the river's bands and shore (ground texture alpha now
  carries depth: 0.4 .. 0 water, 0.5 liquid mud, below 1 a sheen), and mud by zone in `GroundColor` (cracked crust on
  rises, sheen in dips, boot-churned patches round ladders and ramps).
  Night (owner's target `docs/reference/battlefield-night.jpeg`, 2026-09-21): `Atmosphere.Look` picks the mood and
  defaults to `Night` (set `OvercastDay` for the earlier look). Night is a low blue moon coming toward the standard
  view, a deep blue shade tint (`_TWShadeTint`), a sky colour for mirrors (`_TWSky`), soaked glinting mud (`_TWWet`),
  dark blue haze / mist / fog bank, a colder grade and Bloom. `NightLights` adds the warm half as plain URP point
  lights without shadows, stepped by `Shaders/TWLocalLights.hlsl` in TW/Toon, TW/Water and the soldier shader:
  lanterns at composed sites (12 at most) and on trench walls (14), burning stumps (5), a pool of 8 lights for muzzle
  flashes and shell bursts, a star shell every 22-40 s, and additive glow cards (`Shaders/Glow_URP.shader`) for lamp
  halos and horizon fires. Per-object additional light limit raised 4 -> 8 in `Settings/TW-URP.asset`; the renderer
  stays Forward. Unmeasured on the GTX 1050.
  Night, second pass: soaked ground is darker and throws a highlight from every clod (`_TWWet` in TW/Toon); most
  shell holes stand full of water (`BattlefieldSurface.Flooding`, `Bed` / `PoolDepth`: the drawn surface is the flat
  pool, `RenderGround` keeps the bed at most 0.42 m under it so men wade and debris half sinks; presentation only);
  long stretches of trench floor are flooded under the duckboards; more puddles; at night each side's tracers are
  over-bright green / red (`SceneMood.Night` in Presentation.Core, read by `CombatFx`).
  Night, third pass (owner's second target `docs/reference/battlefield-night-game.jpeg`): glows and light pools are
  layered (white-hot core, the light's colour, a halo pushed to its deepest channel) in `Glow_URP.shader` and
  `TWLocalLights.hlsl`; every pooled flash light has a glow card; tracers are a white-hot streak inside an additive
  coloured halo; a rifle shot leaves one small smoke puff (capped at 420 live chunks); smoke is cold blue at night.
  Hard wet glints come from a second, 4.3x finer read of the mud slopes (close range only). Every TW/Toon prop is
  wet at night (gloss by how far a face points up). `Rain` + `Shaders/Rain_URP.shader`: 1,040 streaks in one mesh,
  wrapped round the camera's view in the vertex shader; raindrop rings on all water come from `TWWater.hlsl`
  (`_TWWet.z`, `Atmosphere.Rain`).
  Weather: `Atmosphere.RainNow` / `WindNow` swell and slacken on slow noise (`Squalls`); `Rain` integrates the fall
  on the CPU so a wind change bends the streaks, and each of its 2,800 streaks has its own threshold, so a drizzle is
  few and short and a downpour is all of them. Drops burst on every sky-facing TW/Toon surface within 70 m
  (`TWRainSplash`, hashed by cell and time slot, no texture) and ring the water in step with the squalls.
  Polish inside the low-desktop budget: local lights also glint on anything wet (one reflect and one pow a light, in
  the existing loop of `TWLocalLights`); `Shaders/Flame_URP.shader` draws computed flames on burning stumps and on up
  to 6 torches at dugout paths; lamps and flame lights sway with `WindNow`; shell bursts throw additive sparks at
  night; sheet lightning fires at the height of a squall (`Atmosphere.Lightning`); the night grade adds Neutral
  tonemapping and film grain, both inside URP's final post pass. Deliberately not added: SSAO, depth of field,
  Forward+, light shadows, all of which need a GTX 1050 measurement first.
  Battlefield details (owner request): rounds that miss kick up dirt, or a splash and a ring in water, with the odd
  ricochet spark at night (5 a frame at most); a shell burst puts up crows from the nearest tree, jolts the camera
  (`CameraShake`, runs after `TacticalCamera`), throws a white column if it lands in water and leaves embers glowing
  in its hole; running men kick up mud; dugout stoves smoke and rained-on fires steam; the star shell sheds burning
  drops; guns flicker beyond the horizon; lightning shows in every puddle; squalls close the haze in; and rain far
  off hangs in wind-driven curtains (`Shaders/RainCurtain_URP.shader`, 49 cards, empty cells collapse in the vertex
  shader). `SceneHooks` in Presentation.Core carries the few calls between the camera and terrain assemblies.
  Thunder and lightning: `Storm` aims each strike at a point in the upper part of the frame (the camera never sees
  the sky), builds a forked bolt as ribbons (`Shaders/Bolt_URP.shader`), flickers it 2-4 times, lights the landing
  point, and drives `Atmosphere.StormFlash` / `StormLightFrom` (the key light becomes the bolt's for the flash, so
  shadows swing). Thunder is synthesised from noise in `Storm.MakeThunder` (the project has no audio assets yet),
  three variants by distance, delayed by the speed of sound. Strikes every 7-24 s by rain level.
  World-generator rules on our own terrain (owner's notes on Gaia / Terrain Tools / MicroVerse, 2026-09-21; no asset
  bought, nothing baked): `BattlefieldSurface.BuildDrainage` is hydraulic erosion as a D8 flow map over the drawn
  ground (recomputed when shell holes change); `Rill` cuts the mesh up to 0.17 m and `GroundColor` paints the
  watercourse (dark slick channel, pale silt margin); a slope rule shows streaked subsoil on steep ground; and
  `BattlefieldComposer.Margins` is a biome-style spawn rule: reed stands (`kit.reeds`) only on the river's margin, the
  rims of flooded shell holes and the wet ends of drainage lines, dart-thrown 3.5 m apart, big / medium / small.
  Refinements: reeds, grass and scrub bend with `Atmosphere.WindNow` (TW/Toon `_Sway`, the same in every pass so ink
  and depth follow); `GreyboxTerrainView.Look` sets the mood of the Atmosphere it adds (set it to `OvercastDay` in
  the Inspector to see the day look); the bolt is a drift-corrected random walk with its points crowded toward the
  ground, where the camera sees it; thunder is soft-limited and normalised (it used to hard-clip) and the far
  variant fades in; the guns beyond the horizon are lower and dimmer.
  Close-up detail (what only a zoomed-in camera sees): `TacticalCamera` publishes `SceneHooks.CloseUp` and the shader
  global `_TWClose` (1 at zoom <= `DetailFullZoom` 12, 0 from `DetailGoneZoom` 28, so 0 at the standard view). Every
  item below is made and drawn only while it is above 0; `BattlefieldKit.Module.MaxDistance` is the draw distance for
  small props (`BattlefieldProps` skips their 32 m pages beyond it, and always at the standard view).
  Shaders: TW/Toon reads the mud detail a third time, five times finer and turned (grit, sparkle); `TWWater.hlsl`
  clouds the water inside a young wading ring with silt, adds a finer rain-ring grid and scum flecks at puddle rims;
  the VAT shader cakes boots and shins with mud and puts a rain film and bursting drops on helmets and shoulders.
  The fallen: `VATRenderer.AddFallen` plays one of the four death rows once and holds its last frame (own small
  buffer, near model within 70 m, box model beyond, no shadows), replacing the flattened capsules;
  `ProceduralSoldier.BuildFallen` is the fallback when the VAT path is unavailable (there the helmet rolls off).
  `CombatFx`: boot prints and tank ruts (`TW/GroundMark`, pooled quads with three fade bands, matt pressed mud with
  sky-mirroring water in the deepest part), ejected brass that bounces and lies, a thread of muzzle smoke, breath at
  night, tank exhaust and mud flung from the tracks, clods round fresh craters that steam in the rain; tracers thin
  to 30% up close. Small kit (`BattlefieldKit.BuildSmallKit`, 13 modules, no shadows, 55 m): helmets, boots, mess
  tins, entrenching tools, ammunition tins, rifle-and-helmet markers (`BattlefieldComposer.Litter`); rifles against
  the wall, tins on a nail, buckets, signboards by the ladders and the telephone wire along the revetment
  (`TrenchKit`); rags and alarm tins on the wire (`MapProps`, TW/Toon `_Sway`). `SmallLife`: moths, ash and drips
  are one mesh moved in the TW/Motes vertex shader; up to six rats run the trench floors and bolt from men and shells.
  `Atmosphere.CloseLens`: Gaussian depth of field and a closer vignette in the super zoom only (component inactive
  otherwise). Not measured on the minimum GPU; nothing here runs at the standard view.
  Character controller: designed, not built. `docs/15-character-controller.md` is the design of record (the
  per-man ladder, locomotion, actions, reactions, environment, deaths, archetype tables, the VAT and baker changes,
  the sim steps that need the owner's yes). `docs/reference/animation-clips.md` classifies the owner's 205 Mixamo
  clips (131 use / 28 spare / 46 ditch) from measurements made by `Tools/fbxscan.py`; the unnamed download
  `3e5cc2...fbx` is the forward belly crawl. The clips stay in the owner's Downloads until the baker's clip table
  exists (step 1 of the order of work).
  The 15 clips the download lacked were manufactured: `Tools/animforge.py` reads and writes Mixamo binary FBX in pure
  Python (per-frame bone curves, reverse / retime / layer / offset / IK / FK / stick-figure PNGs) and
  `Tools/make_missing_clips.py` holds the recipes (`python Tools/make_missing_clips.py "<download folder>"` writes
  them to `<download folder>/Made/`). Verified by importing into Unity (one clip, 340 curves each) and rendering on the
  skinned figure: `docs/reference/made-clips/`. Not in the repo: the FBX files live with the download.
  Final stress capture: 3,001 alive (stress harness plus a transient peer unit), no desync, 1,291 drawn,
  1,199,339 reported unit vertices with shadows disabled by the existing guard; under the 1.5M unit budget.
  Three independent harsh scores: **29.25 -> 34 -> 44.75 /100**. This is a reusable foundation, not reference-level
  beauty. Biggest gaps: smooth earth/crater shapes, isolated compositions, repetitive material wear and roof forms.
  GTX 1050 frame timing remains unverified; captures use an RTX 4070 Laptop GPU. No Sim or github-test1 files changed.
- Decisions that shape everything after this: **3,000 units maximum** (2026-09-20; the stress test runs at that count);
  Windows x64 only; online multiplayer deferred until after Mission 3;
  two developers, one can do art; the M1.5 fun gate still decides whether the design holds.
- Unity tooling on the workstation: Unity CLI 1.0.0-beta, the `unity@unity-agent-plugin` Claude Code plugin, and the
  Unity MCP server registered for Claude Code (`unity mcp configure claude-code`).

## 3. Next steps in order (docs/11 §5)
1. ~~M1~~ done.
2. **M1.5 fun gate**: code done. Playtest with both developers against the questions in docs/11 (is trench-to-trench
   assault better in 3D, do 2,000 units matter, is the crossing too long). Nothing below is scheduled until it passes.
3. M2 look, started 2026-09-21 on the owner's word: **B3 renderer done** (`VATRenderer` + `ProceduralSoldier`
   placeholder, F4 switches back to capsules; combat at 2,770 units costs 1.57 ms/tick). `VATBaker` done for the one rig we have
   (`Art/Characters/CrouchedRun.fbx`, Mixamo, 881 vertices, one clip; menu TW/VAT/Bake Infantry writes
   `Resources/Units/InfantryVat*`; the clip drives walk/sprint/crouch, its widest stride is the standing pose, prone
   and deaths are posed in code from the bind T-pose; helmet brim and rifle are added boxes). **Reactive battlefield done** (2026-09-21, plan in the commit log): `BattlefieldGenerator` builds the map
   from `BattlefieldParams` (seed, forest, shelling, mud, river, water level); `MapData` has a water table, props and a
   cover grid; `DeformationSystem` is the only thing that edits the map (craters that fill below the water table, trees
   breaking to stumps, wrecks, wire breaches); `GreyboxTerrainView` is chunked with code-made ground colours and a water
   sheet, `BattlefieldProps` draws props, wire and the trench kit with placeholder meshes. `SimHost.GeneratedBattlefield`
   switches back to the flat playtest map. Not done from that plan: GPU-displaced terrain, slow crater fill, camouflage
   netting, debris bursts. Still open in M2: more
   clips (idle, fire, prone, death) and a German variant, B6 UI in UI Toolkit (the IMGUI `BattleHud` is the layout to port), B2 terrain mesh + trench kit,
   C2 infantry → M3 Mission 1 → M4 Mission 2 → M5 Mission 3 + perf → M6
   online (if required) → M7 meta.

Every stub carries a `// Phase: <id>` header. Never change the three contracts in `docs/02-contracts.md` without
bumping `ReplayRecorder.FormatVersion` and logging it in that file's change log.

## 4. Before every commit
```powershell
cd trench-warfare-3d
python validate.py
unity test . --mode EditMode --timeout 600
unity test . --mode PlayMode --timeout 600 -- -nographics
```
Commit `.meta` files with their assets and `Packages/packages-lock.json` with `manifest.json`. Push to the same branch.

## 5. Prompt for the next agent
> Continue the Trench Warfare 3D project on branch `claude/trench-warfare-2d-3d-plan-idt7lf`. Read `HANDOFF.md`,
> then `docs/11-plan-review.md`, then `docs/01-phases-and-dependencies.md` for the systems catalog. Work M1 then the
> M1.5 fun gate as listed in HANDOFF §3, keeping the determinism rules in `docs/03` and the frozen contracts in
> `docs/02`. Use the `unity-cli` skill for tests and editor control; run `validate.py` and `unity test` before each
> commit and push to the same branch.
