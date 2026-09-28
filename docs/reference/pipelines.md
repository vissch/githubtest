# Asset pipelines and Tools/

How art gets from the owner's source files into `trench-warfare-3d/Assets/_Project/Resources/`. Every script lives in
`trench-warfare-3d/Tools/` and is run from `trench-warfare-3d/`. The editor-driving tools (`tw`, `flameshots`,
`editor_lock.py`, `health.py`, `shotstats.py`, `perfcmp.py`, `codemap.py`, `selftest.py`, `port_split.py`) are in
`workflow.md`. `tracks.py` draws where the men of a test battle walked (`tasks.md`, Movement).

**The source files are not in the repo.** They sit in the owner's Downloads on the workstation, or on their Google
Drive. A pipeline that needs one says so. If it is missing, ask the owner. Do not guess a substitute.

**Parameters the last run used were not recorded** for the Blender splits. Where a table below says UNRECORDED,
re-derive the value from the current output (`houses.json`, the FBX sizes) before regenerating. A fresh run with
defaults will not reproduce what ships.

## Blender splits (Tripo sheets → one FBX per part)

All four run headless in Blender 5.0: `"C:\Program Files\Blender Foundation\Blender 5.0\blender.exe" -b --factory-startup -P <script> -- <args>`.

| Script | Input (owner's, not in repo) | Output | Usage and flags |
|---|---|---|---|
| `envsplit.py` | `Downloads\env sets\` Tripo sheets | `Resources/Env/<Set>/<Prop>.fbx` + `<Set>.jpg` | `-- <set key> <fbx> <outdir> <renderdir>`. `TW_TURNS` = JSON extra yaw per prop. |
| `housesplit.py` | village houses, watchtower, ruins sheets | `Resources/Env/<Set>/Chunks/*.fbx` + `houses.json` | `-- <fbx> <outdir> <renderdir>`. Flags: `TW_SCALE` (sheet units to metres), `TW_CUT` (chunk edge, m), `TW_SMALL`, `TW_TEX` (basecolour, reads stone vs timber), `TW_PIVOT` (house or chunk), `TW_NAMES`, `TW_TURNS`, `TW_LOOSE=1` (welded sheet), `TW_ONE=1` + `TW_KEEP=1` (slice one kit prop, keeps pivot and facing), `TW_FLOORS=1` (cut along storeys), `TW_STOREY`, `TW_BAND`, `TW_MINTRIS`, `TW_OLDCAPS=1` (caps take the first wall loop's UVs, the way before 2026-09-27), `TW_CAPLOG=1`. Values used per set: UNRECORDED except Houses (`TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4`) and the kit props (Biplane `TW_CUT=1.9`). |
| `tanksplit.py` | `Downloads\cartoon+tank+3d+model.zip` (full), `Downloads\tank+3d+model.zip` (far) | `Resources/Vehicles/<Maw,Tusk>/<Tank>_LOD0,1.fbx` + `TankAtlas_LOD0,1.jpg` | `-- <full.fbx> <far.fbx> <outdir> <renderdir>` |
| `crabsplit.py` | the walker sheets (`Downloads\robotic crab 3d model.zip` and five more) | `Resources/Vehicles/<Walker>/` and `crabs.json` (each part's pivot and socket's place) | `-- <sheet...> <outdir> <renderdir>`. Blender's export moves every node two levels under the body, which sits off the root's origin (0.07 m on the Pavise to 4.4 m on the Cutter), so `Editor/TankImport.cs` puts each node back at its `crabs.json` place (`Editor/CrabManifest.cs`, 2026-09-29); a re-run needs no other step. |
| `tank3split.py` | a three-LOD Tripo tank (`Downloads\tank+3d+model.zip` = LOD0, `(1)` = LOD1, `(2)` = LOD2, 2026-09-26) | `Playground/Art/Tanks/<Name>/<Name>_LOD0..2.fbx`, a base-colour jpg per LOD, `tank3.json` | `-- <Name> <lod0.fbx> <lod1.fbx> <lod2.fbx> <outdir> <renderdir>`; `TW_SCALE` metres per model unit (6.6). The SAME named parts at every LOD: loose islands go whole to the region holding most of their area, the hull body is cut by fixed planes (turret ring, casemate plates) and the cuts capped with soot. Fails if a part is empty at any LOD. |
| `frogrig.py` | a three-LOD Tripo figure in T-pose (`Downloads\frog+warrior+3d+model (1).zip` = LOD0, `frog+knight` = LOD1, `frog+warrior` = LOD2) | `Playground/Art/Units/<Name>/<Name>.fbx` (one armature, four skinned LODs), a base-colour jpg per LOD, `frogrig.json` | `-- <lod0.fbx> <lod1.fbx> <lod2.fbx> <out.fbx> <renderdir>`; `TW_HEIGHT` (1.78 m), `TW_LOD3_TRIS` (220). Mixamo bone names; LOD0 bone heat + geometric fallback; accessories ride rigidly; LOD1-3 weights TRANSFERRED from LOD0; LOD2 18 bones x2, LOD3 (decimated LOD2) 13 bones x2. Writes pose renders to check every LOD bends alike. |
| `jeepsplit.py` | a Tripo wheeled vehicle and, optionally, Tripo's lower model of it (`Downloads\ambulance jeep 3d model.zip` 6,236 tris + `green toy jeep 3d model.zip` 880, 2026-09-27) | `Playground/Art/Tanks/<Name>/` like `tank3split.py` (Mercy) | `-- <Name> <lod0.fbx> [<lower.fbx>] <outdir> <renderdir>`; `TW_SCALE` (4.6), `TW_LOD2=tripo|derive` (tripo when a lower model is given), `TW_LOD2_TRIS`, `TW_SYM`. Wheels, lamps, stack, rear doors and fittings by region; the welded body cut into chassis, bonnet lid, cab and the box's two sides, each cut outline filled (not a fan). LOD1 derived from LOD0; LOD2 Tripo's own split by the same rules (a part it welded into its body is carved from the box that part fills at LOD0), because LOD0 decimated to 13-20 % tore into spikes. |
| `mechsplit.py` | a Tripo two-legged walker at up to three LODs (`Downloads\steampunk+frog+robot` 9,738 tris, `mecha frog` 3,702, `mech+robot` 1,008, 2026-09-27) | `Playground/Art/Tanks/<Name>/` like `tank3split.py`, with `"walker": true` and the parts' parents (Croaker) | `-- <Name> <lod0.fbx> [<lod1.fbx> [<lod2.fbx>]] <outdir> <renderdir>`; `TW_SCALE` (6.6), `TW_LOD2=derive|tripo`. Loose pieces sorted into Hull, Turret > Gun, Claw > Jaw, Thigh > Shin > Foot by where their centres sit; pivots at hip, knee, ankle, shoulder, wrist; LOD1/2 derived part by part to Tripo's counts. The playground walks it (`Runtime/WalkerDrive.cs`). `TW_KIND=hover` a hovercraft (Hull, Pods, FanRing > Fan, Engine, Turret > Gun; Skimmer); `TW_KIND=halftrack` a half-track rocket truck (Hull, Wheel_L/R, Turret: the yoke > Gun: the rocket box and tubes, `Socket_Tube00..15` at the mouths; Salvo, 2026-09-28); `TW_TURN` degrees to turn LOD0 first; `TW_LOD2_TRIS` overrides Tripo's LOD2 count (the Croaker is built with 1450); `TW_PIECE_TRIS` a floor of triangles per loose piece at a derived LOD (0, off); `TW_KEEP=Part:share,...` a floor of a part's LOD0 triangles at a derived LOD (round parts: docs/22, 2026-09-28). **`TW_BATTLE=1`** writes the battle's form instead: `Resources/Vehicles/<Name>/<Name>_LOD0,1.fbx` (parts nested under the Hull, sockets as empties; LOD1 is the far LOD at the LOD2 budget), `Resources/Vehicles/<Name>Atlas.jpg`, and into the render dir `<Name>_battle.json` and `<Name>_portrait.png`. |

The battle's copies, as run 2026-09-28 (the sources unzipped to short paths first, each FBX renamed m.fbx with its m.fbm folder beside it):

| Machine | Sources | Command (from `trench-warfare-3d/`, the Blender line as above) | Tris LOD0 / LOD1 |
|---|---|---|---|
| Skimmer | `C:\tw\s0` = `military tank 3d model.zip`, `s1` = `(2)`, `s2` = `(1)` | `TW_KIND=hover TW_TURN=90 TW_LOD2_TRIS=1400 TW_KEEP=FanRing:0.5,Fan:0.35,Hull:0.3 TW_BATTLE=1 ... -- Skimmer C:/tw/s0/m.fbx C:/tw/s1/m.fbx C:/tw/s2/m.fbx Assets/_Project/Resources/Vehicles/Skimmer <renderdir>` (`TW_SCALE` 7.0, the default) | 7,098 / 1,840 |
| Salvo | `C:\tw\z4` = `military+vehicle+3d+model.zip`, `z5` = `military+vehicle+3d+model (1).zip`, `z6` = `rocket+launcher+vehicle+3d+model.zip` | `TW_KIND=halftrack TW_BATTLE=1 TW_PIECE_TRIS=12 TW_KEEP=Wheel_L:0.4,Wheel_R:0.4,Turret:0.35,Gun:0.35,Hull:0.3 ... -- Salvo C:/tw/z4/m.fbx C:/tw/z5/m.fbx C:/tw/z6/m.fbx Assets/_Project/Resources/Vehicles/Salvo <renderdir>` (`TW_SCALE` 8.0, the default) | 7,863 / 2,539 |

| Croaker | `steampunk+frog+robot+3d+model.zip`, `mecha frog 3d model.zip`, `mech+robot+3d+model.zip` | `TW_BATTLE=1 TW_LOD2_TRIS=1443 ... mechsplit.py -- Croaker <lod0.fbx> <lod1.fbx> <lod2.fbx> Assets/_Project/Resources/Vehicles/Croaker <renderdir>` | 9,738 / 1,435 |
| Hopper | `green+tank+3d+model (1).zip`, `green+cartoon+tank+3d+model.zip`, `green+tank+3d+model.zip` | `TW_BATTLE=1 TW_KIND=flyer ... mechsplit.py -- Hopper <lod0.fbx> <lod1.fbx> <lod2.fbx> Assets/_Project/Resources/Vehicles/Hopper <renderdir>` | 5,903 / 985 |
| Bullfrog | `frog+mecha+3d+model.zip` (one LOD; the far one derived) | `TW_BATTLE=1 TW_KIND=gatling TW_LOD2_TRIS=1200 ... mechsplit.py -- Bullfrog <lod0.fbx> Assets/_Project/Resources/Vehicles/Bullfrog <renderdir>`, then `portraitcut.py` (940 tore the far model: 12.2 % of its edges folded, the test allows 12 %), then `python Tools/atlasgrade.py Assets/_Project/Resources/Vehicles/BullfrogAtlas.jpg` (its pale steel paint, median luminance 0.37 against the Brute's 0.24, down to 0.24 and its greys toward teal: a re-split rewrites the atlas, so grade it again) | 4,948 / 1,193 |
| Brute | `tank+3d+model.zip`, `(1)`, `(2)`, each unzipped to a folder of its own, the FBX renamed m.fbx, with the four textures of its .fbm folder (the names ending _0_0 to _0_3) beside it as m_tex0_0 to m_tex0_3 (.jpg) | `TW_BATTLE=1 ... tank3split.py -- Brute <b0>/m.fbx <b1>/m.fbx <b2>/m.fbx Assets/_Project/Resources/Vehicles/Brute <renderdir>` | 7,861 / 1,166 |
| Mercy | `ambulance jeep 3d model.zip`, `green toy jeep 3d model.zip` | `TW_BATTLE=1 ... jeepsplit.py -- Mercy <lod0.fbx> <lower.fbx> Assets/_Project/Resources/Vehicles/Mercy <renderdir>` | 6,984 / 1,214 |

`tank3split.py` and `jeepsplit.py` write the battle's form through `Tools/battleform.py` (imported, not run by
itself): the parts nested, the sockets under their parts, two LODs. The far model of these two is Tripo's own low
sculpt on an atlas of its own, written beside the near one with `_LOD1` after `Atlas` in its name, which
`TankRenderer` loads for the far level when it is there (`TankRenderer.FarAtlasSuffix`). Derived from LOD0 to a sixth
of its triangles (`TW_DERIVE=12` for the tank, `TW_LOD2=derive` for the jeep: the far model then wears LOD0's atlas)
both tore into shards: seen in Play on 2026-09-28 with the far models drawn close, and held since by
ProvingGroundModelTests (a model's FOLDS, edges whose two triangles face away from each other, were 21 % and 16 % of
their edge length; every model that looks whole is under 9 %). Both low sculpts are painted with LOD0's colours
(a Cycles bake, `TW_REBAKE=1`, the default; 0 keeps the sculpt's own paint), but the tank's tracks, which keep their
own: the faces that close a track's open back have UVs across half the atlas, and baked, they were painted over the
tracks' own islands. The Croaker's and the Hopper's far models are derived by `mechsplit.py` and are whole.
Blender exits 0 when the script it ran raised: read the log for `DONE`. In the battle's form the root part stands on
the origin (`mechsplit.py` moves the Hull's pivot there): with the Croaker's Hull pivot at its pelvis every part under
it came out of Unity displaced. `Editor/TankImport.cs` imports these four by the full rule for nested nodes
(`TankImport.ParentAware`), which puts a part three levels down (a foot under a shin under a thigh) where the manifest
has it; the older machines keep the old rule.

Then `python Tools/portraitcut.py <renderdir>/<Name>_portrait.png <Name>` cuts the HUD's two pictures from the render.
Re-importing one of these FBXs into Blender shows the nested parts out of place: that is the exporter's node turn
that `Editor/TankImport.cs` puts right in Unity (the Tusk's files look the same), not a fault in the file.
| `mechsplit.py` | a Tripo two-legged walker at up to three LODs (`Downloads\steampunk+frog+robot` 9,738 tris, `mecha frog` 3,702, `mech+robot` 1,008, 2026-09-27) | `Playground/Art/Tanks/<Name>/` like `tank3split.py`, with `"walker": true` and the parts' parents (Croaker) | `-- <Name> <lod0.fbx> [<lod1.fbx> [<lod2.fbx>]] <outdir> <renderdir>`; `TW_SCALE` (6.6), `TW_LOD2=derive|tripo`. Loose pieces sorted into Hull, Turret > Gun, Claw > Jaw, Thigh > Shin > Foot by where their centres sit; pivots at hip, knee, ankle, shoulder, wrist; LOD1/2 derived part by part to Tripo's counts. The playground walks it (`Runtime/WalkerDrive.cs`). `TW_KIND=hover` a hovercraft (Hull, Pods, FanRing > Fan, Engine, Turret > Gun; Skimmer); `TW_TURN` degrees to turn LOD0 first; `TW_LOD2_TRIS` overrides Tripo's LOD2 count (the Croaker is built with 1450). `TW_KIND=gatling` a four-legged toad with two gatlings (`Downloads\frog+mecha+3d+model` 4,953 tris; Bullfrog, `TW_LOD2_TRIS=940`): Tripo's ground plate dropped, Hull (body and legs, one welded piece), Turret > Gun_L/R > Barrels_L/R (sorted by distance from each gun's barrel axis, pivoted on it); `"hopper": true`, the playground hops it (`Runtime/HopDrive.cs`) and spins the barrels in bursts (`VehicleRig.Gatling`). |
| `legrig.py` | a hull whose legs Tripo welded into it, as each LOD's vertices and triangles dumped from the Playground (`dumphull <dir>`) | `Playground/Art/Tanks/<Name>/<Name>_legs.json`: leg bones and up to four weights a welded vertex per LOD (Bullfrog) | `blender -b --factory-startup -P legrig.py -- <Name> <out.json> <hull0.json> [<hull1.json> ...]`; the joints per machine in `RIGS` (body frame, left legs, mirrored). Blender bone heat on the welded mesh, body bones hold the torso, leg weights fade out towards the midline. The playground skins the legs (`Runtime/LegRig.cs`) and `HopDrive` poses them. Rerun after any re-split (a mismatch leaves the legs still and fails the hopper test). |

Traps, all silent:
- **Axis.** Blender's FBX export with `bake_space_transform` maps Blender -Y to Unity -Z whatever `axis_forward`
  says. The scripts turn every part 180° first. Check facing by where the barrel vertices sit, not by bounds.
- **MAX_PATH.** Tripo texture paths exceed Windows' limit. Copy each sheet to a short path first.
- **Parenting.** Every walker/tank part must be parented to the Body, or `TankModel` never draws it.
- **Re-slicing a kit prop** needs `TW_KEEP=1`, or it comes out turned 180°.
- **Import settings.** `Editor/TankImport.cs` and `Editor/EnvKitImport.cs` set import rules by folder. Vehicle and
  chunk meshes must stay Read/Write enabled; setting it by hand does not stick, the postprocessor resets it.

## Battle lineup

| Script | Does | Usage |
|---|---|---|
| `battlelineup.py` | Blender: the battle FBXs as TankRenderer builds them (the exporter's node turn undone as `Editor/TankImport.cs` does, normals smoothed at 55 degrees as Unity recalculates them), the Maw at 1.7x, the Pincer at 2.5x, the named machines at 1x and a 1.8 m man in a lineup; and each named machine's LOD0->LOD1 pop from four sides (worst silhouette IoU and block colour, full frame and at 85 px), written as pop.json | `blender -b --factory-startup -P Tools/battlelineup.py -- <outdir> Skimmer Salvo` (any machine with a `<Name>Atlas`: the walkers are the reference); `TW_ENGINE=workbench` when memory is short (not comparable). The 2026-09-28 loop: docs/22 |

## Unit portraits

| Script | Does | Usage |
|---|---|---|
| `portraitcut.py` | Cuts a machine's portrait render (`mechsplit.py TW_BATTLE=1`) into `UI/Resources/UnitArt/<Name>.png` (512 px) and `UI/Skin/Portraits/<Name>.png` (256 px): cropped, centred, an ink outline | `python Tools/portraitcut.py <render.png> <Name>` |
| `atlasgrade.py` | Grades a machine's atlas for the night field: a gamma, a saturation lift and a tint on its near-greys; prints the median luminance before and after | `python Tools/atlasgrade.py <atlas.jpg> [--gamma 1.4] [--sat 1.25] [--tint r,g,b] [--grey 0.18]` |

## Asset playground (docs/22)

| Script | Does | Usage |
|---|---|---|
| `playground/pg.sh` | Drives the playground in this checkout's editor (in Play in `Playground.unity`): queue commands, take stills with a JSON report | `bash Tools/playground/pg.sh do "vehicle.compare; seq"`, `... shot NAME [W H]`, `... report` |
| `playground/round.sh` | The critic loop's fixed shot list: vehicle LODs through a destruction, figures at four LODs, a squad to 300 m, the mixed scene on grid and mud, a shelled building, the LOD-pop measurements | `bash Tools/playground/round.sh r1` (stills in `Captures/playground/`) |
| `playground/sheet.py` | Contact sheets of a round, each still labelled from its JSON (LOD, cost, stage), and a stats table | `python Tools/playground/sheet.py r1` |

## Environment textures and atlas

| Script | Does | Usage |
|---|---|---|
| `envgrade.py` | Grades the Tripo set textures into the field's muted range | `python Tools/envgrade.py "<folder with the unzipped sets>"` |
| `envatlas.py` | Packs every set's texture into `Resources/Env/EnvAtlas.jpg` (4x4 grid of 1024 px cells) | `python Tools/envatlas.py` |
| `looks.py` | Rescales the prop looks in `Resources/Layouts/Battlefield1917.asset` to the soldier (docs/21 phase 1) and the hand edits with them | `python Tools/looks.py --check` prints, `--apply` writes |

`envatlas.py`'s `SETS`, `COLS`, `ROWS` must equal `BattlefieldKit.EnvSets`, `EnvCols`, `EnvRows` in
`Presentation/Terrain/BattlefieldKit.cs`. EnvAtlasTests checks both the grid and the set order. A new set appends to
both lists.

## Infantry figures and animation clips

- **Bake:** menu **TW/VAT/Bake Infantry** (`Editor/VATBaker.cs`), about a minute. It reads `Art/Characters/*.fbx` and
  the clips named in `Editor/InfantryClipTable.cs`, and writes one gzip'd `.bytes` atlas per figure to
  `Resources/Units/`. Each rebake adds ~19 MB to git history.
- After a bake that changes a job struct, a stale `Library/BurstCache` throws inside Burst jobs. Close the editor and delete it.

| Script | Does | Usage |
|---|---|---|
| `fbxscan.py` | Reads Mixamo FBX without libraries; writes a CSV of length, loop, root motion | `python Tools/fbxscan.py "<folder of .fbx>" out.csv` |
| `animforge.py` | Library: edit Mixamo clips as bone curves (reverse, retime, layer, IK) and write FBX | imported by `make_missing_clips.py` |
| `make_missing_clips.py` | Makes the 15 clips the Mixamo download lacks | `python Tools/make_missing_clips.py "<Mixamo folder>"` writes `<folder>/Made/` |
| `clipcheck.py` | Foot skate, root drift, floor and helmet clipping of the made clips | `python Tools/clipcheck.py "<folder>/Made"` |

Source clips: the owner's `Downloads\mixamo animations\`. Classification: `docs/reference/animation-clips.md`.

## VFX flipbooks

| Script | Does | Usage |
|---|---|---|
| `firebooks.py` | Cuts the flamethrower's greyscale flipbooks into `Resources/VFX/` from the owner's generated pack | `python Tools/firebooks.py [packdir] [outdir]`, pack default `G:/My Drive/vfx/sheets` |

The other books in `Resources/VFX/` come from Asset Store packs (SrRubfish, Hun0FX). They may ship in the game but
must not be redistributed on their own.

How the owner's pack is read (32 sheets, 8x4 cells, 12 fps, named `<colour>_<kind>_8x4_12fps_<n>f.png`):
- **The palette is irrelevant.** `firebooks.py` keeps value and alpha only and `TW/Flipbook` recolours from value,
  so every sheet is a distinct drawing whatever its colour. Pick sheets by shape, not hue.
- **Value mode is per sheet.** Orange sheets use luma. Saturated sheets use lightness, `(max + min) / 2`; max-channel
  flattens a hue's own shading.
- **Levels and bands are measured per book** off its own output. Whenever the levels move, re-measure the bands; a
  computed core cut of 1.00 means the band is switched off.
- **Choosing a sheet:** score several measures at once (holes as a share of the filled silhouette, solidity =
  alpha area / box area, centroid drift across frames, whether the left edge stays rooted). Each alone can be cheated.
- **Regenerating must not move signed-off books.** The script hashes FireBall, FireColumn and FireBurst only, and
  prints `UNCHANGED <name>.png` or `*** CHANGED *** <name>.png` for each. The other books are not checked: compare
  them yourself (`git diff --stat` on the PNGs). It writes no `.meta`; Unity makes one when it imports a new PNG.
- A capture of the result goes through `Tools/flameshots` and `Tools/flamecheck.py` (`workflow.md`, section 6).

## UI art spec

| Script | Does | Usage |
|---|---|---|
| `gen_artspec.py` | Writes `docs/17-ui-art-spec.md` from `UI/Skin/SkinSpec.cs` | `python Tools/gen_artspec.py Assets/_Project/UI/Skin/SkinSpec.cs ../docs/17-ui-art-spec.md` |

Placeholder sprites are painted by `Editor/UI/UiSkinGenerator.cs`, which never overwrites an artist's file.

## Eval scripts outside Assets

`trench-warfare-3d/AgentScripts/` holds `EnvironmentAudit.cs` and `VisualCapture.cs`. Unity does not compile them.
They are bodies for `tw eval` / `eval_file`, kept for reference. `VisualCapture` is the old capture path; use
`CaptureRig` instead (`workflow.md`).

## Two-station agent pipeline

Work split between the laptop and the desktop, one stage at a time per item, with staleness worked out from the
repo. The board, the states and the commands are in `docs/reference/stations.md`.

| Script | Does | Usage |
|---|---|---|
| `pipeline/pipeline.py` | Job board and stage states (DONE, RECHECK, STALE, IN_PROGRESS, READY, BLOCKED) from `tw3d-board` | `python Tools/pipeline/pipeline.py status` |
| `pipeline/test_pipeline.py` | Its tests, on throwaway git repos | `python Tools/pipeline/test_pipeline.py` |
| `pipeline/run_detached.py` | Runs a gate, bench, sweep or batch detached from the session: pids with start times, a heartbeat from log growth, a timeout that stops only its own tree, a commit-headroom floor. Runs live in `%LOCALAPPDATA%/TrenchWarfare/runs`, never in a checkout | `python Tools/pipeline/run_detached.py start <name> --timeout 3600 --min-headroom-gb 10 -- <cmd>` then `status <name>` |
