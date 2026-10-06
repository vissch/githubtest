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
| `crabsplit.py` | the walker sheets (`Downloads\robotic crab 3d model.zip` and five more) | `Resources/Vehicles/<Walker>/` | `-- <sheet...> <outdir> <renderdir>` |
| `tank3split.py` | a three-LOD Tripo tank (`Downloads\tank+3d+model.zip` = LOD0, `(1)` = LOD1, `(2)` = LOD2, 2026-09-26) | `Playground/Art/Tanks/<Name>/<Name>_LOD0..2.fbx`, a base-colour jpg per LOD, `tank3.json` | `-- <Name> <lod0.fbx> <lod1.fbx> <lod2.fbx> <outdir> <renderdir>`; `TW_SCALE` metres per model unit (6.6). The SAME named parts at every LOD: loose islands go whole to the region holding most of their area, the hull body is cut by fixed planes (turret ring, casemate plates) and the cuts capped with soot. Fails if a part is empty at any LOD. |
| `frogrig.py` | a three-LOD Tripo figure in T-pose (`Downloads\frog+warrior+3d+model (1).zip` = LOD0, `frog+knight` = LOD1, `frog+warrior` = LOD2) | `Playground/Art/Units/<Name>/<Name>.fbx` (one armature, four skinned LODs), a base-colour jpg per LOD, `frogrig.json` | `-- <lod0.fbx> <lod1.fbx> <lod2.fbx> <out.fbx> <renderdir>`; `TW_HEIGHT` (1.78 m), `TW_LOD3_TRIS` (220). Mixamo bone names; LOD0 bone heat + geometric fallback; accessories ride rigidly; LOD1-3 weights TRANSFERRED from LOD0; LOD2 18 bones x2, LOD3 (decimated LOD2) 13 bones x2. Writes pose renders to check every LOD bends alike. |
| `jeepsplit.py` | a Tripo wheeled vehicle and, optionally, Tripo's lower model of it (`Downloads\ambulance jeep 3d model.zip` 6,236 tris + `green toy jeep 3d model.zip` 880, 2026-09-27) | `Playground/Art/Tanks/<Name>/` like `tank3split.py` (Mercy) | `-- <Name> <lod0.fbx> [<lower.fbx>] <outdir> <renderdir>`; `TW_SCALE` (4.6), `TW_LOD2=tripo|derive` (tripo when a lower model is given), `TW_LOD2_TRIS`, `TW_SYM`. Wheels, lamps, stack, rear doors and fittings by region; the welded body cut into chassis, bonnet lid, cab and the box's two sides, each cut outline filled (not a fan). LOD1 derived from LOD0; LOD2 Tripo's own split by the same rules (a part it welded into its body is carved from the box that part fills at LOD0), because LOD0 decimated to 13-20 % tore into spikes. |
| `mechsplit.py` | a Tripo two-legged walker at up to three LODs (`Downloads\steampunk+frog+robot` 9,738 tris, `mecha frog` 3,702, `mech+robot` 1,008, 2026-09-27) | `Playground/Art/Tanks/<Name>/` like `tank3split.py`, with `"walker": true` and the parts' parents (Croaker) | `-- <Name> <lod0.fbx> [<lod1.fbx> [<lod2.fbx>]] <outdir> <renderdir>`; `TW_SCALE` (6.6), `TW_LOD2=derive|tripo`. Loose pieces sorted into Hull, Turret > Gun, Claw > Jaw, Thigh > Shin > Foot by where their centres sit; pivots at hip, knee, ankle, shoulder, wrist; LOD1/2 derived part by part to Tripo's counts. The playground walks it (`Runtime/WalkerDrive.cs`). `TW_KIND=hover` a hovercraft (Hull, Pods, FanRing > Fan, Engine, Turret > Gun; Skimmer); `TW_KIND=halftrack` a half-track rocket truck (Hull, Wheel_L/R, Turret: the yoke > Gun: the rocket box and tubes, `Socket_Tube00..15` at the mouths; Salvo, 2026-09-28); `TW_TURN` degrees to turn LOD0 first; `TW_LOD2_TRIS` overrides Tripo's LOD2 count (the Croaker is built with 1450); `TW_PIECE_TRIS` a floor of triangles per loose piece at a derived LOD (0, off); `TW_KEEP=Part:share,...` a floor of a part's LOD0 triangles at a derived LOD (round parts: docs/22, 2026-09-28). **`TW_BATTLE=1`** writes the battle's form instead: `Resources/Vehicles/<Name>/<Name>_LOD0,1.fbx` (parts nested under the Hull, sockets as empties; LOD1 is the far LOD at the LOD2 budget), `Resources/Vehicles/<Name>Atlas.jpg`, and into the render dir `<Name>_battle.json` and `<Name>_portrait.png`. |

The battle's copies, as run 2026-09-28 (the sources unzipped to short paths first, each FBX renamed m.fbx with its m.fbm folder beside it):

| Machine | Sources | Command (from `trench-warfare-3d/`, the Blender line as above) | Tris LOD0 / LOD1 |
|---|---|---|---|
| Skimmer | `C:\tw\s0` = `military tank 3d model.zip`, `s1` = `(2)`, `s2` = `(1)` | `TW_KIND=hover TW_TURN=90 TW_LOD2_TRIS=1400 TW_KEEP=FanRing:0.5,Fan:0.35,Hull:0.3 TW_BATTLE=1 ... -- Skimmer C:/tw/s0/m.fbx C:/tw/s1/m.fbx C:/tw/s2/m.fbx Assets/_Project/Resources/Vehicles/Skimmer <renderdir>` (`TW_SCALE` 7.0, the default) | 7,098 / 1,840 |
| Salvo | `C:\tw\z4` = `military+vehicle+3d+model.zip`, `z5` = `military+vehicle+3d+model (1).zip`, `z6` = `rocket+launcher+vehicle+3d+model.zip` | `TW_KIND=halftrack TW_BATTLE=1 TW_PIECE_TRIS=12 TW_KEEP=Wheel_L:0.4,Wheel_R:0.4,Turret:0.35,Gun:0.35,Hull:0.3 ... -- Salvo C:/tw/z4/m.fbx C:/tw/z5/m.fbx C:/tw/z6/m.fbx Assets/_Project/Resources/Vehicles/Salvo <renderdir>` (`TW_SCALE` 8.0, the default) | 7,863 / 2,539 |

Then `python Tools/portraitcut.py <renderdir>/<Name>_portrait.png <Name>` cuts the HUD's two pictures from the render.
Re-importing one of these FBXs into Blender shows the nested parts out of place: that is the exporter's node turn
that `Editor/TankImport.cs` puts right in Unity (the Tusk's files look the same), not a fault in the file.

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

### Outside references for effect drawing

Places to study a look before drawing it, and to hand a critic beside a capture. Every link was opened on 2026-10-06
unless marked. None of it is in the repo or the pack. A studio's clip is for study and for the critic only: the sheet
generator copies its reference closely, so its `<Video 1>` stays a clip from `refs360` unless the owner says otherwise.

| Source | What it is | Use it for |
|---|---|---|
| Sakugabooru, `https://www.sakugabooru.com/post?tags=<tag>` | Hand-drawn animation clips, stepped frame by frame, tagged by effect and by animator. Free | The nearest match to the owner's look (cel flames, two-tone smoke): shapes and timing of bursts, smoke, debris, water |
| Refs library - 1500 vfx, `https://realtimevfx.com/t/refs-library-1500-vfx/25346` | 1,500 tagged clips of effects from shipped games, each with its source link; a zip on Google Drive made for the Eagle app. Free. The download itself was not opened | How other games solve one effect, by element and camera |
| Ten tips from the League of Legends VFX style guide, `https://www.vfxapprentice.com/blog/10-league-of-legends-vfx-design-tips` | Rules for value, saturation, shape and short timing seen from a raised camera. Riot's own article and PDF are gone from their site | A readability rubric for `tw-critic` |
| Company of Heroes explosions taken apart, `http://valdism.blogspot.com/2014/02/vfx-showcase-company-of-heroes.html` | Screenshots and notes on artillery bursts in an RTS camera | J01 and the shell looks |
| Duc Vu, "2D FX Explosion Tutorial: Basic to Advanced", `https://ducvu.gumroad.com/l/2d-fx-smoke-explosion-tutorial` | A tutorial by the artist of the 24 `refs360` clips. Price not read | How the reference style is built |
| Stylised effects threads, `https://realtimevfx.com/t/inspirational-resources-for-stylized-effects/625` and `https://realtimevfx.com/t/where-to-find-good-vfx-references/2294` | Link lists: the Wind Waker effect analysis (cel smoke), slow-motion and weapons footage of real bursts and dirt | Real behaviour to exaggerate from |
| Gabriel Aguiar, Unique Stylized Explosions Vol. 1, `https://gabrielaguiarprod.itch.io/unique-stylized-explosions-vol1` | 37 stylised explosions for Unity. Not owned | Look reference only |
| War FX (Jean Moreno), `https://assetstore.unity.com/packages/vfx/particles/war-fx-5669` | Free Unity particle pack of grounded military effects | How a shell burst is layered. Study only: the game draws flipbook cards, not particle systems |
| SineVFX Top Down Effects 2.0, `https://assetstore.unity.com/packages/vfx/particles/spells/top-down-effects-2-0-323684` | Paid pack of ground rings and markers that follow the terrain | Look reference for a ground marker on cratered ground |

Sakugabooru tags, with the clip counts read that day: `explosions` 24,020, `smoke` 62,251, `debris` 27,965, `fire`
16,636, `sparks` 15,259, `impact_frames` 13,758, `liquid` 30,605, `lightning` 14,112, `wind` 12,433, `mecha` 17,773,
`missiles` 2,970, `itano_circus` 606. Animator tags: `hideaki_anno`, `mitsuo_iso`, `takashi_hashimoto`,
`yoh_yoshinari`, `yutaka_nakamura`, `ichiro_itano`, `yoshimichi_kameda`.

Where to look first, by moment (`docs/reference/aosa/JUICE.md`). The tags exist; which animator suits which effect is
a judgement nobody has checked against the clips, so look before relying on one.

| Need | Search |
|---|---|
| J01 shell burst, earth column | `hideaki_anno`, `mitsuo_iso`; the Company of Heroes page |
| J02 cook-off, layered fire and smoke | `takashi_hashimoto`, `yoh_yoshinari` |
| J03 volley, hits, flash frames | `yoshimichi_kameda`, `impact_frames`, `sparks` |
| J04 walker mounting a parapet | `mecha` |
| J05 leg blown off, flying chunks | `yutaka_nakamura`, `debris` |
| J07 lightning | `lightning` |
| J08 landing craft, water | `takashi_hashimoto`, `liquid` |
| J09 gas rolling | `smoke`, `wind` |
| Rockets in flight, a salvo | `ichiro_itano`, `itano_circus`, `missiles` |

Looked for and not found (2026-10-06): public effect breakdowns of Company of Heroes 3, Valkyria Chronicles, Iron
Harvest, Arcane's hand-drawn smoke, Hi-Fi Rush's sound words, Foxhole and The Great War: Western Front. For those,
take frames from gameplay video.

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

## The relay

Runs pipeline jobs and queued lane work as a chain of short headless Claude sessions (legs), so no session fills its
context. A script picks the work, starts one leg at a time and checks the result. The contract is
`docs/reference/relay.md`; the `/relay` skill wraps the commands.

| Script | Does | Usage |
|---|---|---|
| `relay/relay.py` | The command line: run, status, stop, add, a leg's close-out, view, proofs | `python Tools/relay/relay.py run --work <work checkout> --dry-run` |
| `relay/runner.py`, `relay/launch.py` | The run loop and its stop rules; starts and owns one leg | through `relay.py run` |
| `relay/relay_hook.py`, `relay/cmdrules.py` | The Claude Code hooks of a leg: the context meter, the tool guards, the compaction block | wired per leg by `relay/relay-hooks.json` |
| `relay/prepush.py` | Git pre-push hook: while a leg holds a checkout, only a forward push of its own lane leaves it | installed by the runner at run start |
| `relay/legcmd.py` | A leg's close-out: the edit gate detached, then commit and push | `python Tools/relay/relay.py leg gate start` |
| `relay/sources/` | Where work comes from: `pipeline.py` (board jobs), `lane.py` (queued lane work) | one file per source |
| `relay/ledger.py` | The day's spend, added up from the leg records on the board; the usual cost of a leg | `python Tools/relay/relay.py budget` |
| `relay/test_relay.py` | Its tests, with a stand-in for Claude (`relay/fake_claude.py`) | `python Tools/relay/test_relay.py` |
| `relay/test_ledger.py` | The tests of the day's spend | `python Tools/relay/test_ledger.py` |
| `relay/usage.py` | The readings of the plan's weekly limit a source hands in, and what a leg used of the week. Calls nobody | `python Tools/relay/relay.py usage` |
| `relay/test_usage.py` | The tests of those readings | `python Tools/relay/test_usage.py` |
| `relay/day.py` | The master's one screen: budget, run, queue in its order, what needs the owner. Reads only | `python Tools/relay/relay.py day` |
| `relay/test_day.py` | The tests of that screen and of the queue's order (`relay.py prio`) | `python Tools/relay/test_day.py` |
