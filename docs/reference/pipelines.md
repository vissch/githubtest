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
| Brute | `tank+3d+model.zip`, `(1)`, `(2)`, each unzipped to a folder of its own, the FBX renamed m.fbx, with the four textures of its .fbm folder (the names ending _0_0 to _0_3) beside it as m_tex0_0 to m_tex0_3 (.jpg) | `TW_BATTLE=1 ... tank3split.py -- Brute <b0>/m.fbx <b1>/m.fbx <b2>/m.fbx Assets/_Project/Resources/Vehicles/Brute <renderdir>` | 7,861 / 1,166 |
| Mercy | `ambulance jeep 3d model.zip`, `green toy jeep 3d model.zip` | `TW_BATTLE=1 ... jeepsplit.py -- Mercy <lod0.fbx> <lower.fbx> Assets/_Project/Resources/Vehicles/Mercy <renderdir>` | 6,984 / 1,214 |

`tank3split.py` and `jeepsplit.py` write the battle's form through `Tools/battleform.py` (imported, not run by
itself): the parts nested, the sockets under their parts, two LODs. The far model of these two is Tripo's own low
sculpt on an atlas of its own, written beside the near one with `_LOD1` after `Atlas` in its name, which
`TankRenderer` loads for the far level when it is there (`TankRenderer.FarAtlasSuffix`). Derived from LOD0 to a sixth
of its triangles (`TW_DERIVE=12` for the tank, `TW_LOD2=derive` for the jeep: the far model then wears LOD0's atlas)
both tore into shards: seen in Play on 2026-09-28 with the far models drawn close, and held since by
ProvingGroundModelTests (a model's FOLDS, edges whose two triangles face away from each other, were 21 % and 16 % of
their edge length; every model that looks whole is under 9 %). The jeep's low sculpt is painted with LOD0's colours
(a Cycles bake, `TW_REBAKE=1`, its default); the tank's keeps its own paint (`TW_REBAKE=0`, its default), because
the faces that close its tracks' open backs have UVs across half the atlas and the bake painted them over the
tracks' own islands. The Croaker's and the Hopper's far models are derived by `mechsplit.py` and are whole.
Blender exits 0 when the script it ran raised: read the log for `DONE`. In the battle's form the root part stands on
the origin (`mechsplit.py` moves the Hull's pivot there): with the Croaker's Hull pivot at its pelvis every part under
it came out of Unity displaced. `Editor/TankImport.cs` imports these four by the full rule for nested nodes
(`TankImport.ParentAware`), which puts a part three levels down (a foot under a shin under a thigh) where the manifest
has it; the older machines keep the old rule.

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

## UI art spec

| Script | Does | Usage |
|---|---|---|
| `gen_artspec.py` | Writes `docs/17-ui-art-spec.md` from `UI/Skin/SkinSpec.cs` | `python Tools/gen_artspec.py Assets/_Project/UI/Skin/SkinSpec.cs ../docs/17-ui-art-spec.md` |

Placeholder sprites are painted by `Editor/UI/UiSkinGenerator.cs`, which never overwrites an artist's file.

## Eval scripts outside Assets

`trench-warfare-3d/AgentScripts/` holds `EnvironmentAudit.cs` and `VisualCapture.cs`. Unity does not compile them.
They are bodies for `tw eval` / `eval_file`, kept for reference. `VisualCapture` is the old capture path; use
`CaptureRig` instead (`workflow.md`).

## Asset board (every character, vehicle and building, and how far each is)

A static site, built from the code, the files, the branches and the docs, that shows each asset's models, a preview,
how far it is integrated, how it meets the effects, which levels it is in, what is in flight on which lane, and every
change to it. It goes to the Drive folder TW3D-pipeline/assets (open index.html there), never into the repo. Five
buckets: final in the battle, in progress, needs a visual (drawn with another unit's model), ready but not used, idea.
The rules for each are at the top of Tools/assetboard/model.py.

| Script | Does | Usage |
|---|---|---|
| `assetboard/build.py` | Builds the site. `--check` only reads and prints the counts. A code table that changed shape stops it with the file and what it expected; a source this machine lacks (Blender, gym runs, the board) is only a warning on the process page | `python Tools/assetboard/build.py` |
| `assetboard/test_assetboard.py` | Its tests: the counts and known assets on the real tree, and the rules on fixtures | `python Tools/assetboard/test_assetboard.py` |
| `assetboard/thumb_blender.py` | The previews, in headless Blender (run by build.py). A building is put together from its chunk files and `houses.json` | not run by hand |
| `assetboard/films.py`, `film_blender.py` | The films an asset's page opens on (run by build.py): every model once round on a turntable, the battle figures playing every clip of their baked atlas (the game's own vertex data, decoded as `VatCodec.cs` does), the buildings drawn apart into their chunks. Renders of the model, not the game. A first build is about 25,000 frames, ten minutes; after that only what changed. `--no-films` keeps the films already made | not run by hand |
| `assetboard/ops.py`, `src_ops.py`, `static/office.js`, `crew.js` | The overview's live half and the branches page (`floor.html`): what needs the owner (the owner queue, next row), who is at work, and an office with a room per open branch (its crew at their desks, its board items, the assets it touches, its last change); branches nobody is in as a strip. Every session, skill, agent and machine is a frog with its own look: a Krea2 still and two Minimax H3 loops (busy, dozing; idle frogs play the dozing loop's eyes-shut frames). The frogs are the owner's art and stay out of git, in the station's board cache (the folder crew under LOCALAPPDATA, TrenchWarfare/assetboard); ops.py copies them in and without them each worker is drawn as a mark. An agent's task is the last thing it was asked, as a short phrase. build.py writes the pages once; `--watch` keeps the reading live | `python Tools/assetboard/ops.py --watch 20` |
| `assetboard/src_queue.py`, `static/queue.js` | The owner queue, the page's "Needs you": everything that waits on the owner, derived from where it is written down. Five groups, a card each, rows of chips: **Decide** (the open questions of `decisions.md` on integration, oldest first; one this checkout has answered and taken out of its own copy, committed or not, is no longer counted and is listed as answered, waiting for its lane to land), **Say land** (a checkout whose green full gate tested its tip), **Approved, not landed** (the owner said land: a file per lane in the board's `approvals` folder, with their words; listed with what holds it up until the lane is in integration), **Ready to take** (the board's ready stages), **Broken** (a red `checks` run on integration, a run before a commit over 300 s, decisions written down only on a lane, and, as one row, the answers he gave on the Decide page that no session has taken up for over two hours: nothing wakes a session when he answers). ops.py writes it to `data/queue.js` and stamps `data/beat.js` on every read, so the header says "read N min ago" and turns red past an hour: a stopped watcher no longer looks like a quiet floor. `--stranded` lists the decisions a lane wrote down that integration lacks (a decision lane is built from it) | `python Tools/assetboard/ops.py --queue`, `python Tools/assetboard/src_queue.py --stranded` |
| `assetboard/static/house.js`, `housedraw.js`, `house.css`, `templates/house.html` | The house (`house.html`): the same reading as the office, drawn as seven rooms, one per kind of work, and every session, skill, agent and machine a frog in the room of what it is doing. Workroom (code, docs, commits), lab (reading, searching, reviews, tests, gates), workshop (builds, Unity, tools, machines), studio (animation, films, art), war room (plans, briefing agents), bunkhouse (resting sessions and whoever on the roster nothing calls, each on the mat of its place on the roster), and the hall, where a session that waits on the owner stands by the notice board. A frog walks when its work changes, along walkways that run on the plan's two axes only (the sprites have the four diagonal walks), stays twelve seconds where it arrived before it follows the next change, and gets up from its mat at once. `house.js` is the plan and the rules, with no page in it; `housedraw.js` draws it on a canvas, the rooms in code, the frogs from the sheets. A room where someone is awake is a white plate, a room nobody is awake in a navy one drawn in thin blue lines; the canvas reads that night colour, that line and the pond from the stage's own CSS (`house.css`), so the picture follows the page's look. `house.html?demo` is a made-up floor to look at it by | written by `ops.py`; open `house.html` |
| `assetboard/src_acts.py` | Which room a piece of work is in, for the house: a tool call (a shell command by what it does: only looking and tests are the lab, films and renders the studio, builds the workshop, git that changes something the workroom), a skill, an agent and a machine by their trade, and of a worker's last calls the room most of the newest point to. No file or clock in it. `src_ops.py` asks it for each worker's `act` and each roster entry's `home`, and also says when a session waits on the owner (`wait`), which checkout a session started outside every checkout works in (the one its calls point into), and that a session waiting on running agents is at work | read by `src_ops.py` |
| `assetboard/sprites.py` | Packs the owner's walking frog (the sprite project `pepe_frog_walk_h3`, outside git: a PNG per frame) into a sheet per animation, at full and at half size, with a manifest (frog.json) saying what is on each, where the feet stand and how far a walk travels a frame. Into the station's board cache (the folder frog under LOCALAPPDATA, TrenchWarfare/assetboard); `ops.py` copies the sheets into the site and without them the house draws marks. A walk that is another's mirror is not packed. Run it again when the sprites change. Needs Pillow | `python Tools/assetboard/sprites.py` (`--src FOLDER`, `--out FOLDER`) |
| `assetboard/test_house.py` | The house's own tests, under node: every spot can be walked to along the two axes, through the gaps in the walls and round the furniture; who is put in which room and gets which spot; the frogs over time. The Python half (rooms of work, sessions, the sheets) is in `test_assetboard.py` | `python Tools/assetboard/test_house.py` |
| `assetboard/notes.py`, `static/board.js`, `board.css` | The owner's notes. Every thing on the board that can be clicked (a frog in the house, a desk and a room of the office, a row of "Needs you", a model's page) opens a panel: what it is, where it leads (its branch, the models that branch touches, its room) and the notes about it, with a box for a new one. A note is a file in the notes folder (the folder notes of the Drive's TW3D-pipeline, so both stations read the same ones; TW_NOTES names another), and it is the owner's word: read the open ones for your branch before you start, and answer with `done`, which closes it and shows the answer on the page. A page cannot write a file, so `ops.py --watch` keeps a listener on this machine only (127.0.0.1, port 8765) that takes a note with the key of the notes folder and nothing without it; when no listener answers the note stays in the browser, marked not sent, until one does | `python Tools/assetboard/notes.py` (`--for BRANCH`, `--all`, `done ID "what you did"`, `add "text"`, `serve`) |
| `assetboard/src_graphs.py`, `static/charts.js`, `templates/graphs.html` | The graphs page (`graphs.html`), made again on every read of `ops.py`: the tool calls an hour by the room each is work in (48 hours), which branch got the work (7 days), what waited on the owner over time, the commits a day on integration (30 days) and the models by kind and status. The work is read from this station's transcripts through a cache (a transcript is read once, then only what was written since); what waited is a sample a read, kept in the station's cache (history.jsonl), since nothing else holds yesterday's number. A room, a branch and a status are links to their place on the board; every graph has its table | written by `ops.py`; open `graphs.html` |
| `assetboard/static/control.js`, `control.css`, `src_visuals.py`, `templates/index.html` | The control screen: the overview (`index.html`) is one page that says who is at work on what, what waits on the owner and how the day went. Down the page: a headline that is the answer (how many at work, how much waits) with the age of the last reading counted every second (late after a minute, long before the hour that turns it red); six tiles; the house, with the profile of one worker beside it (the note panel of `board.js`, docked into the page): the frog the owner clicked, else the one the house follows, a session at work, kept for as long as it works. A ring on the floor and a ringed name mark that frog. The profile has the worker's portrait, what it is doing, where it leads, its notes and a note box that a reading leaves alone, and its last visual: the last picture it looked at or the last picture or film one of its commands named (`src_visuals.py`, from the same cached pass over the transcripts as the graphs), copied small into the site's folder img/last and said to be what it is (looked at, named in a command, or the newest capture of its branch, at most a week old). Not seen: a picture a script wrote without naming it, a picture a tool returned without a path, a picture the owner pasted. Then what waits on the owner, the five graphs, the branch rooms and the models. `house.html` and `graphs.html` are the same parts full size (the templates share them), one click from the screen. A number counts to its value and a graph grows from its baseline once; nothing moves when the reader asked for less motion | written by `build.py` (the page) and `ops.py` (its readings and the visuals); open `index.html` |
| `assetboard/briefs.py`, `static/decide.js`, `decide.css`, `templates/decide.html` | Decision briefs: for every decision that waits on the owner, one short report he can decide from. `briefs.py add` writes one into the folder decisions on the Drive (beside the notes; TW_BRIEFS names another): what it is for (45 words at most), two to four options with the writer's first and why, and a copy of each picture or film that bears on it, or the word why nothing can be shown. It refuses a brief that is not short and says every reason. `briefs.py missing` lists the questions under "Open" in decisions.md that have none. `ops.py` puts the open ones in the site on every read; `decide.html` opens on an index of the open briefs (a title each, marked once he has said something), then shows them with the options as buttons (a click leaves a note through `notes.py`, the owner's word), the ones he has not answered first and newest first, the ones he has answered under them as "Decided by you, in progress" (his answer is the decision: such a question is no longer counted as waiting on him, `src_queue.py` lists it as `decided`), then the questions without a brief, then what was decided this week, with the questions answered on this lane and not landed. A row of "Decide" on the overview leads to its brief. **What happens then.** An option can say it: `briefs.py then ID OPTION --says "..." --unit FILE` puts a line "Then: ..." under the option on the page (14 words at most) and names the unit that is queued on the relay's board when he takes it (FILE is a unit in the shape of the relay's queue: id, lane, goal, done_when; without `--unit` the line says nothing is built). A click on such an option is his yes to that unit and to no other (decisions.md, 2026-10-06): the page sends the stamp of the Then line it showed with the click. `briefs.py waiting` lists what he has answered that no session has taken up, every note of his about each, and what it leads to: `queue` (his yes: the master queues the unit without asking), `nothing` (his yes to an option that builds nothing) or `write` (no Then line, one added or changed after his click, clicks on two options, words of his own, a note that is not his click: his answer is the decision all the same, so the session writes the unit it leads to and queues it, or says why nothing is built, and asks him only when his words do not say what to build). That rule is written in `briefs.py` and nowhere else; `health.py` prints the same lines. **Taking an answer up:** the row in decisions.md first; for a `queue` answer `briefs.py unit ID --note NOTE --out FILE` writes the unit for the relay to queue; then `briefs.py take ID --note NOTE --by WHO` closes the brief with what all his notes say and with what became of it (the unit or the "nothing" of his yes; for a `write` answer `--queued UNIT` or `--outcome "words"`, so no brief closes without saying), and answers his notes. It refuses a brief somebody has already taken and one he has answered again since NOTE. The page then says who took it up and what was queued. `briefs.py answer ID OPTION "his words"` is for an answer he gave somewhere else. **A row of "Needs you", clicked:** `src_queue.py` `details` gives every row of the owner queue at most five short lines and at most three pictures (`ops.py` puts them in the site under img/queue), and the panel of `static/board.js` shows them with buttons that leave his word as a note. **Every step of an asset:** `briefs.py steps` (run by `ops.py` on every read) writes a brief for each stage of an item on the pipeline's board that has passed and has a picture or a film to show: approve it, send it back, or ask for better captures. A step with nothing a page can show gets no brief and the page lists it as owing a capture, as it does a stage that names no band. **Concepts first:** work on an effect, an animation or a character starts with `briefs.py concepts`, a brief of two to four concepts or references (pictures, short films, or pages drawn in HTML or SVG, which it photographs) that he picks from; nothing of it is built before he has picked | `python Tools/assetboard/briefs.py add --title ... --for ... --option ... --option ... --why ... --evidence PATH="what it shows"` from `trench-warfare-3d/`; open `decide.html` |
| `assetboard/gamefilm.py` | Films of the game itself, into the assetfilm folder of `Captures/`, which build.py picks up. `up` starts a batch-mode editor on this checkout (no window, but it renders). `film` films every playground vehicle and figure (turning, moving and firing, shot to pieces) and every chunked building (turning, shelled). `battle` films every battle machine in a match in GreyboxCorridor (turning, driving at enemy riflemen and firing, destroyed). `down` closes the editor. Frames are taken with `Time.captureFramerate`, so a film does not depend on the machine's frame rate | `python Tools/assetboard/gamefilm.py up`, then `film [NAME ..]` and `battle [NAME ..]`, then `down` |

Films of the game are harvested, not derived: nothing remakes them when a model changes. Run `gamefilm.py` again
after art changes; the page shows when each was filmed.

What only a person knows goes in `docs/reference/asset-notes.json`: `priority`, `note`, `wants`, `parked` and
`model_of` per asset, and a `planned` list for assets that are only an idea. An unknown id or key stops the build.
A new machine or building needs nothing here: it appears once its files or its id exist.

## Two-station agent pipeline

Work split between the laptop and the desktop, one stage at a time per item, with staleness worked out from the
repo. The board, the states and the commands are in `docs/reference/stations.md`.

| Script | Does | Usage |
|---|---|---|
| `pipeline/pipeline.py` | Job board and stage states (DONE, RECHECK, STALE, IN_PROGRESS, READY, BLOCKED) from `tw3d-board` | `python Tools/pipeline/pipeline.py status` |
| `pipeline/test_pipeline.py` | Its tests, on throwaway git repos | `python Tools/pipeline/test_pipeline.py` |
| `pipeline/run_detached.py` | Runs a gate, bench, sweep or batch detached from the session: pids with start times, a heartbeat from log growth, a timeout that stops only its own tree, a commit-headroom floor. Runs live in `%LOCALAPPDATA%/TrenchWarfare/runs`, never in a checkout | `python Tools/pipeline/run_detached.py start <name> --timeout 3600 --min-headroom-gb 10 -- <cmd>` then `status <name>` |
