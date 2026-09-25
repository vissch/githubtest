# Asset pipelines and Tools/

How art gets from the owner's source files into `trench-warfare-3d/Assets/_Project/Resources/`. Every script lives in
`trench-warfare-3d/Tools/` and is run from `trench-warfare-3d/`. The editor-driving tools (`tw`, `flameshots`,
`editor_lock.py`, `health.py`, `shotstats.py`, `codemap.py`) are in `workflow.md`.

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
| `housesplit.py` | village houses, watchtower, ruins sheets | `Resources/Env/<Set>/Chunks/*.fbx` + `houses.json` | `-- <fbx> <outdir> <renderdir>`. Flags: `TW_SCALE` (sheet units to metres), `TW_CUT` (chunk edge, m), `TW_SMALL`, `TW_TEX` (basecolour, reads stone vs timber), `TW_PIVOT` (house or chunk), `TW_NAMES`, `TW_TURNS`, `TW_LOOSE=1` (welded sheet), `TW_ONE=1` + `TW_KEEP=1` (slice one kit prop, keeps pivot and facing), `TW_FLOORS=1` (cut along storeys), `TW_STOREY`, `TW_BAND`, `TW_MINTRIS`. Values used per set: UNRECORDED except Houses (`TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4`) and the kit props (Biplane `TW_CUT=1.9`). |
| `tanksplit.py` | `Downloads\cartoon+tank+3d+model.zip` (full), `Downloads\tank+3d+model.zip` (far) | `Resources/Vehicles/<Maw,Tusk>/<Tank>_LOD0,1.fbx` + `TankAtlas_LOD0,1.jpg` | `-- <full.fbx> <far.fbx> <outdir> <renderdir>` |
| `crabsplit.py` | the walker sheets (`Downloads\robotic crab 3d model.zip` and five more) | `Resources/Vehicles/<Walker>/` | `-- <sheet...> <outdir> <renderdir>` |

Traps, all silent:
- **Axis.** Blender's FBX export with `bake_space_transform` maps Blender -Y to Unity -Z whatever `axis_forward`
  says. The scripts turn every part 180° first. Check facing by where the barrel vertices sit, not by bounds.
- **MAX_PATH.** Tripo texture paths exceed Windows' limit. Copy each sheet to a short path first.
- **Parenting.** Every walker/tank part must be parented to the Body, or `TankModel` never draws it.
- **Re-slicing a kit prop** needs `TW_KEEP=1`, or it comes out turned 180°.
- **Import settings.** `Editor/TankImport.cs` and `Editor/EnvKitImport.cs` set import rules by folder. Vehicle and
  chunk meshes must stay Read/Write enabled; setting it by hand does not stick, the postprocessor resets it.

## Environment textures and atlas

| Script | Does | Usage |
|---|---|---|
| `envgrade.py` | Grades the Tripo set textures into the field's muted range | `python Tools/envgrade.py "<folder with the unzipped sets>"` |
| `envatlas.py` | Packs every set's texture into `Resources/Env/EnvAtlas.jpg` (4x4 grid of 1024 px cells) | `python Tools/envatlas.py` |

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
- **Regenerating must not move signed-off books.** The script hashes existing PNGs and prints `UNCHANGED <name>.png`;
  a missing line for a book nobody meant to touch means its look moved. New `.meta` files are cloned from
  `FireBall.png.meta` with a fresh GUID.
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
