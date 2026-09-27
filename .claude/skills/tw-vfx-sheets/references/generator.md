# The desktop generator: C:\Users\PC\Documents\unity\vfx-flipbooks

Unity 6000.3.19f1 (2D URP) project with its own sheet importer; its `Tools/gen` scripts are what matter here.
Run every script from `Tools\gen`.

## Where things land
| What | Path |
|---|---|
| Reference clips (cut) | `C:\Users\PC\Documents\drive\vfx-flipbooks\refs360\<ref>.mp4`, `<ref>_k0..k2.png` |
| Raw H3 videos | `C:\Users\PC\Documents\drive\vfx-flipbooks\<tag>\<job>_00001_.mp4` (ComfyUI output root `C:/Users/PC/Documents/drive`) |
| Stage and review | `C:\Users\PC\Documents\drive\vfx-flipbooks\stage\<tag>\`, `stage\rv_<tag>.jpg` |
| The owner's pack (firebooks input) | `G:\My Drive\vfx\sheets` (35 PNGs on 2026-09-28: RGBA straight alpha, 320 px cells) |

## comfy.py (the client)
- `API = "http://127.0.0.1:8188"` (the broker), `AGENT = "claude-vfx-flipbooks"` (hardcoded, `GPU_AGENT` is ignored),
  `OUT_ROOT = C:/Users/PC/Documents/drive`.
- `submit()` POSTs `/prompt`; `upload()` posts to `/upload/image` (subfolder `vfx-flipbooks`); `wait()` polls `/history/{id}`.
- `h3_video()`: MiniMax H3 ref2va — UNET `minimax_h3_ref2va_pruned_int8_convrot.safetensors`, CLIP
  `qwen3vl_32b_minimax_h3_nvfp4_awq.safetensors`, `KSamplerSelect res_multistep` + `BasicScheduler beta`, 768×768,
  124 frames, 20 steps, `SaveVideo` mp4 at 24 fps. `h3_video_ref()` adds a reference video and, with `anchor_black`, a
  `MiniMaxH3AddGuide` per black anchor (frame 0 = start, -1 = end).
- `krea2_multi()`: stills (krea2 turbo fp8, 1024², 10 steps).
- Graphs are Python dicts built in comfy.py, not stored JSON workflows.

## Batch modules (`python -u batchN.py <tag> [name,name] [seed_base]`)
| Module | Subject | Seed base | Notes |
|---|---|---|---|
| `batch2.py`, `batch3.py` | text-only | — | the generic look the owner rejected; colour triplets live here (batch2) |
| `batch4.py` | DucVu explosion remakes | 500 | every ref `h3_video_ref(w=768, h=768, length=124, steps=20, anchor_black)` |
| `batch5.py` | blood | 900 | `JOBS = {name: (ref, action)}` |
| `batch6.py` | fire | 2000 | `J = {name: (ref, colors, action, mode, (w,h), pivot)}`; jets 1152×576 |
| `batch7.py` | electricity | 4000 | `NOHALO` clause, `EXTRA --topfade 0.3` for sky bolts, portrait 576×1152 |

Modes: `oneshot` (black at both ends, one event), `loop` (burning from frame 0, never changes size), `chain`
(start/loop/end from one video), `ignite` (black start), `die` (black end). Pivots: L (jet at the left edge),
B (on an invisible floor), C (centre), R.

Timing: 23 fire jobs took 3,849 s (≈167 s each). Broker class `h3_full` (20 steps, priority 500); ≤ 8 steps is `h3_turbo` (40).

## Cutters
- **`sheet.py`** (square, centre pivot):
  `python sheet.py video.mp4 --name N --out DIR [--frames 32 --size 320 --fps 12 --cols 0 --t0 14 --t1 70 --start --end --energy 0.002 --margin 0.06 --loop --cycle 0 --bg median|black --select change|even --keep_frames]`.
  - Keying: alpha = smoothstep((max-RGB or |rgb − bg|) − t0) / (t1 − t0), un-premultiplied; dark desaturated pixels dropped.
  - Background: the 12th percentile over time.
  - `--cycle 0` keeps only the first burst, because H3 repeats a burst to fill 5 s.
  - Grid: `cols = 8 if frames > 16 else ceil(sqrt(frames))`.
  - Name: `{name}_{cols}x{rows}_{fps}fps_{n}f[_loop].png`.
  - Output is RGBA straight alpha, plus a preview GIF.
- **`firesheet.py`**: `--mode oneshot|loop|chain|ignite|die --pivot L|B|C|R --size 256`. `--size` is the frame height, so cells are NOT square. Name ends `_piv{X}`. Assumes a 24 fps source.
- **`make_sheets.py <tag>`** / **`make_fire_sheets.py <tag> --jobs batch6|batch7 --out DIR`** / **`remake_all.py`**:
  batch wrappers; **they delete older same-name sheets in `--out`**.
- **`edgecheck.py "<glob>" [--json out.json]`**: exit 1 if any sheet has the drawing touching its bounding box.
  Constants: TOUCH_A 40, CUT_LEN 0.18, WALL_LEN 0.25. Pivot sides are exempt: pivB bottom, pivR right, pivL left middle 25-75 %.
- **`wallfix.py "<glob>"`**: in place, erodes straight walls into wavy lobes (up to 3 passes per frame).
- **`review_sheets.py "<glob>" out.jpg [row_h]`**: montage, 8 frames per sheet on grey.

## GPU broker (shared: C:\Users\PC\Documents\claude\EMTD-AgentAnimation\tools\gpu-broker)
- Broker on 8188 in front of ComfyUI on 8191; the broker owns the backend process. Operator page `http://127.0.0.1:8188/broker`.
- Owner: header `X-Gpu-Agent` (else `extra_data.agent`). Up to 200 pending per owner (HTTP 429 above that). A job orphans after 3,600 s with no poll or submit.
- Wait: `GET /broker/jobs/{id}/wait?timeout=N` (max 540). It returns on a phase change or with `timed_out: true`. A running job refuses cancel without `force`.
- Priorities (live, operator only): from `C:\Users\PC\Documents\claude\EMTD-AgentAnimation`,
  `uv run --directory tools/gpu-broker python -m gpu_broker prio list | job <id> top | owner <glob> N | class <name> N | reset`.
  A value of −100 or lower blocks an owner.
- Thrash guard: more than 3,000 pages/s **and** commit above 108 % of RAM for 90 s fails the running job (`SystemThrashing`) and holds its class for 30 min, doubling per strike.
- A zombie ComfyUI on port 8190 (pid 39748) is listed as `foreign`: leave it.
- Stale model list: new model files are rejected until any image is POSTed to `/upload/image`. The permanent fix (`preflight.fetch_timeout_s` above 20) is not applied.

## Unity side of the generator (optional)
`FlipbookSheetImporter.cs` parses `^(name)_(cols)x(rows)_(fps)fps(_(n)f)?(_loop)?(_piv[LBCRT])?$`. Headless rebuild:
`"C:/Program Files/Unity/Hub/Editor/6000.3.19f1/Editor/Unity.exe" -batchmode -nographics -projectPath . -executeMethod VFXFlipbooks.Editor.FlipbookBuildTools.BatchBuild -quit -logFile Tools/unity_build.log`.
The game does not use this project's sheets directly.
