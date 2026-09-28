---
name: tw-vfx-sheets
description: Make a NEW visual-effect flipbook for Trench Warfare 3D on the desktop and bring it into the game — generate the animation with MiniMax H3 through the shared GPU broker, cut it into a sprite sheet, check its edges, publish it to the owner's pack, convert it with Tools/firebooks.py and register it in FlipbookFx. Use for "make a new explosion/fire/gas/smoke/blood effect", "we need a flipbook for X", "regenerate the jet sheet", "add this sheet to the game". Desktop only (DESKTOP-NLJRH7R: ComfyUI + RTX 5090). NOT for placing or tuning existing effects in battle (tw-destruction-vfx) and NOT for the EMTD or other projects' flipbooks.
---

# New VFX sheets (desktop → game)

The game draws effects as **greyscale flipbooks**: `Tools/firebooks.py` turns the owner's colour sheets into
premultiplied-grey books in `Assets/_Project/Resources/VFX/`, and `FlipbookFx` tints them. New drawings are made on
the desktop by the `vfx-flipbooks` generator. This skill is the whole road, and the format contract between the two
halves is where it breaks, so read section 3 before cutting anything.

Details: `references/generator.md` (every script, its flags and constants), `references/prompt-recipe.md` (the H3
prompt that works), `references/firebooks-contract.md` (what the game accepts, and how to register a book).

## 0. Preflight

| Check | How | If it fails |
|---|---|---|
| This is the desktop | `hostname` = `DESKTOP-NLJRH7R` | stop: the laptop has no ComfyUI |
| Broker healthy | `curl -s http://127.0.0.1:8188/broker/status` → `"health": "ok"` | `schtasks /Run /TN gpu-broker`; diagnose with `uv run --directory C:/Users/PC/Documents/claude/EMTD-AgentAnimation/tools/gpu-broker python -m gpu_broker doctor`. Never start ComfyUI yourself |
| Commit headroom | the broker trips its thrash guard above 108 % commit with paging; `python trench-warfare-3d/Tools/pipeline/run_detached.py` refuses below a floor | run one H3 job at a time; never alongside a full gate or a Unity bench |
| A lane worktree for the game half | a `lane/show/*` checkout (e.g. `githubtest-desk-show`) | the main clone is on integration: never edit there |

## 1. Generate (about 150-200 s per job, RTX 5090)

Work from `C:\Users\PC\Documents\unity\vfx-flipbooks\Tools\gen` — the scripts import each other and write
`black768.png` into the working directory.

1. Pick a reference burst from `C:\Users\PC\Documents\drive\vfx-flipbooks\refs360\` (24 cut DucVu refs +
   `<ref>_k0..k2.png` peak frames). New references need `refclips.py` and the source video (not found on this machine).
2. Add one job to a batch module, copying its shape: fire → `batch6.py`, electricity → `batch7.py`, blood → `batch5.py`,
   explosions → `batch4.py`. A fire/electric job is `name: (ref, COLORS, action, mode, (w, h), pivot)`.
   **For a game book use a square frame (768×768) and pivot C or B** — see section 3.
3. Run it in the background and log: `python -u batch6.py <tag> <name> <seed> > batch_<tag>.log 2>&1`.
   Output: `C:\Users\PC\Documents\drive\vfx-flipbooks\<tag>\<name>_00001_.mp4` (24 fps, 124 frames).

Broker rules (it is shared with other agents' jobs):
- `comfy.py` sends `X-Gpu-Agent: claude-vfx-flipbooks` on every request. Just submit and wait; do not pre-check the queue.
- Never `/interrupt`, `/free`, restart or kill ComfyUI, never use port 8191 directly, never edit `broker.toml`.
  Priorities change only on the owner's word: `python -m gpu_broker prio ...` (see `references/generator.md`).
- A slow job is not a stuck job: never cancel because a wait timed out. `SystemThrashing` means use a smaller
  model, not retry.
- A new model file rejected as `ModelNotFound`: POST any small image to `/upload/image` (every batch already does).

## 2. Cut, fix, check — always into a stage folder

```bash
D=C:/Users/PC/Documents/drive/vfx-flipbooks
python sheet.py $D/<tag>/<name>_00001_.mp4 --name <name> --out $D/stage/<tag> \
   --frames 32 --size 320 --fps 12 --t0 8 --t1 40 --select change --cols 8
python wallfix.py   "$D/stage/<tag>/*.png"          # rewrites in place: straight cut walls -> organic lobes
python edgecheck.py "$D/stage/<tag>/*.png"          # must be all clean, exit 0
python review_sheets.py "$D/stage/<tag>/*.png" $D/stage/rv_<tag>.jpg 130   # montage for the owner
```
- `sheet.py`'s own defaults are t0 14 / t1 70; the pipeline's are **8 / 40**. Always pass them.
- **Never point `--out` at `G:/My Drive/vfx/sheets` or the Unity `Sheets` folder**: the batch cutters
  (`make_sheets.py`, `make_fire_sheets.py`, `remake_all.py`) delete older sheets of the same name in `--out`.
- A montage alone misses flat cuts; edgecheck + wallfix every time.

## 3. The format contract (why good sheets come out wrong in the game)

`firebooks.py` slices **every** input as an **8 × 4 grid** and resizes each cell to **256 × 256**. It never reads
the filename. So a sheet the game can take must be:

1. exactly 8 columns × 4 rows — `sheet.py` makes 8×4 only when **25-32 frames** are picked (17-24 give 8×3, ≤16 a
   square-ish grid); pass `--cols 8 --frames 32` and check the file name says `_8x4_`;
2. square cells — `firesheet.py` (pivots L/R/B, loops, chains) makes non-square cells; use `sheet.py` for game books;
3. clean at the cell edges (edgecheck), and rooted at the left edge if it is a directional stream (`root_left`).

Anything else is silently cut wrong. Check the grid before publishing.

## 4. Publish and convert (game repo, a `lane/show/*` worktree)

1. Copy the accepted `<name>_8x4_12fps_<n>f.png` into `G:\My Drive\vfx\sheets` **under a new name**. Never overwrite
   or delete a sheet there: signed-off books are cut from exact files, and the Unity `Sheets` recut does not match them.
2. `Tools/firebooks.py`: add `BOOKS["Fire<X>"] = (file, "luma" | "light", root_left, keep)` and `FRAMES_IN["Fire<X>"] = n`.
   `luma` for orange sheets, `light` for saturated blue/green/red; pick the sheet by its **shape** (the colour is thrown away).
   `Tools/**` is shared: this edit is **a commit of its own**.
3. `cd trench-warfare-3d && python Tools/firebooks.py` — expect `UNCHANGED` for FireBall, FireColumn, FireBurst, then
   `git diff --stat -- Assets/_Project/Resources/VFX`: only the new book may appear.
4. `FlipbookFx.cs`: a `Book` enum entry **and** a `Sheets` row at the **same position** (the table is indexed by the
   enum's number), with Cols 8, Rows 4, Frames = kept count, Fps 12, and Low/High/Bands/Ink/Fill pasted from the
   firebooks printout. A missing PNG disables every flipbook (`Ready`).
5. Let the editor import it (writes the `.meta`), `gate.ps1 -EditOnly`, then look at it in Play:
   `bash Tools/flameshots <prefix>` and `python Tools/flamecheck.py Tools/flame-shots/<prefix>_jet.png`. Capture the same
   build twice first to know the noise.

## Owner questions — ask, never decide

- **Decided on 2026-09-28** (`decisions.md`):
  - explosion batches B and C are **go**, inside the VFX pass;
  - blood on hits uses the pack's blood sheets, scaled by GORE.

  The VFX lane and the broker queue belong to the session doing the VFX run (pc-e5 on 2026-09-28): check with it
  before queueing.
- Priority changes on the shared broker; overwriting or renaming anything in `G:\My Drive\vfx\sheets`.
- The `FlipbookFx` Core/Head/Bloom row order is fixed **inside the VFX pass**, as its own commit with
  `FlipbookOrdinalTests` (decided 2026-09-28). It is never fixed inside an unrelated change.

## Learning loop
Brief 2 §B5, the same for every role: see `../pipeline/SKILL.md`, "The learning loop". Step 0 is reading this role's
lines in the board's `lessons.md`.
