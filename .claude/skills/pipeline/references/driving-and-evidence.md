# Driving the game and capturing evidence (shared by every tw-* skill)

The repo's pages are the source of truth: `docs/reference/workflow.md` (sections 1-6, 8) and `CLAUDE.md`. This page is
the short form every role skill relies on. Run commands from `trench-warfare-3d/` unless a line says otherwise.

## Before touching Unity
- `python Tools/health.py`: lock, memory, your editor, validate, your lane, your inbox. `lock held` means do not gate and do not write into `Assets/`.
- Memory is **commit headroom**, not free RAM. Under 6 GB, run nothing. Under 10 GB, a batch gate is fine but open no new editor.
- One editor per checkout. Never drive another checkout's editor: `Tools/tw` pins every call to this one.
- The gate needs this checkout's editor **closed**. CaptureRig needs it **open and in Play**. A stage says which it needs.

## The editor from the command line
| Do | Command |
|---|---|
| open this checkout's editor | `Tools/tw up` |
| open the battle scene | `Tools/tw run open_scene -- --path "Assets/_Project/Scenes/GreyboxCorridor.unity"` |
| play / stop | `Tools/tw run editor_play` / `Tools/tw run editor_stop` |
| run C# | `Tools/tw eval '<fully-qualified C#; return ...;>'` (one-liners); longer bodies: `Tools/tw run eval_file -- --path <abs .cs>` |
| compile | `Tools/tw build`; check `compilationFailed` before believing anything you see |
| one test class | `editor_stop`, `RequestScriptReload`, then `Tools/tw run run_tests -- --mode EditMode --filter TW.Tests.<Class> --timeout 300` |
| gate | from the repo root, editor closed: `powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly` (before every commit) or without `-EditOnly` (before landing, after any sim change) |

Long runs (gate, bench, batch tests, Blender, ComfyUI) go through
`python Tools/pipeline/run_detached.py start <name> --timeout <s> --min-headroom-gb 10 -- <cmd>`, then `status <name>`.
Git Bash rewrites `/E`-style flags into paths, so prefix Windows tools with `MSYS2_ARG_CONV_EXCL="*"`.

## Seeing the game: CaptureRig (Editor/CaptureRig.cs), in Play in GreyboxCorridor
- `TW.Editor.CaptureRig.Shot(path, x, z, zoom, yaw, pitch=25, w=1600, h=900, aimY=NaN)`, then poll `Pending()` until it returns "0".
- `Series(dir, stem, x, z, zoom, yaw, pitch, count=8, everyFrames=6, ...)`, `Sheet(dir, stem, outPath, cols=4, cellW=400)`.
- `Diff(a, b, outPath)` returns `changed_frac` (a pixel counts as changed above 0.02 luma), a box, and before/after stats.
- `Hold(weatherClock)` / `Release()` hold the clock; never pause. `Crowd(cell)`, `ShotCrowd`, `LastReport()`, `Bench(args)`.
- Every still writes a JSON sidecar: `luma_mean`, `luma_p95`, `blown_frac`, `black_frac`, `men_in_frame`, `contrast_median`, `contrast_p10`, `pose_error_m`.
- **Read the numbers, then look.** Capture the unchanged build twice to know the noise.
- `shotstats.py` uses 0-255 Rec.601 and CaptureRig uses 0-1 Rec.709: never compare a number from one with a number from the other.
- A render-to-texture capture bypasses post-processing. What the owner sees is the camera or the player's `shot=`.

## Zoom bands (the repo's tiers: "never change these", docs/reference/aosa/README.md)
| Band | Camera | Used for |
|---|---|---|
| T1 standard | zoom 30, fov 25, pitch 25, yaw +21 | the view the budget is judged at |
| T2 mid | zoom 14-18 | squads |
| T3 close | zoom 6-9, 42° lens, ~1.1 m up | detail (behind `_TWClose` / `SceneHooks.CloseUp`, free at T1) |
| Overview / far | zoom 120-240 / 600 | the battlefield (`OverviewFullZoom` 240, `ZoomMax` 600) |
| Walkers | docs/20's ladder: 14× / 7× / 3.5× / 1.8× machine height | tw-vehicle-sim |

The ready-made tier script is `AgentScripts/aosa_tiers.cs` (EditorPrefs `aosa.dir`, `aosa.stem`, `aosa.hold`, then
`eval_file`). It shoots T1 (30, 21, 25), T2 (16, 21, 22) and T3 (7.5, 21, 13) at 1920×1080.

## Staging traps (workflow.md)
- Move the camera with `TacticalCamera.FrameFrom(focus, zoom, yawDeg)`. Writing `Camera.main.transform` is overwritten every frame.
- Read `World.Position[slot]` back after a spawn. Freeze with `SimHost.TimeScale = 0`.
- `Hold()` before the first sim step draws every vehicle at the origin.
- `AlignWorlds` sometimes answers "worlds a tick apart": retry.
- Never write one world from an eval; use `SimHost.WriteWorlds(...)`.
- Repeatable stills: a seeded Random, `stepabs`, and on the bench `shot_tick=N shot_hud=0`.

## Evidence on the board
JPG, at most 400 KB, at `evidence/<item>/<stage>/<band>.jpg` on `tw3d-board`. Keep the PNG and its JSON sidecar
under `trench-warfare-3d/Captures/` (git-ignored), never as untracked files a gate would record. Copy the sidecar
beside the evidence JPG on the board as `<band>.json`: the critic's validity rules (`pose_error_m`, `blown_frac`) read
it, and a relay critic is handed only the top-level files of that folder. Add `frames.txt` there, one line per
image, starting `<band>.jpg:`: view and band, moment, clock held or running, what is in frame. Facts only: the
sidecar holds none of these. A still the rig did not shoot as it stands (a contact sheet, a diff, a concept sheet,
a mock-up) has no sidecar: its `frames.txt` line says `no sidecar`. The relay fails a job whose still has neither.

## After a critic round: fix it and prove it
The critic's paper is on the board beside the evidence: `evidence/<item>/<stage>/critic-r<n>.md`. A relay fix card
carries only its three fix lines, so read the whole paper first.
1. **CAPTURES first.** INVALID or UNCHECKED means the stills are the fault: shoot them again, a sidecar beside each, before changing any art or code.
2. **Reproduce, then measure the claim.** Shoot the critic's frame again from its sidecar (`focus_x`, `focus_z`, `zoom`, `yaw`, `pitch`), at the same moment (`stepabs`, or `shot_tick` on the bench), under `Hold()`. Put a number on the claim: a sidecar field, `Diff`, `shotstats.py`, a probe row. A claim the number does not bear out is rebutted with that number, not fixed.
3. **Find the cause yourself.** The critic has not seen the code: its `GUESS:` is a guess. Its `want:` is how far to go; read OVERDONE before turning anything up.
4. **One fix, one pair.** Before and after from the same pose, moment, seed and held weather, and the unchanged build shot twice for the noise. `Diff(before, after, out)` has to show a change larger than that noise, in the region the finding names. A fix whose pair cannot be told apart has not reached the screen.
5. **Check it where it ships:** in the battle (GreyboxCorridor) at T1, not only on a bench or in the playground, and look that readability did not drop.

**Hand back, in the evidence folder** (top level, JPG at most 400 KB, a sidecar beside each still):
- every band again, fresh: `<band>.jpg` and `<band>.json`;
- per fix, the pair as `fix<k>-x.jpg` and `fix<k>-y.jpg`: a coin decides which is the old one, and the key goes in your report, never in the folder;
- where the subject is small in the frame (a man can be 30 px), the same pair cropped to the region the finding names, at native resolution or 2× nearest-neighbour: `fix<k>-crop-x.jpg`, `fix<k>-crop-y.jpg`;
- each shot the critic asked for, at the view it asked for: `shot<k>.jpg` and `shot<k>.json`;
- `frames.txt` again, with a line for every new image.

Delete the pairs, crops and shots of earlier rounds. Keep out of that folder anything that says what you changed or
why: the next critic is blind, and it is handed every top-level file there.

**Your report, not the folder,** has one line per mandated fix, `FIX <k>: done | rebutted | shot made; <measure>
before -> after (noise <n>); <files>`, and the x/y key.
