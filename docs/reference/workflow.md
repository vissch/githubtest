# Workflow: run it, test it, see it

Read this third, after `CLAUDE.md` and `tasks.md`. Every command is run from `trench-warfare-3d/` in Git Bash unless it
says PowerShell. Outputs marked **verified 2026-09-25** were run and pasted on that date; the others say when they
were last known to work.

## 1. Land checks (every session, first)

```bash
python Tools/health.py
```
Verified 2026-09-25, with this checkout's editor open:
```
lock     held  (an editor or batch run holds this checkout: no gate, no writes into Assets/ unless it is yours)
editor   pid 14352 state ready port 7801
validate OK
branch   lane/show/maint-2026-09  lane show  ahead 0 behind 0 of claude/trench-warfare-2d-3d-plan-idt7lf  13 uncommitted
```
- `lock held` with an editor that is not yours: do not gate, do not write into `Assets/` (a save recompiles their
  editor and kills their Play session). `lock FREE`: nobody has this checkout.
- `lane NONE`: stop and work out your lane (`CLAUDE.md`).
- `validate FAILED`: read the lines under it. `codemap:` lines are docs that no longer match the code
  (`Tools/codemap.py` explains each rule).
- Then read `docs/reference/inbox.md` for notes addressed to you.

## 2. The machine you share

Several Claude sessions and the owner work on this workstation at once, each in its own checkout
(`githubtest` = the main clone, `githubtest-aosa`, `githubtest-sim`, `githubtest-maint`, `aosa-c1-*`). Rules that
cost real time when broken:
- **One editor per checkout.** Never open a second editor on a path that has one.
- **Memory is the scarce resource.** An editor in Play holds 5-9 GB and the machine has 16. On 2026-09-25 three
  editors plus a player benchmark left 0.4 GB free; two editors vanished without a crash dump and the owner's was
  paged out and stopped answering. Check before you open another editor:
  ```bash
  powershell -NoProfile -Command "[math]::Round((Get-CimInstance Win32_PerfFormattedData_PerfOS_Memory).AvailableMBytes/1024,1)"
  ```
  Under ~4 GB available, do not open one; use a batch run later, or offline checks.
- **Never drive someone else's editor.** The `unity` CLI auto-detects a project and several are connected at once.
  `Tools/tw` pins every call to its own checkout (`UNITY_PROJECT_PATH`). A bare `unity cmd ...` from another folder
  can land in the owner's editor or a batch test run and fail it.
- **The advisory slot.** `python Tools/editor_lock.py claim <you> --minutes 15 --why "<what>" || exit 1` before
  taking a shared checkout's editor; `release <you>` after the commit, not after the gate. The claim is a gate:
  `;` or a pipe after it swallows its exit code. `editor_lock.py wait` watches only the lockfile, not the slot.
- **Never set `EditorApplication.update = null`** in an eval. It removes the pipeline's own pump and every
  session's evals time out. Remove only your own delegate (`-= tick`).

## 3. Open and drive an editor for your checkout

```bash
Tools/tw up
```
Verified 2026-09-25 (about 60 s from nothing):
```
no editor connected - launching...
editor ready
```
`tw up` returns only once an eval answers. Right after launch the editor opens an **untitled** scene, so open the
battle scene before Play:
```bash
Tools/tw run open_scene -- --path "Assets/_Project/Scenes/GreyboxCorridor.unity"
Tools/tw run editor_play          # prints "Entered play mode"
Tools/tw run editor_stop          # prints "Exited play mode"
```
Evaluate C# in the editor. There are no `using` directives: fully qualify everything, and `return` a value.
```bash
Tools/tw eval 'var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); return "tick=" + h.Local.World.Tick + " peer=" + (h.Peer!=null);'
```
Verified 2026-09-25 in Play: `"result": "tick=135 peer=False"`. Single player runs one world, so `Peer` is null.

Longer scripts: write a `.cs` body to a file and run `Tools/tw run eval_file -- --path <absolute path>` (it refuses
any other extension). Any pipeline command is `Tools/tw run <command> [-- --arg value]`; `unity cmd` with no
command lists them all.

Answers you will see, and what they mean:
| Output | Meaning | Do |
|---|---|---|
| `503 Service Unavailable: Server Busy ... still settling` | editor just launched or compiling | wait and retry; `tw up` already waits |
| `Main thread operation timed out after 5000ms` | main thread busy (first seconds of Play build the battlefield, or a compile) | retry after 10 s; for long evals pass `--timeout 90` to `unity cmd` |
| `No Pipeline instance found for project` | no editor for this checkout: it was never opened, it quit, or it died | `unity status`; check free memory; `tw up` |
| a paused editor, `frameCount` stuck at 1 after a refresh | Error Pause stopped Play on a logged error | `Tools/tw pause false` |
| NullReferenceException every frame in `SimHost.Update` | a recompile during Play reloaded the domain | stop Play and start again; it is not a bug |

`Event.current` is null in eval, so code guarded by GUI events cannot be driven from it.

## 4. Compile

```bash
Tools/tw build
```
Verified 2026-09-25. Clean:
```
    "compilationFailed": false,
    "consoleErrors": 0,
```
With an error (a probe file, since removed):
```
    "compilationFailed": true,
    "consoleErrors": 1,
      "message": "Assets\\_Project\\Presentation\\Core\\ZzProbe.cs(2,76): error CS0029: Cannot implicitly convert type 'string' to 'int'",
```
A `Failed to handle /api/exec request` line during a build is `tw build` polling while the editor compiles, and is
harmless. Unity keeps running the last good assemblies after a failed compile, so Play and captures still work and
silently show the old code. Check `compilationFailed` before believing anything you see.

No editor of your own? Tools/aosa/occ.py (on `lane/show/aosa` until it merges) compiles assemblies offline with
Roslyn. It resolves unchanged assemblies from the main clone's `Library/ScriptAssemblies`, so name every assembly
you changed, in dependency order, or it compiles against yesterday's dlls. It cannot check shaders, USS or scenes.

## 5. Tests

**One class, in your open editor** (fast, while iterating). Leave Play and reload scripts first, or statics from the
Play session fail tests that pass in a fresh domain:
```bash
Tools/tw run editor_stop
Tools/tw eval 'UnityEditor.EditorUtility.RequestScriptReload(); return 0;'
Tools/tw run run_tests -- --mode EditMode --filter TW.Tests.EnvAtlasTests --timeout 300
```
Verified 2026-09-25, about 5 s:
```
  "Summary": { "Total": 1, "Passed": 1, "Failed": 0, ... },
  "Results": [ { "FullName": "TW.Tests.EnvAtlasTests.Every_Set_Has_A_Cell_...", "Status": "Passed", ... } ],
```
`--filter` takes a full name: a class (`TW.Tests.CombatTests`) runs all its tests, a method
(`TW.Tests.CombatTests.Garrison_ShootsAnAssaultInTheOpen_AndWinsTheExchange`) runs one. The class form is
verified above. PlayMode in the editor: add `--async_tests true`. The in-editor runner has hung the pipeline after about four
full runs (2026-09-24). Use it for one class at a time; run everything through the gate.

**The gate** (before every commit). It needs this checkout's editor closed. PowerShell refuses unsigned scripts on
this machine, so call it with a bypass:
```bash
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1            # validate + EditMode + PlayMode
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -EditOnly  # validate + EditMode
```
| Exit | Meaning |
|---|---|
| 0 | green |
| 8 | a test failed; the gate prints each failed test and its message |
| 6 | no verdict: compile error or licence. Not a pass |
| 3 | the checkout is held by an editor or another batch run |
| other | validate.py failed (its lines are printed) |

Full EditMode is ~338 tests and takes a few minutes; PlayMode is 15.

### False reds
- **After Play in the same editor:** statics survive leaving Play (`CameraShake`'s look point is the known one).
  `RequestScriptReload` and rerun before believing it. A red that survives a fresh domain with identical numbers
  is real.
- **External pipeline noise** in a batch run: `Unhandled log message: '[Error] WriteToProjectRoot failed: Sharing
  violation on path ...\.unity-pipeline-port'` or `'[Error] Failed to handle /api/exec request: Main thread
  operation timed out'`. Another session's CLI or the MCP server reached the batch run's pipeline port, and the
  logged error failed whichever test was running (a different one each run, seen twice on 2026-09-25). The gate
  reruns the failed tests once when every failure is one of these; a real failure is never retried.
- **First run after a Burst job edit** can run managed code and differ from the next run. Every sim job has
  `CompileSynchronously = true`; keep it on new jobs. Rerun before trusting a single determinism failure.
- **Stale `Library/BurstCache`** after a job struct changes: NullReference or IndexOutOfRange inside Burst jobs.
  Close the editor, delete it, reopen.

## 6. See the game

Do not judge a picture by eye first. Read the numbers, then look.

**A still of a pose** (the right way: posed on one frame, rendered on the next, camera put back):
```bash
Tools/tw eval 'return TW.Editor.CaptureRig.Shot("C:/abs/path/Captures/name.png", 100f, 120f, 30f, 21f);'
```
Arguments: path, focus x, focus z, zoom (30 = standard view), yaw (21 = standard), then optional pitch (25 =
standard; 80 looks nearly straight down), width, height. Night is the default look of GreyboxCorridor.
A top-down shot of a walker, for ground marks (archetypes: 4 Maw, 5 Tusk, 6 Pincer, 7 Kettle, 8 Censer, 9 Pavise,
10 Banner, 11 Redoubt):
```bash
Tools/tw eval 'return TW.Editor.TankCapture.Spawn(0, 6, 100f, 120f);'      # prints "slot N"
Tools/tw eval 'var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); var p=h.Local.World.Position[N]; return p.x+" "+p.z;'
Tools/tw eval 'return TW.Editor.CaptureRig.Shot("C:/abs/Captures/marks.png", X, Z, 12f, 21f, 80f);'
```
**Did it get brighter?** The canonical number is `luma_mean` (and `luma_p95`) in the JSON beside each CaptureRig
still. Take the "before" on the old code first (commit or stash your change, capture, re-apply), use the same pose
and `CaptureRig.Hold()` for both, and capture the unchanged build twice to know the noise. `shotstats.py` gives the
same numbers for any PNG and compares two of them.
It returns `queued ...`; the PNG and a `.json` beside it land a few frames later. Verified 2026-09-25; the JSON holds
`luma_mean`, `luma_p95`, `blown_frac`, `men_in_frame`, `contrast_median`, `rain`, `drawn_infantry` and the camera pose.
`Captures/` is ignored by git.

**Whatever the game view shows right now:**
```bash
Tools/tw shot name        # writes Tools/flame-shots/name.png (never leaves a PNG or .meta inside Assets)
python Tools/shotstats.py Tools/flame-shots/name.png
```
Verified 2026-09-25, standard view at night:
```
Tools/flame-shots/nav-check.png  1600x900
  luma mean 43.9  p5 16  p50 39  p95 87  blown 0.01%  black 0.0%
  warm 2.14%  warm span 1600 px
  grid  41  40  43 /  36  49  48 /  34  47  57
```
Compare two captures: `python Tools/shotstats.py after.png before.png` adds the mean change, the share of pixels
that moved, and where. Rain and wind move 1-3% of pixels between identical poses: freeze first with
`TW.Editor.CaptureRig.Hold()` (and `CaptureRig.Release()` after), or compare the grid rather than the diff.

**Other capture tools** (all in `Editor/`): `TW.Editor.CaptureRig.Series` / `Sheet` / `Diff` / `Bench`,
`TW.Editor.TankCapture.Spawn(team, archetype, x, z)` then `Shot` or `Follow(slot)`, and
`TW.Editor.HudCapture.Shoot(path)`, the only path that includes the HUD.

Staging traps:
- Move the camera with `TacticalCamera.FrameFrom(new Vector2(x, z), zoom, yaw)`. Setting `Camera.main.transform`
  does nothing; the controller overwrites it every frame. For a fixed shot, disable `TacticalCamera` in the same eval.
- `CaptureRig.Pose` clamps zoom to `TacticalCamera.ZoomMin` (6). Lower `ZoomMin` for super close-ups.
- A spawned unit is moved to its deploy zone by the sim. Read `World.Position[slot]` back before aiming.
- Freeze a live scene with `SimHost.TimeScale = 0` (and `Time.timeScale = 0` for effects), then put both back.
- Ground height at a point: `TW.Presentation.RenderGround.Sample(map, x, z)`. The ground sits well above y = 0.
- Pausing the editor stops per-frame effects (ground marks, flipbooks) from drawing. Do not diff paused frames.
  `CaptureRig.Hold()` is different: it stops game time but the editor keeps rendering, so marks still draw.
- `CaptureRig.Hold()` freezes time. Let the sim step a few ticks first, or the presenter has nothing to interpolate
  and every vehicle draws at the origin.
- Anything that writes the sim from eval goes through `SimHost.WriteWorlds(...)`. `AlignWorlds()` sometimes
  answers "worlds a tick apart": wrap a scripted spawn, impact or hold in a retry loop, or the check films nothing.

**Timed effects** (last verified 2026-09-25 by the flamethrower session, not re-run here):
```bash
Tools/tw pause true       # pause BEFORE creating the effect
Tools/tw eval '<create the effect>'
Tools/tw stepto 1.5       # step until game time advanced 1.5 s (not a frame count)
Tools/tw step 12          # or exactly 12 frames
Tools/tw shot name
Tools/tw pause false
```
Each CLI call is a second or two of wall clock and the editor only stays paused while inside one, so an effect
created before pausing runs out between calls. `Tools/flameshots <prefix>` is the worked example.

**Repeatable captures.** Anything animated on noise or `Time.time` looks different at a different absolute time,
so two captures of "the same moment" can differ more than the change you are judging (a ninefold spread was
measured on 2026-09-25). Seed `UnityEngine.Random.InitState(...)`, and use `Tools/tw stepabs <seconds>` to create
the effect and to sample it at fixed absolute game times. Before believing a difference between two rounds,
capture the same build twice and treat anything inside that spread as noise. `python Tools/flamecheck.py <png>`
refuses a fire capture with no fire in it. In a player build, PerfBench's `shot_tick=N shot_hud=0` gives stills
that repeat bit for bit (the HUD animates on real time).

A paused game view does not repaint. After changing anything, step one frame before capturing.

## 7. Windows build and benchmark

Last verified 2026-09-23 by the performance pass; not re-run on 2026-09-25.
- Build from an open editor, in the form the perf pass used (pin the project first, as in section 2):
  `UNITY_PROJECT_PATH=... unity command --detach eval "return TW.Editor.BuildWindows.Build(true);"`, or the menu
  **TW/Build/Windows Bench**. (`BuildWindows.Queue` relies on `delayCall`, which never fires in a background editor.) Output: `Builds/WinBench[Dev]/TrenchWarfare.exe`.
- Benchmark in the player: `Builds/WinBench/TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 canary=0 shot=<png> out=<json>"`.
  Same options in the editor: `TW.Editor.CaptureRig.Bench("... out=<abs path>.json")` with GreyboxCorridor open.
  Two reports with the same `hash_start` measured the same battle. Look at the `shot=` image before trusting numbers.
- Any new `Shader.Find("TW/...")` must be Always Included or it is missing from the player; ShaderInclusionTests guards it.
- Count allocations with `TW.Perf.AllocProbe`. `GC.GetAllocatedBytesForCurrentThread` reads 0 in Unity.

**Batch without an editor:** `Unity.exe -batchmode -quit -projectPath <checkout>/trench-warfare-3d -executeMethod <Class.Method> -logFile <file>`
(for instance `-executeMethod TW.Editor.AssetScaleAudit.Run`, which writes the asset scale audit to `docs/reference/asset-scale.md`)
runs any static editor method with the editor closed (the Hub install is `C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe`).
In Git Bash, `taskkill /PID` gets its slashes mangled: use `taskkill //PID <n> //F`, and only on a process you started.

## 8. Committing

- Commit `.meta` files with their assets and `Packages/packages-lock.json` with `manifest.json`.
- Change packages only through the `unity-package-management` skill, never by editing `manifest.json`.
- Line endings are mixed (CRLF from git checkout, LF from tools). Strip CR before merging or diffing text by script.
- Never let Python's `subprocess` decode a repo file: `text=True` decodes cp1252 here and mangles UTF-8. Read bytes
  and `.decode("utf-8")`.
- Long inline Python in the Bash tool gets its backslashes mangled. Write patch scripts to a file and run them.
- **A shared file with someone else's uncommitted edits:** stage only your hunks by building the blob from
  `git show HEAD:<path>` plus your change (`git hash-object -w --path <path>`, then `git update-index --cacheinfo`),
  so their work stays in the working tree. Check both: the index has yours only, the working tree has both.
