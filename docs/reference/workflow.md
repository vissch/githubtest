# Workflow: run it, test it, see it

Commands to run, what they print, and what the output means. Run from `trench-warfare-3d/` in Git Bash unless a
command says PowerShell. Pasted outputs are real, from the date given.

## 1. Land checks (every session, first)

```bash
python Tools/health.py
```
Output (2026-09-27):
```
lock     free
memory   15.6 GB commit headroom, 1.3 GB RAM available
editor   none connected for this checkout (Tools/tw up opens one)
validate OK
branch   lane/show/maint-2026-09  lane show  ahead 0 behind 0 of origin/claude/trench-warfare-2d-3d-plan-idt7lf  0 uncommitted
inbox    7 notes, 1 for you
         FOR YOU docs/inbox/2026-09-27-all-rebase-onto-maintenance-pass.md
```
- `lock held` with an editor that is not yours: do not gate, do not write into `Assets/` (a save recompiles their
  editor and kills their Play session). `lock free`: nobody has this checkout. `lock unknown`: the probe failed;
  treat it as held.
- `lane NONE`: stop and work out your lane (`CLAUDE.md`).
- `validate FAILED`: read the lines under it. `codemap:` lines are docs that no longer match the code
  (`Tools/codemap.py` explains each rule). After changing any tool under `Tools/`, run
  `python Tools/selftest.py`: it breaks a throwaway copy of the repo on purpose and checks each break is still caught.
- `python Tools/scorecard.py [--selftest] [--history FILE]` measures the docs, code and tools (reading cost, unrouted
  files, big files, `SceneHooks` references, explained statics, last gate counts). With a history file it prints
  every metric worse than the last clean run, on every run until fixed; `--accept` records a deliberate one.
- Then read the notes `health.py` marks as yours (`docs/inbox/`).

## 2. The machine you share

Several Claude sessions and the owner work on this workstation at once, each in its own checkout. See who, on
which branch, and where your branch would collide with theirs (2.4 s, nothing checked out):
```bash
python Tools/health.py --lanes
```
```
githubtest-sim  [lane/show/units-meta]  +19 -5 vs integration
  09-25 20:42  The silent defaults stop arming and armouring things that are not there
  CONFLICTS with yours in 3: TankCapture.cs, TankRenderer.cs, Selection.meta
```
Rules that cost real time when broken:
- **One editor per checkout.** Never open a second editor on a path that has one.
- **Memory is the scarce resource.** An editor in Play holds 5-9 GB of a 16 GB machine. `python Tools/health.py`
  prints commit headroom, which is what runs out and kills editors (low free RAM only means paging): under 6 GB run
  nothing, under 10 GB a batch gate but no new editor. If it is low, find the holder with
  `Get-Process | Sort-Object PagedMemorySize64 -Descending` (PowerShell) and ask the owner before closing anything.
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
| `Main thread operation timed out after 5000ms` | main thread busy (first seconds of Play build the battlefield, or a compile) | retry after 10 s; for a long eval give a timeout in seconds: `Tools/tw eval '<code>' 90` |
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
With an error:
```
    "compilationFailed": true,
    "consoleErrors": 1,
      "message": "Assets\\_Project\\Presentation\\Core\\ZzProbe.cs(2,76): error CS0029: Cannot implicitly convert type 'string' to 'int'",
```
A `Failed to handle /api/exec request` line during a build is `tw build` polling while the editor compiles:
harmless. Unity keeps running the last good assemblies after a failed compile, so Play and captures still work and
silently show the old code. Check `compilationFailed` before believing anything you see.

No editor of your own? The offline Roslyn compiler lives on `lane/show/aosa` until it merges. Fetch it for the
run and delete it after (the aosa lane owns it), pointing it at your own built dlls:
```bash
mkdir -p Tools/aosa && git show lane/show/aosa:trench-warfare-3d/Tools/aosa/occ.py > Tools/aosa/occ.py
TW_LIB="$PWD/Library/ScriptAssemblies" python Tools/aosa/occ.py TW.Presentation.Core TW.Presentation.Camera
rm -rf Tools/aosa
```
Name every assembly you changed, in dependency order, or it compiles against yesterday's dlls. It prints
`occ OK <assembly> (N files)` or the compiler errors. It cannot check shaders, USS or scenes.

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
(`TW.Tests.CombatTests.Garrison_ShootsAnAssaultInTheOpen_AndWinsTheExchange`) runs one. PlayMode in the editor: add `--async_tests true`. The in-editor runner has hung the pipeline after about four
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
| 8 | a test failed (the xml decides, even if unity exited 0); each failed test is printed with its message |
| 6 | no verdict: compile error, licence, or a suite in which no test ran or none passed. Not a pass |
| 5 | validate.py failed; its lines are printed (`codemap:` lines are docs that no longer match the code) |
| 3 | the checkout is held by an editor or another batch run |
| 1 | unity.exe is missing |
| other | unity's own exit code |

Each suite prints one line of what ran, and keeps its results beside `test-results.xml` (verified 2026-09-27):
```
EditMode : 343 run, 343 passed, 0 failed, 0 skipped (test-results-EditMode.xml)
```
EditMode takes a few minutes. After a full gate `test-results.xml` holds only PlayMode.

### False reds
- **After Play in the same editor:** statics not reset by `SceneStatics` (the `Explained` list in
  StaticLifecycleTests) survive leaving Play. `RequestScriptReload` and rerun; a red that survives that is real.
- **External pipeline noise** in a batch run: `Unhandled log message: '[Error] WriteToProjectRoot failed: Sharing
  violation on path ...\.unity-pipeline-port'` or `'[Error] Failed to handle /api/exec request: Main thread
  operation timed out'`. Another session's CLI or the MCP server reached the batch run's pipeline port, and the
  logged error failed whichever test was running (a different one each run, seen twice on 2026-09-25). The gate
  reruns the failed tests once when every failure is one of these and none is an assertion.
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
and `CaptureRig.Hold()` for both, and capture the unchanged build twice to know the noise. `shotstats.py` measures
any PNG and compares two of them, but on its own scale (0-255, Rec.601 luma, blown at 250 and up), not CaptureRig's
(0-1, Rec.709, blown above 0.90). Never compare a number from one with a number from the other.
It returns `queued ...`; the PNG and a `.json` beside it land a few frames later. Verified 2026-09-25; the JSON holds
`luma_mean`, `luma_p95`, `blown_frac`, `men_in_frame`, `contrast_median`, `rain`, `drawn_infantry` and the camera pose.
`contrast_median` (and `contrast_p10`) answer "can you still see the men": each man's brightness against the ground
around him, where 0.4 means you can find him and 0.1 means he is mud. Use them, with `men_in_frame`, to judge smoke,
fog or tints over the infantry.
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
refuses a fire capture with no fire in it. On `lane/show/aosa` (until "AOSA C33: repeatable player stills" lands), PerfBench's `shot_tick=N shot_hud=0` gives stills
that repeat bit for bit (the HUD animates on real time).

A paused game view does not repaint. After changing anything, step one frame before capturing.

**Every editor entry point.** Each `public static string` method under `Editor/` is meant for `Tools/tw eval` and
returns what the call prints. This table is generated from the code (`python Tools/codemap.py`), so a helper added
today is listed today. Read the method's own comment for its arguments.

<!-- gen:eval-api -->
| Call | File | What it does |
|---|---|---|
| `TW.Editor.BuildWindows.Queue(development)` | Editor/BuildWindows.cs | Schedules a build for the next editor tick and returns at once, so a `unity command eval` does not hold the command server for the minutes a first ... |
| `TW.Editor.BuildWindows.Build(development)` | Editor/BuildWindows.cs | (no summary: read the method) |
| `TW.Editor.CaptureRig.Shot(path, x, z, zoom, yaw, pitch, w, h)` | Editor/CaptureRig.cs | Queue one still. |
| `TW.Editor.CaptureRig.Pending()` | Editor/CaptureRig.cs | How many shots are still to be taken; 0 means the set is finished and the camera is back. |
| `TW.Editor.CaptureRig.ShotCrowd(path, zoom, yaw, pitch, w, h, cell)` | Editor/CaptureRig.cs | A still of the thickest knot of men, so a capture contains soldiers without anyone guessing at coordinates. |
| `TW.Editor.CaptureRig.Series(dir, stem, x, z, zoom, yaw, pitch, count, everyFrames, w, h)` | Editor/CaptureRig.cs | A run of stills from one pose, `everyFrames` apart, so a thing that only exists over time — a shell's smoke column climbing and leaning off, a body ... |
| `TW.Editor.CaptureRig.Sheet(dir, stem, outPath, cols, cellW)` | Editor/CaptureRig.cs | Tiles a series into one image, because eight PNGs opened one after another is not a sequence you can see. |
| `TW.Editor.CaptureRig.Hold(weatherClock)` | Editor/CaptureRig.cs | Stops the clock and pins the weather. |
| `TW.Editor.CaptureRig.Release()` | Editor/CaptureRig.cs | Lets the world run again and gives the weather back its own clock. |
| `TW.Editor.CaptureRig.Diff(pathA, pathB, outPath)` | Editor/CaptureRig.cs | Compares two stills pixel for pixel and says what moved. |
| `TW.Editor.CaptureRig.LastReport()` | Editor/CaptureRig.cs | The measurements from the last still, so a script can read them without opening the file. |
| `TW.Editor.CaptureRig.Bench(args)` | Editor/CaptureRig.cs | Runs TW.Perf.PerfBench in the editor on the open GreyboxCorridor: the same fixed battle, camera, sky and report the Windows player measures with ... |
| `TW.Editor.CaptureRig.Profile(path, frames)` | Editor/CaptureRig.cs | Samples the frame cost over `frames` frames and writes it as json. |
| `TW.Editor.CaptureRig.Pigments(path, cell)` | Editor/CaptureRig.cs | Lays every painted surface out side by side as one PNG, each tiled 2x2 so the repeat is visible. |
| `TW.Editor.CaptureRig.Textures(path, top)` | Editor/CaptureRig.cs | Every texture in memory, largest first, with what it really costs, and a count of the distinct materials and shaders the props are drawn with. |
| `TW.Editor.CaptureRig.Stress(unitsPerSide, path, frames, settleSeconds)` | Editor/CaptureRig.cs | Profiles the game with `unitsPerSide` riflemen deployed by EACH side (so 1000 is the documented 2,000-man stress preset), then puts the scene back as ... |
| `TW.Editor.EnvPropEditing.LearnLooks()` | Editor/EnvPropEditor.cs | Makes each kind's look from the hand edits (the owner's way of setting them, 2026-09-22): the scale the edited props were given becomes the kind's ... |
| `TW.Editor.InkLinesSetup.Install()` | Editor/InkLinesSetup.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Shot(path, w, h)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Follow(slot, zoom, yaw)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Silver(amount)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Spawn(team, archetype, x, z, yawDeg)` | Editor/TankCapture.cs | A unit at an exact place in every world, with the stats its archetype has in the roster (either side's: each side fields different machines). |
| `TW.Editor.TankCapture.Ignite(slot, fire)` | Editor/TankCapture.cs | Set something burning in both worlds (to watch a fire, a bail-out and a cook-off). |
| `TW.Editor.TankCapture.Status()` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.HudCapture.Shoot(path, width, height)` | Editor/UI/HudCapture.cs | Queue a capture; returns the absolute path the PNG will be written to, or null with a reason logged. |
| `TW.Editor.UiSkinGenerator.FullPath(assetPath)` | Editor/UI/UiSkinGenerator.cs | (no summary: read the method) |
<!-- /gen:eval-api -->

## 7. Windows build and benchmark

Last verified 2026-09-23 by the performance pass; not re-run on 2026-09-25.
- Build from an open editor, in the form the perf pass used (pin the project first, as in section 2):
  `UNITY_PROJECT_PATH=... unity command --detach eval "return TW.Editor.BuildWindows.Build(false);"` (release, to
  `Builds/WinBench/`) or `Build(true)` (Development, to `Builds/WinBenchDev/`); the menu is **TW/Build/Windows Bench
  [(Development)]**. (`BuildWindows.Queue` relies on `delayCall`, which never fires in a background editor.) Each
  build writes `build-info.json` beside the exe (commit, dirty flag, dev flag); the report copies it into `build_info`.
- **Release or Development.** Profiler markers compile out of a release player, so a release report has frame totals
  and an empty `per_tick_ms`. To see which system costs what, bench the Development build or the editor. Never
  compare numbers across the two (`run.build` in the report says which).
- Benchmark in the player: `Builds/WinBench/TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 canary=0 shot=<png> out=<json>"`
  (`Builds/WinBenchDev/` for the Development one). Same options in the editor: `TW.Editor.CaptureRig.Bench("...
  out=<abs path>.json")` with GreyboxCorridor open. Two reports with the same `hash_start` measured the same battle.
  Look at the `shot=` image before trusting numbers. Pass `quality=` always: the default (-1) takes whatever the
  machine's `settings.json` says.
- **Compare two reports:** `python Tools/perfcmp.py before.json after.json`. Noise on one build and one fight (two
  archived runs, 2026-09-23): p50 within about 1%, p95 about 7%, p99, hitch counts and one system's per-tick ms up to
  about 25%. Run each side twice; judge p50 and `per_tick_ms`, not one p99.
- **Before and after across commits** (a regression that came in on the integration branch):
  `git worktree add ../tw-<sha> <sha>`, check `python Tools/health.py` for headroom (the first import of a new
  checkout takes minutes and several GB), build it closed:
  `Unity.exe -batchmode -quit -projectPath ../tw-<sha>/trench-warfare-3d -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev] -logFile <file>`,
  bench both exes with the same options, and check `build_info.commit` in each report. For looks, open an editor on
  that worktree and use `CaptureRig` (section 6). `git worktree remove ../tw-<sha>` when done.
- Any new `Shader.Find("TW/...")` must be in Always Included Shaders or used by a material under
  `Resources/ShaderKeep/`, or it is missing from the player; ShaderInclusionTests guards it.
- Count allocations with `TW.Perf.AllocProbe`. `GC.GetAllocatedBytesForCurrentThread` reads 0 in Unity.

**Batch without an editor:** `Unity.exe -batchmode -quit -projectPath <checkout>/trench-warfare-3d -executeMethod <Class.Method> -logFile <file>`
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

**Crossing a file split during a rebase.** Git cannot follow your edits into code that another branch moved to other
files (as `CombatFx.cs` was split into `CombatFx.*.cs`), so the rebase stops on the old file. At each stop:
```bash
python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs --rebase
```
It keeps the upstream file and places each of your commit's edits where its lines now occur exactly once across the
file and its siblings, and prints where each went. An edit whose lines upstream also changed is a real conflict: it
goes to `CombatFx.cs.port.rej` for you, and a `CHECK` line marks a hunk applied only in part. Then compile, apply the
`.rej` by hand, `git add` the files, delete the `.rej`, `git rebase --continue`. In a merge instead of a rebase, use
`--merge` (it works out which side has the split). `--dry-run` shows the placement without writing.

## 9. Debugging

- **An error in the editor.** `Tools/tw run console` prints the log entries with counts, and `groundTruth` says
  whether the last compile failed; `Tools/tw build` prints compile errors. The full log is
  `%LOCALAPPDATA%\Unity\Editor\Editor.log`. The table in section 3 explains the pipeline's own errors, and
  section 5 the test reds that are not bugs.
- **A sim bug you can see in Play.** Live matches are not recorded: a recorder needs a hash every tick
  (`LockstepDriver` refuses one otherwise) and single player turns hashing off for speed. Reproduce it in an EditMode
  test instead, built the way the game builds it: `SimHost`'s `NewMatch` makes the world from `MatchLaunch.Field(Ground,
  BattlefieldSeed)` with bombardment on and the mission's overrides (`MatchLaunch.Apply`), and the enemy is
  `ScriptedEnemy` (SHOW code) giving orders every tick. SinglePlayerEquivalenceTests drives exactly that:
  `LockstepSession` plus `session.StepOnce(ai)`. A player's order lands on the tick of the next frame sent, so the
  frame rate moves it; if the bug depends on timing, sweep the tick you issue the order on by a few ticks either
  side. DeterminismReplayTests shows recording and replaying. `TW.Editor.TankCapture.Spawn` and
  `SimHost.WriteWorlds` set a scene up in Play when you first need to see it.
- **Is it the sim or the drawing?** Read the sim's state for the unit with `tw eval` in Play
  (`h.Local.World`, the eval pattern in section 6): position, `TrenchId` (-1 = not garrisoned), `PostCell` and `PostKind` (1 firing step, 2
  reserve), `StanceOf`, `Layer` (Surface or Trench). If the sim has him posted in a trench and he is drawn standing
  on the parapet, the fault is SHOW (`SimPresenter`, `AnimationController`, the ground height from
  `RenderGround.Sample`). If the sim has him on the surface, it is SIM: a test and a note in `docs/inbox/`.
  `DebugOverlay` keys: F1 flow-field arrows (`Debug.DrawRay`: Scene view or Gizmos on only), F2 stats and per-trench
  garrison counts, F3 next flow goal, F4 plain capsules instead of the figures.
- **Only in the player.** Its log and `settings.json` are in
  `%USERPROFILE%\AppData\LocalLow\DefaultCompany\trench-warfare-3d\` (`Player.log`; `settings.json` is the same
  file editor Play reads, so a quality or zoom setting follows you into the editor). The player always runs Burst and
  usually a different frame rate, which changes which tick an order lands on. `tw eval` cannot reach a player:
  reproduce in the editor with `Application.targetFrameRate` set to the player's rate, or in a test.
- **A desync.** The canary runs a second world beside yours and compares hashes every `HashInterval` ticks;
  `LockstepSession` records the first tick they differ. `SimWorld.Hash()` is one chain over every array, so it says
  when, not what: to find which array, hash them one by one in both worlds at that tick (no tool does this yet).
  Rules that prevent most desyncs: `docs/03-determinism-rules.md`.
- **Slower than before.** Run the bench (section 7) on both builds with the same options, twice each, and compare
  with `python Tools/perfcmp.py`; section 7 gives the noise and the recipe for an older commit. `per_tick_ms` names
  the system (Development build or editor only; `Sim/Core/PerfMarkers.cs` lists the markers). `FrameBudget` counts
  draws (not all of them: `tasks.md`, Performance). `CaptureRig.Stress` and `CaptureRig.Profile` are rough footprint
  checks: no fixed tick, no hash, not the same fight twice. Do not A/B with them.
- **Looks different than before.** Capture the same pose on both builds and compare with `python Tools/shotstats.py
  after.png before.png`; capture the unchanged build twice first to know the noise (section 6). An older commit:
  section 7, "Before and after across commits". Brightness and grade come from the `BiomeProfile` preset
  (`Exposure`, `Contrast`, `Bloom`), which `Atmosphere` turns into a Volume built at runtime (there is no Volume asset
  to diff), then `Shaders/Toon_URP.shader` and the URP asset in `Settings/`. Start with
  `git log -p --since=<when> -- Assets/_Project/Presentation/Terrain/BiomeProfile.cs Assets/_Project/Presentation/Terrain/Atmosphere.cs Assets/_Project/Shaders Assets/_Project/Settings`.
  There is no dusk look: the biomes are NightMud, Lava and Winter (`BiomeProfile.Biome`).
