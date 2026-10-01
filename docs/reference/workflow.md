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
         FOR YOU docs/inbox/2026-09-27-all-lanes-landed.md
```
- `lock held` with an editor that is not yours: do not gate, do not write into `Assets/` (a save recompiles their
  editor and kills their Play session). `lock free`: nobody has this checkout. `lock unknown`: the probe failed;
  treat it as held.
- `lane NONE`: stop and work out your lane (`CLAUDE.md`).
- `validate FAILED`: read the lines under it. `codemap:` lines are docs that no longer match the code
  (`Tools/codemap.py` explains each rule). `validate.py` runs one file per check from Tools/checks/:
  `python validate.py --list` says what each is for, `--only <name>` runs one while you fix it, and a new check is
  a new file there plus its name in `ORDER` in `validate.py`.
- **After changing any tool** (`Tools/`, `validate.py`, `gate.ps1`, `.github/`) run `python Tools/toolcheck.py`, the
  one check of the docs and the tools, no Unity, about two minutes: `validate.py`, `python Tools/selftest.py` (it breaks
  a throwaway copy of the repo on purpose and checks each break is still caught) and every tool's own tests (any
  `test_<tool>.py` under `Tools/`, found by name; `--list` names the parts). The gate runs it in place of
  `validate.py` on a lane that changes a tool, and `land.py` refuses such a lane while it is red.
- `python Tools/land.py [--dry-run]` lands your lane (CLAUDE.md, Integration); the full gate must have gone green on
  the exact commit first, and `toolcheck.py` too when the lane changes a tool.
- `python Tools/scorecard.py [--selftest] [--history FILE]` measures the docs, code and tools (reading cost, unrouted
  files, big files, `SceneHooks` references, explained statics, last gate counts, how long the last run before a
  commit took and whether that is past 300 s). With a history file it prints
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
**No desktop** (a session running as administrator, or one with no screen): a windowed editor never comes up, so `tw up`
waits for nothing. Start a batch-mode editor instead and leave it running; it has the graphics card unless you pass
`-nographics`, so cameras render and captures work:
```bash
"/c/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode -projectPath "$(pwd -W)" -logFile Logs/batch-editor.log &
Tools/tw eval 'UnityEditor.SceneManagement.EditorSceneManager.OpenScene("Assets/_Project/Scenes/GreyboxCorridor.unity"); UnityEditor.EditorApplication.isPlaying = true; return 1;'
```
Verified 2026-10-03 (first import of a new checkout: two minutes). `unity status` does not list a batch editor, so `tw up`
does not see it: an eval that answers is the test. Close it with `Tools/tw eval 'UnityEditor.EditorApplication.Exit(0); return 0;'`.
`Tools/assetboard/gamefilm.py` works this way.

Evaluate C# in the editor. There are no `using` directives: fully qualify everything, and `return` a value.
```bash
Tools/tw eval 'var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); return "tick=" + h.Local.World.Tick + " peer=" + (h.Peer!=null);'
```
Verified 2026-09-25 in Play: `"result": "tick=135 peer=False"`. Single player runs one world, so `Peer` is null.

Longer scripts: write a `.cs` body to a file and run `Tools/tw run eval_file -- --file <absolute path>` (it refuses
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

No editor of your own? The offline Roslyn compiler `Tools/aosa/occ.py` compiles against this checkout's
`Library/ScriptAssemblies`; a checkout with no Library sets `TW_LIB` to another checkout's:
```bash
python Tools/aosa/occ.py TW.Presentation.Core TW.Presentation.Camera
```
Name every assembly you changed, in dependency order, or it compiles against yesterday's dlls. It prints
`occ OK <assembly> (N files)` or the compiler errors. It cannot check shaders, USS or scenes.

Then Tools/otr.py runs the EditMode test modules on those dlls in Unity's own Mono with a stand-in engine (Tools/otr/*.cs:
native memory and jobs, JsonUtility, meshes and FBX import, maths, text assets and .asset files), no editor, no RAM
(`python Tools/otr.py [ClassNameFilter ...] [--module Name,...] [-v]`; `python Tools/gate_scope.py --all` lists the
modules). Nearly all of the sim and presentation suite runs; UI Toolkit
layout, GameObjects, physics and audio still report ENGINE. A strong first filter, never the gate's verdict (no Burst,
no job threads, no pixels). A FAIL caused only by the stand-in goes in `Tools/otr/known.txt` with its reason.

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

**The test modules.** The EditMode tests are one assembly per folder under `Tests/`, so a run can leave out what a
change cannot reach. `python Tools/gate_scope.py --all` lists them.

| Module | Holds | Time |
|---|---|---|
| `Tests/Sim/` | the sim alone (references only `TW.Sim.*`, `TW.Net`, `TW.Data`) | slow: most of the run |
| `Tests/Match/` | whole matches through LockstepSession and ScriptedEnemy (adds `Presentation/Core/`) | slow |
| `Tests/Show/` | Presentation, Perf, the source-text checks | seconds |
| `Tests/UI/` | `TW.UI` | seconds |
| `Tests/Project/` | what needs `TW.Editor` (fresh clone, UI skin, shaders kept in builds) | seconds |

A new test goes in the folder of the highest assembly it needs. `Tests/EditMode/` is the landing folder: a lane cut
before the split (2026-10-02) finds its new tests there after rebasing, they still compile, and `validate.py` prints
the `git mv` line that puts each in its module. A class is still run by name (`--filter TW.Tests.<Class>`): the
namespace did not change.

**The gate** (before every commit). It needs this checkout's editor closed. PowerShell refuses unsigned scripts on
this machine, so call it with a bypass:
```bash
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1                    # validate + every EditMode test + PlayMode
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -EditOnly          # validate + the EditMode modules your lane can reach
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -EditOnly -All     # validate + every EditMode test
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -Module Sim,Match  # validate + exactly these modules
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -EditOnly -Long    # any -EditOnly run, with the Long tests too
powershell -NoProfile -ExecutionPolicy Bypass -File ../gate.ps1 -EditOnly -Plan    # say what would run; run nothing
```
`-EditOnly` skips a slow module only when nothing your lane changed can reach it. It compares the working tree
with the merge-base on origin's integration branch, so a lane that touched the sim runs the sim tests on every commit:

| Your lane changed | Sim | Match |
|---|---|---|
| `Sim/`, `Net/`, `Data/` | runs | runs |
| `Tests/Sim/` | runs | skipped |
| `Presentation/Core/`, `Tests/Match/` | skipped | runs |
| any asmdef, `Packages/`, `ProjectSettings/`, `gate.ps1`, a path the rule does not know | runs | runs |
| any other Presentation folder, UI, Editor, Perf, Resources, Art, Shaders, Scenes, other tests, docs, tools | skipped | skipped |

Every other module always runs, except `TW.Tests.Stills`, which no gate run selects at all (`EXPLICIT_ONLY` in
Tools/gate_scope.py) because every test there is `[Explicit]`; the same `test_modules` check fails when a test in
that assembly is not `[Explicit]`, so nothing there is left unrun. The rule is `NEEDS` in Tools/gate_scope.py, and
`validate.py` (the `test_modules` check) fails when a slow module could reach code the rule does not watch. The
first line of the gate says what it skips and why. A scoped run keeps its results in `test-results-EditMode-scoped.xml`, is no verdict (exit 6) unless
they hold every assembly it asked for, and never counts for landing: only the full gate records a tree. The full run
checks its results against the module list too (`gate_scope.py --all`), and when it cannot get that list it stays
green but records no tree. A run with `TW_GATE_UNITY` set (a stand-in for Unity, for `Tools/selftest.py`) writes a
marker token `land.py` refuses by name.

**The Long tier.** A test that takes over 3 s carries `[Category("Long")]` (`[Test, Category("Long")]`): 47 sim and
match tests on 2026-10-04, two thirds of the run time, most of the determinism and whole-battle tests among them.
Every `-EditOnly` run leaves them out, so the run before a commit stays under 300 s even on a lane that touches the
sim; `-Long` puts them back, and the full gate always runs every test. So a Long test your change breaks shows at the
full gate, not at the commit: after a sim change run the full gate (or `-EditOnly -Long`) before you trust it. The
gate prints how long EditMode took; past 300 s it lists the slowest tests it ran, which are the ones to tag. A test
belongs to the lane of the code it tests, so tag a sim test on a SIM lane.
| Exit | Meaning |
|---|---|
| 0 | green |
| 8 | a test failed (the xml decides, even if unity exited 0); each failed test is printed with its message; a test that ended Inconclusive is red too |
| 6 | no verdict: compile error, licence, a suite in which no test ran or none passed, an unknown `-Module`, or a run whose results miss an assembly the module list holds. Not a pass |
| 5 | validate.py failed (on a lane that changes a tool: `toolcheck.py`, which says which part); its lines are printed (`codemap:` lines are docs that no longer match the code) |
| 3 | the checkout is held by an editor or another batch run |
| 1 | unity.exe is missing |
| other | unity's own exit code |

Each suite prints one line of what ran, and keeps its results beside `test-results.xml` (verified 2026-09-27):
```
EditMode : 343 run, 343 passed, 0 failed, 0 skipped (test-results-EditMode.xml)
```
Inconclusive tests are printed as `INCONCLUSIVE <test>` and are red; Ignored tests as `IGNORED <test>`, which is not.
An Explicit skip only counts.
Every EditMode test takes about eleven minutes on the desktop; without the Long tests about four. After a full gate
`test-results.xml` holds only PlayMode.

**A behaviour change, A/B (2026-10-01).** `BehaviourBenchTests.Report_HowEveryUnitBehaves` (Explicit) plays three
eight-minute matches with the scene's enemy on both seats and scores every unit (machines jammed, spinning, hunting;
men stuck, idle under fire, piled up; deaths in clumps), naming the worst. It is deterministic, so two reports are an
A/B with no noise to measure. From `trench-warfare-3d/`, this checkout's editor closed:
- `python Tools/abtest.py bench --file trench-warfare-3d/Assets/_Project/Sim/Nav/VehicleKinematics.cs --variant try=<copy>.cs`
  runs it on the working tree ("now") and on each variant, and prints each metric side by side with whether every
  seed moved the same way. Any report test that writes `<TW_BENCH_OUT>.json` works (`--filter`). The matches are
  deterministic but chaotic (a change moves every later tick): trust what every seed agrees on, and pass `--seeds
  1,2,3,4,5,6` or more before believing a match-wide number (deaths, who wins, when).
- `python Tools/abtest.py fails-on-old --file <changed file> --filter <new test> [--old HEAD~1]`: the new test must
  pass on the change and fail on the older copy of the file (HEAD's unless `--old` says), or it guards nothing.
The study that finds a behaviour problem (MachineStudy, a match in Play at 4x) is not deterministic; decide with these.

**A visual change, A/B.** `python Tools/abtest.py gym --gym "tabs=scenes,units filter=Maw|Kettle|Barrage" --file <file>
--variant try=<copy>` runs the gym (section 6) on the working tree and on each variant with the same options, prints
per entry and zoom band the share of pixels that changed and every sidecar number that moved, and writes blind pairs
(pairs/<variant>/*.jpg, the two sheets stacked in a random order; the key in key.json, apart). `--noise` runs the working tree
twice first: the floor. The gym runs at a fixed 20 frames a game second (`Gym.CaptureFps`), so the same code draws the
same entries (2026-10-01, measured twice: every scenes entry and band 0.00 % of its pixels, the barrage's closest
band 0.06 %, every sidecar number equal, once the night's star shell and distant guns restarted with the run:
`NightLights.Rewind`). Believe a band that moved well past its floor; judge a pair before opening the key, and a
second time with the halves swapped.

**The night's run (2026-10-01).** `python Tools/nightly.py` runs the behaviour bench (six seeds) and every gym tab on
this checkout, as it stands, and compares them with the night before: each bench metric with whether every seed moved
the same way (a jam, spin, hunting, stuck, idle, piled or clumps number that rose on every seed is a regression), the
gym flags that are new, and per entry and band the pixels that changed (information only: a scenes-only gym run repeats,
a whole run does not yet, measured 2026-10-01 at up to 35 % of a close band between two runs of one commit). The report (report.md, report.json) goes to `%LOCALAPPDATA%\TrenchWarfare\nightly\<stamp>-<sha>\`
(the newest seven kept); exit 0 nothing new, 2 something to look at, 1 could not run. Scheduling it daily is the owner's call.

### False reds
- **After Play in the same editor:** statics not reset by `SceneStatics` (the `Explained` list in
  StaticLifecycleTests) survive leaving Play. `RequestScriptReload` and rerun; a red that survives that is real.
- **External pipeline noise** in a batch run: `Unhandled log message: '[Error] WriteToProjectRoot failed: Sharing
  violation on path ...\.unity-pipeline-port'` or `'[Error] Failed to handle /api/exec request: Main thread
  operation timed out'`. Another session's CLI or the MCP server reached the batch run's pipeline port, and the
  logged error failed whichever test was running (a different one each run, seen twice on 2026-09-25). The gate
  reruns the failed tests once when every failure is one of these and none is an assertion (`Expected:`, `But was:`,
  `Assert.`; the wrapper names `LogAssert.Expect` itself, which does not count).
- **First run after a Burst job edit** can run managed code and differ from the next run. Every sim job has
  `CompileSynchronously = true`; keep it on new jobs. Rerun before trusting a single determinism failure.
- **Stale `Library/BurstCache`** after a job struct changes: NullReference or IndexOutOfRange inside Burst jobs.
  Close the editor, delete it, reopen. It can also fail quietly, as a wrong answer in a job the change never touched:
  on 2026-09-28 a change to `InfantrySpec` and `TankSpec` made `MineTests.AMineUnderALongHullsBowIsTakenOnTheHullNotAsANearMiss`
  fail in Unity (the mine job no longer saw the hull over it) while otr passed; with the cache deleted it passed, twice.
  Suspect it when Unity and otr disagree after a struct in `Sim/` changed shape.

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
refuses a fire capture with no fire in it. PerfBench's `shot_tick=N shot_hud=0` gives stills that repeat bit for
bit: the clock is held and the HUD, which animates on real time, is hidden.

A paused game view does not repaint. After changing anything, step one frame before capturing.

**Every editor entry point.** Each `public static string` method under `Editor/` is meant for `Tools/tw eval` and
returns what the call prints. This table is generated from the code (`python Tools/codemap.py`), so a helper added
today is listed today. Read the method's own comment for its arguments.

<!-- gen:eval-api -->
| Call | File | What it does |
|---|---|---|
| `TW.Editor.AssetScaleAudit.Write(path)` | Editor/AssetScaleAudit.cs | (no summary: read the method) |
| `TW.Editor.BuildWindows.Queue(development)` | Editor/BuildWindows.cs | Schedules a build for the next editor tick and returns at once, so a `unity command eval` does not hold the command server for the minutes a first ... |
| `TW.Editor.BuildWindows.Build(development)` | Editor/BuildWindows.cs | (no summary: read the method) |
| `TW.Editor.CaptureRig.Shot(path, x, z, zoom, yaw, pitch, w, h, aimY)` | Editor/CaptureRig.cs | Queue one still. |
| `TW.Editor.CaptureRig.Pending()` | Editor/CaptureRig.cs | How many shots are still to be taken; 0 means the set is finished and the camera is back. |
| `TW.Editor.CaptureRig.ShotCrowd(path, zoom, yaw, pitch, w, h, cell)` | Editor/CaptureRig.cs | A still of the thickest knot of men, so a capture contains soldiers without anyone guessing at coordinates. |
| `TW.Editor.CaptureRig.Series(dir, stem, x, z, zoom, yaw, pitch, count, everyFrames, w, h, aimY)` | Editor/CaptureRig.cs | A run of stills from one pose, `everyFrames` apart, so a thing that only exists over time — a shell's smoke column climbing and leaning off, a body ... |
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
| `TW.Editor.CrabManifest.MachineOf(assetPath)` | Editor/CrabManifest.cs | The machine a model file belongs to: "Kettle" for .../Kettle_LOD1.fbx. |
| `TW.Editor.EnvPropEditing.LearnLooks()` | Editor/EnvPropEditor.cs | Makes each kind's look from the hand edits (the owner's way of setting them, 2026-09-22): the scale the edited props were given becomes the kind's ... |
| `TW.Editor.Gym.Run(options)` | Editor/Gym.cs | Play the catalogue unattended. |
| `TW.Editor.Gym.Root()` | Editor/Gym.cs | The folder gym runs go in: TW_GYM, else %LOCALAPPDATA%\TrenchWarfare\gym. |
| `TW.Editor.InkLinesSetup.Install()` | Editor/InkLinesSetup.cs | (no summary: read the method) |
| `TW.Editor.LookLab.Knob(name, value)` | Editor/LookLab.cs | A knob set in the game on the lab's next frame. |
| `TW.Editor.LookLab.Clear()` | Editor/LookLab.cs | Every knob back to its default on the lab's next frame. |
| `TW.Editor.RiderLab.Setup(archetype, riders, x, z, team, yawDeg, climb)` | Editor/RiderLab.cs | A walker of `archetype` (6 Pincer .. |
| `TW.Editor.RiderLab.Climbers(slot, n)` | Editor/RiderLab.cs | `n` riflemen of the machine's team spawn 12-15 m behind it, held, and run in and climb aboard (for a machine that is already standing: spawn it ... |
| `TW.Editor.RiderLab.BoardMen(slot, men)` | Editor/RiderLab.cs | Real men take these riders' places: board from where they stand. |
| `TW.Editor.RiderLab.Board(slot, count)` | Editor/RiderLab.cs | Riders appear seated at once (no climb, no sim men behind them). |
| `TW.Editor.RiderLab.Dismount(slot, count)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Unboard(slot, count)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Drive(slot, dz)` | Editor/RiderLab.cs | Walk the machine `dz` metres along its own column (a cell goal), releasing the hold. |
| `TW.Editor.RiderLab.Stop(slot)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Kill(slot)` | Editor/RiderLab.cs | Destroy the machine (its riders are thrown off). |
| `TW.Editor.RiderLab.Enemies(slot, count, ahead, bearingDeg)` | Editor/RiderLab.cs | Enemy riflemen ahead of the machine, held still, so its guns and its riders have a target. |
| `TW.Editor.RiderLab.ClearEnemies(slot, radius)` | Editor/RiderLab.cs | Remove every living enemy of that machine within `radius` metres (so its guns turn to a new group). |
| `TW.Editor.RiderLab.Size(archetype, factor)` | Editor/RiderLab.cs | Draw one crab kind at `factor` times its shipped size (VehicleSize.Walker * factor). |
| `TW.Editor.RiderLab.ClearOfGuns(on)` | Editor/RiderLab.cs | Riders kept out from under the guns' sweep (true) or seated anywhere (false). |
| `TW.Editor.RiderLab.Fire(on)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Freeze(on)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Panel(on)` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Camera(slot, zoom, yaw, pitch)` | Editor/RiderLab.cs | Frame the camera on a slot: zoom, yaw, pitch (the follow keeps it framed while it walks). |
| `TW.Editor.RiderLab.Film(dir, seconds, fps, w, h)` | Editor/RiderLab.cs | Record `seconds` of GAME time as `fps` frames a second into `dir` (a frame is repeated when the editor renders slower). |
| `TW.Editor.RiderLab.VolleyShot(path, shots)` | Editor/RiderLab.cs | Save a still to `path` the frame `shots` more rider shots have been fired (a volley caught as it lands). |
| `TW.Editor.RiderLab.FilmStatus()` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.Status()` | Editor/RiderLab.cs | (no summary: read the method) |
| `TW.Editor.RiderLab.SeatSheet(name, archetype, scale, path, w, h)` | Editor/RiderLab.cs | One picture of a crab at `scale` (a multiple of the sculpt) with a kneeling-man stand-in on every seat, from three-quarter above on the left and ... |
| `TW.Editor.SimProbe.Unit(slot)` | Editor/SimProbe.cs | One unit, sim then picture: what it is, where the sim has it and its trench post, where it is drawn over which ground height, and what it is ... |
| `TW.Editor.SimProbe.Match()` | Editor/SimProbe.cs | The match being played: seed, battlefield seed and ground, bombardment, stress, tick and mission, the values a test needs to rebuild it. |
| `TW.Editor.SimProbe.Arrays(peer)` | Editor/SimProbe.cs | A hash per world array, so the canary's two worlds can be compared array by array at the desync tick: pause, then diff Arrays() with Arrays(true). |
| `TW.Editor.TankCapture.Shot(path, w, h)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Follow(slot, zoom, yaw)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Silver(amount)` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.TankCapture.Spawn(team, archetype, x, z, yawDeg)` | Editor/TankCapture.cs | A unit at an exact place in both worlds, with the stats the match table gives its archetype. |
| `TW.Editor.TankCapture.Ignite(slot, fire)` | Editor/TankCapture.cs | Set something burning in both worlds (to watch a fire, a bail-out and a cook-off). |
| `TW.Editor.TankCapture.Status()` | Editor/TankCapture.cs | (no summary: read the method) |
| `TW.Editor.HudCapture.Shoot(path, width, height)` | Editor/UI/HudCapture.cs | Queue a capture; returns the absolute path the PNG will be written to, or null with a reason logged. |
| `TW.Editor.UiSkinGenerator.FullPath(assetPath)` | Editor/UI/UiSkinGenerator.cs | (no summary: read the method) |
| `TW.Editor.VfxLab.Shell(x, z, radius, damage)` | Editor/VfxLab.cs | One shell, bursting next tick in every world. |
| `TW.Editor.VfxLab.Fire(x, z, radius)` | Editor/VfxLab.cs | An incendiary burst: the men inside it catch fire. |
| `TW.Editor.VfxLab.Call(ability, x, z, args)` | Editor/VfxLab.cs | A support call for player 0 at a point (silver topped up first). |
| `TW.Editor.VfxLab.Men(n, x, z, team, spacing)` | Editor/VfxLab.cs | n riflemen of a side standing still in a line along x from (x, z), facing +z; returns their slots. |
| `TW.Editor.VfxLab.Wreck(archetype, x, z)` | Editor/VfxLab.cs | A machine held where it stands and shelled to death 1.5 s later. |
| `TW.Editor.VfxLab.Burning(archetype, x, z)` | Editor/VfxLab.cs | A machine alight and still alive, held where it stands. |
| `TW.Editor.VfxLab.Flare()` | Editor/VfxLab.cs | A star shell now (night: it lights the field for about 16 s). |
| `TW.Editor.VfxLab.Rain(amount)` | Editor/VfxLab.cs | Rain on the field, 0..1 (0 off). |
| `TW.Editor.VfxLab.Later(seconds, act)` | Editor/VfxLab.cs | Something done `seconds` from now (real time in Play), its answer logged. |
| `TW.Editor.VfxLab.Scene(name, x, z)` | Editor/VfxLab.cs | A whole staging by name at (x, z), the effect's centre. |
| `TW.Editor.WeightLab.Knob(name, value)` | Editor/WeightLab.cs | A knob set in the game on the lab's next frame (Knobs.Set from compiled code). |
| `TW.Editor.WeightLab.Halt(slot)` | Editor/WeightLab.cs | The machine's drive taken away (its Speed to 0): with momentum it sheds its way at its Brake and runs on to a stop, as at a halt. |
| `TW.Editor.WeightLab.Trace(slot, csv, seconds)` | Editor/WeightLab.cs | Traces the machine in `slot` for `seconds` of game time into `csv`. |
| `TW.Editor.WeightLab.TraceStatus()` | Editor/WeightLab.cs | The trace in progress, or, once done, where it went and the stop's numbers: dip, rebound, settled. |
| `TW.Editor.WreckLab.Shell(x, z, damage, radius)` | Editor/WreckLab.cs | One shell at a point in every world, bursting next tick. |
| `TW.Editor.WreckLab.Wreck(x, z)` | Editor/WreckLab.cs | The wreck prop nearest a point within 8 m: "prop N Kind hp H", or "none". |
<!-- /gen:eval-api -->

## 7. Windows build and benchmark

Last verified 2026-09-23 by the performance pass; not re-run on 2026-09-25.
- Build from an open editor, in the form the perf pass used (pin the project first, as in section 2):
  `UNITY_PROJECT_PATH=... unity command --detach eval "return TW.Editor.BuildWindows.Build(false);"` (release, to
  `Builds/WinBench/`) or `Build(true)` (Development, to `Builds/WinBenchDev/`); the menu is **TW/Build/Windows Bench
  [(Development)]**. (`BuildWindows.Queue` relies on `delayCall`, which never fires in a background editor.) Each
  build writes `build-info.json` beside the exe (`git_sha`, `dirty_files`, `development`); the report copies it into
  `build_info`.
- **Release or Development.** Profiler markers compile out of a release player, so a release report has frame totals
  and an empty `per_tick_ms`. To see which system costs what, bench the Development build or the editor. Never
  compare numbers across the two (`run.build` in the report says which).
- Benchmark in the player: `Builds/WinBench/TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 canary=0 shot=<png> out=<json>"`
  (`Builds/WinBenchDev/` for the Development one). `fx=<x> fz=<z> zoom=<z>` holds the camera on one place instead of
  the armies' centre, for a still of a trench bay or a hamlet (`-screen-fullscreen 0 -screen-width 1280
  -screen-height 720` before `-twbench` keeps the player in a window). Same options in the editor:
  `TW.Editor.CaptureRig.Bench("... out=<abs path>.json")` with GreyboxCorridor open. Two reports with the same
  `hash_start` measured the same battle. Look at the `shot=` image before trusting numbers. Pass `quality=` always:
  the default (-1) takes whatever the machine's `settings.json` says.
- **Compare two reports:** `python Tools/perfcmp.py before.json after.json`. Noise on two editor runs of one fight
  (2026-09-23): p50 within about 1%, p95 up to about 12%, p99 and one system's per-tick ms up to about 25%, hitches 2
  vs 5. Run each side twice; trust p50; believe a per-system change only past about 30% and in both runs.
- **Before and after across commits** (a regression that came in on the integration branch):
  from the repo root (outside this checkout) `git worktree add ../tw-<sha> <sha>`, check `python Tools/health.py`
  for headroom (the first import of a new checkout takes minutes and several GB), build it closed:
  `Unity.exe -batchmode -quit -projectPath ../tw-<sha>/trench-warfare-3d -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev] -logFile <file>`,
  bench both exes with the same options, and check `build_info.git_sha` (and `dirty_files` 0) in each report. A
  commit older than 96f4366 has no `BuildWindows`: copy `Editor/BuildWindows.cs` and `Perf/` into the worktree first.
  For looks, open an editor on that worktree and use `CaptureRig` (section 6). `git worktree remove ../tw-<sha>`.
- Any new `Shader.Find("TW/...")` must be in Always Included Shaders or used by a material under
  `Resources/ShaderKeep/`, or it is missing from the player; ShaderInclusionTests guards it.
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
- **One session per checkout.** A second session makes its own (`git worktree add`); two in one tree gate each
  other's half-done edits and cannot rebase.

**Crossing a file split during a rebase**, at each stop:
```bash
python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs --rebase   # --merge in a merge; --dry-run
```
Edits it cannot place for certain go to `CombatFx.cs.port.rej`, and a `CHECK` line marks a hunk applied in part:
apply those by hand, compile, `git add`, delete the `.rej`, `git rebase --continue`.

## 9. Debugging

- **An error in the editor.** `Tools/tw run console` prints the log entries with counts, and `groundTruth` says
  whether the last compile failed; `Tools/tw build` prints compile errors. The full log is
  `%LOCALAPPDATA%\Unity\Editor\Editor.log`. The table in section 3 explains the pipeline's own errors, and
  section 5 the test reds that are not bugs.
- **A sim bug you can see in Play.** Live matches are not recorded: a recorder needs a hash every tick
  (`LockstepDriver` refuses one otherwise) and single player turns hashing off for speed. Reproduce it in an EditMode
  test instead, built the way the game builds it. `SimHost`'s private `NewMatch` makes
  `MatchSim.CreateBattlefield(cfg, MatchLaunch.Field(Ground, BattlefieldSeed))` with `field.Bombardment` from
  `BombardmentOverride` or `BombardmentPerMinute` (0 since 2026-09-28: no constant bombardment) when `GeneratedBattlefield` is set, else `CreatePlaytest` or
  `CreateGreybox`; a mission first writes its values into those fields (`MatchLaunch.Apply` in `Awake`). In a test,
  call `MatchSim.CreateBattlefield` with the match's values yourself. The enemy is `ScriptedEnemy` (SHOW code);
  SinglePlayerEquivalenceTests shows the loop that drives it (`LockstepSession` plus `session.StepOnce(ai)`), on the
  Playtest map without bombardment. A player's order lands on the tick of the next frame sent, so the
  frame rate moves it; if the bug depends on timing, sweep the tick you issue the order on by a few ticks either
  side. DeterminismReplayTests shows recording and replaying. `TW.Editor.TankCapture.Spawn` and
  `SimHost.WriteWorlds` set a scene up in Play when you first need to see it.
- **Is it the sim or the drawing?** In Play, `Tools/tw eval 'return TW.Editor.SimProbe.Unit(N);'` prints the unit's
  sim state and its picture side by side: position, `TrenchId` (-1 = not garrisoned), `PostCell` and `PostKind` (1
  firing step, 2 reserve), `StanceOf`, `Layer` (Surface or Trench), then where it is drawn, the ground height there,
  its pose and its animation clip. `SimProbe.Match()` prints the seeds, ground, bombardment and mission a test needs
  to rebuild the match. Why the animation chose a clip, tick by tick: `h.Animation.Follow(N)`, let it run, then
  `return h.Animation.TraceText(80);`. A slot is reused after a death: check `generation` has not changed between
  reads. If the sim has him posted in a trench and he is drawn standing
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
  when, not what: pause at that tick and compare `TW.Editor.SimProbe.Arrays()` with `SimProbe.Arrays(true)` (the
  canary's world), one hash per array.
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
