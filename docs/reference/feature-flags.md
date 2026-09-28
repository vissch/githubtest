# Runtime switches and flags

Every switch that changes what the game or the editor does without a code change: command-line arguments,
PlayerPrefs / EditorPrefs / SessionState keys, environment variables, and the static or inspector fields tools set.
The tables are generated from the code by `Tools/codemap.py`; the "Effect" text lives in that script's `FLAG_EFFECT`
and `STATIC_SWITCHES`. A new command-line flag, pref or environment variable fails `validate.py` until it is
described here. A new static switch does not: add it to `STATIC_SWITCHES` in `Tools/codemap.py` yourself, and
give it a reset (`SceneStatics.Register`) or a reason in `StaticLifecycleTests`, or that test fails.

**Which kind of switch.** Something the player chooses: a field in `Presentation/Core/GameSettings.cs`, applied by
`UI/Shell/SettingsApplier.cs` (GameSettingsTests). A knob for experiments or the bench: a static in the class that
uses it, plus a `BenchOptions` key if the bench should set it. Something a scene or level chooses: a `SimHost`
inspector field, carried by `MatchLaunch.Request` when a mission sets it.

**Two that surprise people:**
- `tw.hud.toolkit` is stored in the Windows registry per machine. If someone pressed F9, that machine shows the old
  IMGUI HUD while every other machine shows the UI Toolkit one.
- `SimHost.Ground` picks the terrain the generator builds, not the look. The biome look comes from the scene's
  `GreyboxTerrainView` / `SceneMood` unless a mission is launched from the menu (`MatchLaunch`). For a visual test
  of winter set both, and put both back: they dirty the scene.

<!-- gen:flags -->
| Switch | Kind | Read at (under Assets/_Project) | Effect |
|---|---|---|---|
| `-twbench` | command-line arg | `Perf/PerfBench.cs` | Player/editor arg: run PerfBench with "key=value ..." options and quit. Options: `Perf/BenchOptions.cs` `Parse`; recipes: workflow.md, section 7. |
| `-twCanary` | command-line arg | `Presentation/Core/SimHost.cs` | Player/editor arg: run the second (peer) sim world as a determinism canary. Off in single player. |
| `-twdev` | command-line arg | `Editor/BuildWindows.cs` | Arg to the batch Windows build: make a Development build. |
| `-twgym` | command-line arg | `Editor/Gym.cs` | Editor arg with -executeMethod TW.Editor.Gym.CommandLine: the gym run's options ("tabs=clips,abilities filter=Fire max=20 bands=close out=<dir>"); the editor exits when the run ends (0 clean, 2 flagged, 1 could not run). |
| `-twknob` | command-line arg | `Presentation/Core/Knobs.cs` | Player/editor arg, repeatable: -twknob name=value sets a run-time knob (`Presentation/Core/Knobs.cs`); wins over TW_KNOBS. |
| `TW.EnvProps.Edit` | EditorPrefs | `Editor/EnvPropEditor.cs` | EditorPrefs bool: hand placement of props in the Scene view during Play (EnvPropEditor). Default on. |
| `TW.Gym.Request` | SessionState | `Editor/Gym.cs` | SessionState key (editor): a gym run asked for before Play, carried over the domain reload; erased when the run starts. |
| `tw.hud.toolkit` | PlayerPrefs | `Presentation/Core/HudBridge.cs` | PlayerPrefs int (registry, per machine): 1 = UI Toolkit HUD, 0 = IMGUI BattleHud. F9 flips it. Default 1. A machine where someone pressed F9 shows the other HUD. |
| `tw.rig.stress` | SessionState | `Editor/CaptureRig.cs` | SessionState (this editor session only): CaptureRig stress request carried across a domain reload. |
| `tw.rig.stress.restore` | SessionState | `Editor/CaptureRig.cs` | SessionState: the StressUnits value CaptureRig puts back afterwards. |
| `TW_AUDIT_OUT` | environment variable | `Editor/AssetScaleAudit.cs` | Environment variable: where -executeMethod TW.Editor.AssetScaleAudit.Run writes the asset scale audit. Default docs/reference/asset-scale.md. |
| `TW_BENCH` | environment variable | `Perf/PerfBench.cs` | Environment variable, same as -twbench. Also fires in editor Play if set, so unset it after use. |
| `TW_BOARD` | environment variable | `Editor/Gym.cs` | Environment variable: the pipeline job board checkout (default ../tw3d-board beside the repo); a gym run appends its recurring flags to its lessons.md. |
| `TW_FFMPEG` | environment variable | `Editor/Gym.cs` | Environment variable: the ffmpeg the gym joins its film= frames with into an mp4 (default: ffmpeg on PATH; none: the frames are kept). |
| `TW_GYM` | environment variable | `Editor/Gym.cs` | Environment variable: the folder gym runs are written in (default %LOCALAPPDATA%\TrenchWarfare\gym). Never a checkout. |
| `TW_KNOBS` | environment variable | `Presentation/Core/Knobs.cs` | Environment variable: run-time knobs, "a=1,b=2" (also \| or ; between entries; `Presentation/Core/Knobs.cs`); a bench report lists every knob it read. |
| `TW_SEATSHEET_OUT` | environment variable | `Editor/RiderLab.cs` | Environment variable: where the RiderLab batch seat sheet (the seats of every crab) writes its images. |
| `TW_SEATSHEET_SCALES` | environment variable | `Editor/RiderLab.cs` | Environment variable: the walker sizes the RiderLab batch seat sheet draws, comma separated (default 1,1.5). |

Code and inspector switches (static fields or `SimHost` inspector fields):

| Field | Declared at (under Assets/_Project) | Effect |
|---|---|---|
| `CanaryOverride` | `Presentation/Core/SimHost.cs` | Test/bench override of the canary (null = use arg/inspector). |
| `BombardmentOverride` | `Presentation/Core/SimHost.cs` | Ambient shells/min override set by TestPanel presets; -1 = off. |
| `StressOverride` | `Presentation/Core/SimHost.cs` | StressUnits override for CaptureRig/PerfBench; -1 = off. |
| `GeneratedBattlefield` | `Presentation/Core/SimHost.cs` | Inspector: generated battlefield (true) or flat playtest map. |
| `PlaytestMap` | `Presentation/Core/SimHost.cs` | Inspector: two-line playtest layout when not generated. |
| `Ground` | `Presentation/Core/SimHost.cs` | Inspector: which terrain preset the generator builds. Does NOT change the biome look (GreyboxTerrainView / SceneMood do). Set both for a visual test. |
| `UseAnimationController` | `Presentation/Core/SimHost.cs` | Inspector: per-man clip ladder on (default) or raw atlas rows. |
| `StressUnits` | `Presentation/Core/SimHost.cs` | Inspector: deploy N per side and send both over the top. |
| `DeterminismCanary` | `Presentation/Core/SimHost.cs` | Inspector: same as -twCanary. |
| `Strength` | `Presentation/Camera/CameraShake.cs` | CameraShake.Strength: 0 turns shake off (settings screen writes it). |
| `Gore` | `Presentation/Camera/DebrisRenderer.cs` | Gore slider, 0..1 (settings screen writes it). |
| `ProfileSubscribers` | `Presentation/Core/EventPump.cs` | Profiler marker per event subscriber (perf work only). |
| `Disabled` | `UI/HudBootstrap.cs` | HudBootstrap.Disabled: tests set it so no HUD is added to their scenes. |
| `Disabled` | `UI/Shell/ShellBoot.cs` | ShellBoot.Disabled: tests set it so no menu shell is added. |
| `LegacyOverlayActive` | `UI/HudHotkeys.cs` | Whether the IMGUI debug overlay still takes F-keys. |
| `PinnedClock` | `Presentation/Terrain/Atmosphere.cs` | Freeze weather/rain at a clock value for repeatable captures; -1 = live. |
<!-- /gen:flags -->
