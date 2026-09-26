# Runtime switches and flags

Every switch that changes what the game or the editor does without a code change: command-line arguments,
PlayerPrefs / EditorPrefs / SessionState keys, environment variables, and the static or inspector fields tools set.
The tables are generated from the code by `Tools/codemap.py`; the "Effect" text lives in that script's `FLAG_EFFECT`
and `STATIC_SWITCHES`. A new switch fails `validate.py` until it is described there.

**Two that surprise people:**
- `tw.hud.toolkit` is stored in the Windows registry per machine. If someone pressed F9, that machine shows the old
  IMGUI HUD while every other machine shows the UI Toolkit one.
- `SimHost.Ground` picks the terrain the generator builds, not the look. The biome look comes from the scene's
  `GreyboxTerrainView` / `SceneMood` unless a mission is launched from the menu (`MatchLaunch`). For a visual test
  of winter set both, and put both back: they dirty the scene.

<!-- gen:flags -->
| Switch | Kind | Read at (under Assets/_Project) | Effect |
|---|---|---|---|
| `-twbench` | command-line arg | `Perf/PerfBench.cs:37` | Player/editor arg: run PerfBench with "key=value ..." options and quit. See workflow.md, Benchmark. |
| `-twCanary` | command-line arg | `Presentation/Core/SimHost.cs:106` | Player/editor arg: run the second (peer) sim world as a determinism canary. Off in single player. |
| `-twdev` | command-line arg | `Editor/BuildWindows.cs:75` | Arg to the batch Windows build: make a Development build. |
| `TW.EnvProps.Edit` | EditorPrefs | `Editor/EnvPropEditor.cs:36` | EditorPrefs bool: hand placement of props in the Scene view during Play (EnvPropEditor). Default on. |
| `tw.hud.toolkit` | PlayerPrefs | `Presentation/Core/HudBridge.cs:21` | PlayerPrefs int (registry, per machine): 1 = UI Toolkit HUD, 0 = IMGUI BattleHud. F9 flips it. Default 1. A machine where someone pressed F9 shows the other HUD. |
| `tw.rig.stress` | SessionState | `Editor/CaptureRig.cs:674` | SessionState (this editor session only): CaptureRig stress request carried across a domain reload. |
| `tw.rig.stress.restore` | SessionState | `Editor/CaptureRig.cs:673` | SessionState: the StressUnits value CaptureRig puts back afterwards. |
| `TW_AUDIT_OUT` | environment variable | `Editor/AssetScaleAudit.cs:27` | Environment variable: where -executeMethod TW.Editor.AssetScaleAudit.Run writes the asset scale audit. Default docs/reference/asset-scale.md. |
| `TW_BENCH` | environment variable | `Perf/PerfBench.cs:53` | Environment variable, same as -twbench. Also fires in editor Play if set, so unset it after use. |

Code and inspector switches (static fields or `SimHost` inspector fields):

| Field | Declared at (under Assets/_Project) | Effect |
|---|---|---|
| `CanaryOverride` | `Presentation/Core/SimHost.cs:21` | Test/bench override of the canary (null = use arg/inspector). |
| `BombardmentOverride` | `Presentation/Core/SimHost.cs:41` | Ambient shells/min override set by TestPanel presets; -1 = off. |
| `StressOverride` | `Presentation/Core/SimHost.cs:44` | StressUnits override for CaptureRig/PerfBench; -1 = off. |
| `GeneratedBattlefield` | `Presentation/Core/SimHost.cs:36` | Inspector: generated battlefield (true) or flat playtest map. |
| `PlaytestMap` | `Presentation/Core/SimHost.cs:34` | Inspector: two-line playtest layout when not generated. |
| `Ground` | `Presentation/Core/SimHost.cs:76` | Inspector: which terrain preset the generator builds. Does NOT change the biome look (GreyboxTerrainView / SceneMood do). Set both for a visual test. |
| `UseAnimationController` | `Presentation/Core/SimHost.cs:74` | Inspector: per-man clip ladder on (default) or raw atlas rows. |
| `StressUnits` | `Presentation/Core/SimHost.cs:57` | Inspector: deploy N per side and send both over the top. |
| `DeterminismCanary` | `Presentation/Core/SimHost.cs:19` | Inspector: same as -twCanary. |
| `Strength` | `Presentation/Camera/CameraShake.cs:25` | CameraShake.Strength: 0 turns shake off (settings screen writes it). |
| `Gore` | `Presentation/Camera/DebrisRenderer.cs:153` | Gore slider, 0..1 (settings screen writes it). |
| `ProfileSubscribers` | `Presentation/Core/EventPump.cs:18` | Profiler marker per event subscriber (perf work only). |
| `Disabled` | `UI/HudBootstrap.cs:17` | HudBootstrap.Disabled: tests set it so no HUD is added to their scenes. |
| `Disabled` | `UI/Shell/ShellBoot.cs:14` | ShellBoot.Disabled: tests set it so no menu shell is added. |
| `LegacyOverlayActive` | `UI/HudHotkeys.cs:16` | Whether the IMGUI debug overlay still takes F-keys. |
| `PinnedClock` | `Presentation/Terrain/Atmosphere.cs:46` | Freeze weather/rain at a clock value for repeatable captures; -1 = live. |
<!-- /gen:flags -->
