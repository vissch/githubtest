# Gym design reference (read from the repo at `c43b73f`, 2026-09-28)

Line numbers drift. Search for the symbol when a line number is off, and fix this page.
Paths are under `trench-warfare-3d/Assets/_Project/` unless they say otherwise.

## What the gym stands on
| Need | Existing API | Where |
|---|---|---|
| The man's clip, per tick | `AnimationController.Tick` runs `Decide` for every living man; `Start` (private) starts a clip; `Advance` writes the row and phase | `Presentation/Core/AnimationController.cs` ~226-255 (Decide ~251), `Start` ~369, `Advance` ~866-907 |
| Watching one man | `Follow(slot)` / `TraceText()` (record only, they never steer) | same file ~911-928 |
| Figure of an archetype | `VATRenderer.FigureOfArchetype` (archetype 3 = Sniper, else Soldier); clip → atlas row; `ClipAtlas` | `Presentation/Units/VATRenderer.cs` ~96, ~123, ~188-194 |
| Writing to the sim legally from SHOW | `SimHost.WriteWorlds(Action<...>)` (both worlds, same order), `Issue`, `IssuePeer` | `Presentation/Core/SimHost.cs` ~174-180, ~200, ~203 |
| A quiet battle | `ScriptedPeer`, `PeerAttacks`, `BombardmentOverride` | `SimHost.cs` ~48, ~53-56 |
| Calling an ability the way the benches do | `BenchScenarios.Issue`: tops up silver and clears cooldowns through `WriteWorlds`, then issues SupportFire | `Perf/BenchScenarios.cs` ~117-122, ~144-156 |
| Which abilities the sim takes | `OffMapAbilities.TryGetStats` (false → CommandRejected); `FactionRoster.MayCall` (ParaDrop Brass only) | `Sim/Match/OffMapAbilities.cs` ~159-161, ~193; `Sim/Core/FactionRoster.cs` ~117-120 |
| Unit abilities | Breaker, Leap and Support run on their own. `SpecialAbilitiesSystem.Step` throws, and `CommandType.UnitAbility` has no consumer | `Sim/Units/Breaker.cs`, `Sim/Combat/Leap.cs`, `Sim/Units/Support.cs`, `Sim/Units/SpecialAbilities.cs` ~18 |
| Burning a man | `m.Burning.Ignite(...)` inside `WriteWorlds`; the burning system kills him in its step; `Animation.SetAlight` gives the look | `Sim/Combat/Burning.cs` ~90, ~199; AnimationController ~397 |
| Events to the screen | `EventPump.Frame` (a public list dispatched once per frame), `Events.OnEvent` | `Presentation/Core/EventPump.cs` ~15, ~23, ~66 |
| Who draws which event | `CombatFx` (13 types), `TankRenderer` (13), `PropDestruction` (4) | `Presentation/Camera/CombatFx.cs` ~484-786; `TankRenderer.cs` ~958-1110 |
| Zoom | `TacticalCamera.FrameFrom`; zoom 6-600 | `Presentation/Camera/TacticalCamera.cs` ~77, ~33 |
| Captures | `CaptureRig.Shot`, `Series`, `Pending`, `Hold`, `Release`, batch render, JSON metrics, the `EnterPlaymode` pattern | `Editor/CaptureRig.cs` ~348, ~398, ~356, ~465, ~474; render ~122-136; metrics ~205-292; entry ~592-597 |
| A unit spawn window | TW > Unit Sandbox (over `TankCapture.Spawn`, which writes worlds directly: **do not copy that**) | `Editor/UnitSandbox.cs`; `Editor/TankCapture.cs` ~88-89 |

## The files the gym adds (SHOW lane, `lane/show/gym`)
| File | What |
|---|---|
| `Presentation/Core/AnimationController.Pin.cs` | Partial. `Pin(slot, clip, rate)`, `Unpin(slot)`, `PinnedCount`. A `NativeArray<byte>` of clip + 1 per slot (0 = none), a rate array, and the pinned generation. One guarded call in `Tick` before `Decide`: a living pinned man whose generation is unchanged replays the clip through `Start`; a one-shot restarts after a 0.5 s hold. A death takes over because the alive check comes first. Allocation-free. |
| `Perf/GymCatalogue.cs` | Pure data: the entry lists built from `Enum.GetValues`, the roster and the bands, with per-entry expectations and exclusion reasons. |
| `Perf/GymDirector.cs` | A runtime MonoBehaviour. It quiets the battle, stages each entry through `WriteWorlds`/`Issue`, records events via `Events.OnEvent` and log lines via `Application.logMessageReceived`, and drives the camera. |
| `Editor/Gym.cs` | The TW > Gym window, `Gym.Run(string opts)` and `Gym.CommandLine()`: capture per band via `CaptureRig`, a JPG sheet per entry, the JSON sidecars, `summary.json` and retention. |
| `Tools/gymscore.py` | Diffs two runs and joins `clipcheck.py` by clip. A Tools-only commit, named in `pipelines.md`. |
| Tests | `GymCatalogueTests` and `AnimationPinTests` (EditMode), `GymPlayTests` (PlayMode, one: HeBarrage is accepted and the pin draws). Named in a new `tasks.md` "Gym" row. |
| Docs | The `tasks.md` Gym row (files, tests, how to see it), `code-map.md`, `feature-flags.md` FLAG_EFFECT for `TW_GYM`, `workflow.md` §6 for `Gym.Run`. Every new `.cs` starts with `// Phase:`. |

## Staging per death cause (deaths must come from a system inside a tick)
| Cause | Staging |
|---|---|
| Shot | Victim at Hp 1 (WriteWorlds), then an enemy rifle line in range |
| Blast | `HeBarrage` on him |
| Gas | `ChlorineGas` on him (slow: 40 s timebox) |
| Beam | `Beam` along a heading through him |
| Burning | `Burning.Ignite` via WriteWorlds; the burning system kills in its step |
| Crushed | Best effort: a Maw ordered over him. May fail and flag; the owner may ask the SIM lane for a debug path |

## Estimates (not measured, update after the first run)
About 220 clip entries at 10 men per sheet row, ~10 min; 21 units × 15 s ≈ 5 min; abilities, deaths, effects and
vehicle modules ≈ 10 min. **About 25 min and 60-100 MB per run.** The run aborts when it passes 1 GB or when free disk
falls under 10 GB.

## Defaults taken (Brief 2; the owner may overturn them)
- Editor only. A `-twgym` switch in a development player comes later and is the owner's call; it would need a
  `Debug.isDebugBuild` guard because `Perf/` ships.
- Death clips are pinned on living men for clip review, next to the real per-cause deaths.
- Replayed events are labelled "preview".
- The canary is off by default, since it doubles the sim cost; a canary run is what catches WriteWorlds desyncs.
- Crush death is best effort.
- Raw stills are kept for flagged entries only.
