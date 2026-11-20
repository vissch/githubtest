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
| A quiet battle | the `ScriptedPeer`, `PeerAttacks` and `PeerUsesSupport` fields (read every tick by `SyncEnemy`), plus `Bombardment.ShellsPerMinute = 0` via WriteWorlds. `BombardmentOverride` is static and read only in `NewMatch`, so it is no use mid-match | `SimHost.cs` ~48-61, ~115, ~191-196 |
| Calling an ability the way the benches do | `BenchScenarios.Issue`: tops up silver and clears cooldowns through `WriteWorlds`, then issues SupportFire | `Perf/BenchScenarios.cs` ~117-122, ~144-156 |
| Which abilities the sim takes | `OffMapAbilitySystem.TryGetStats` (false → CommandRejected); `FactionRoster.MayCall` (ParaDrop Brass only) | `Sim/Match/OffMapAbilities.cs` ~159-161, ~193; `Sim/Core/FactionRoster.cs` ~117-120 |
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
| `Presentation/Core/AnimationController.Pin.cs` | A partial class. `Pin(slot, clip, rate)`, `Unpin`, `UnpinAll`, `PinnedClip` and `PinnedCount`. The arrays are made on the first Pin and disposed from `Dispose` (`DisposePins`). `Tick` runs `if (PinnedCount == 0 \|\| !Pinned(i, ref s)) Decide(...)`. A pinned one-shot rests `PinHold` (0.5 s) and replays. A new generation drops the pin. Allocation-free. |
| `Perf/GymCatalogue.cs` | Pure data. The tabs `Clips` / `Units` / `Abilities` / `Deaths` / `Events` are built from `Enum.GetValues` and the unit table. `GymExpect` is Fires / Rejected / FactionSeat / Covered / Preview / Excluded. `EventHow` has **no default case**. There are six bands. |
| `Perf/GymDirector.cs` | Under `#if UNITY_EDITOR \|\| DEVELOPMENT_BUILD`. `Attach`, `Quiet`, `Begin`/`End` (a Result with events, rejects, errors, canary, desync), `Spawn` via WriteWorlds, `Row`, `Clear` (only the gym's own units), `PlayClip`, `Ability`, `Death(DeathKind)`, `Preview`, `Look`. |
| `Editor/Gym.cs` | `Gym.Run(opts)`, `Gym.CommandLine()`, the `GymRun` coroutine, and the `GymWindow` (**TW > Gym**). |
| `Tools/gymscore.py` | **To build** (G3, a Tools-only commit named in `pipelines.md`). |
| Tests | `GymCatalogueTests` and `AnimationPinTests` (EditMode), and `GymPlayTests` (PlayMode). Each is named in the `tasks.md` "Gym" row. |
| Docs | The `tasks.md` Gym row, `code-map.md`, `feature-flags.md` (FLAG_EFFECT for `TW_GYM` and `-twgym`), `workflow.md` §6 (`Gym.Run`). Every new `.cs` starts with `// Phase:`. |

## Staging per `DeathKind` (deaths come from a system inside a tick)
`DeathCause` (the sim's) has no Shot, and its `Crush` is reserved and never raised: a shot and a crush keep the killer's
slot. The gym therefore lists the presentation's `DeathKind`, which is what the picture shows.
| Cause | Staging |
|---|---|
| Shot | Victim at Hp 1 (WriteWorlds), then an enemy rifle line in range |
| Blast | `HeBarrage` on him |
| Gas | `ChlorineGas` on him (slow: 40 s timebox) |
| Beam | `Beam` along a heading through him |
| Burning | `Burning.Ignite` inside WriteWorlds (a tooling write; `Burning.cs` says presentation never calls it, so it is flagged to the SIM lane; the fallback is the Beam, which sets men alight); the burning system kills in its step |
| Crushed | Best effort: there is no move order for one vehicle, so a Maw of ours is placed 25 m short of him, nose on, and drives for the enemy on its own. It may miss, and the entry flags it |

## Estimates (not measured, update after the first run)
About 220 clip entries at 10 men per sheet row, ~10 min; 21 units × 15 s ≈ 5 min; abilities, deaths, effects and
vehicle modules ≈ 10 min. **About 25 min and 60-100 MB per run.** The run aborts when it passes 1 GB or when free disk
falls under 10 GB.

## Defaults taken (Brief 2; the owner may overturn them)
- **Editor now, a development player later.** G4 adds `-twgym` behind `DEVELOPMENT_BUILD`. When it happens is the owner's
  question Q-B2; the plan builds it either way.
- **Death clips** are pinned on living men for clip review, alongside the real deaths per kind.
- **Replayed events** are labelled "preview".
- **The canary** is off by default. `desync` is then `null`, not false. A weekly run with the canary catches WriteWorlds
  desyncs.
- **Crushed** is best effort.
- **Raw stills** are kept for flagged entries only.
