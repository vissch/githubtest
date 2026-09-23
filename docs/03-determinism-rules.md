# Determinism rules

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

## Why
Lockstep multiplayer and replays require every peer to compute a byte-identical `SimWorld` from the same seed and
command stream. Every rule below exists to protect that property.

## Rules
- Fixed **20 Hz** sim tick (`SimConfig.TickRate`); presentation interpolates at display rate.
- `Unity.Mathematics` only; Burst `FloatMode.Strict`, `FloatPrecision.Standard`; no `Mathf`, `Time.*`,
  `UnityEngine.Random`, PhysX, or managed collections in sim. Seeded `Unity.Mathematics.Random` from
  `hash(matchSeed, tick, systemId, slot)`.
- Parallel jobs write only to their own index; reductions are fixed-order.
- Transcendentals via `SimMath` (polynomial sin/cos/atan2 built from +, −, ×, ÷, sqrt under `FloatMode.Strict`) so behaviour is ours, not the platform libm's.
- **Platform decision (2026-09-20):** Windows x64 is the only ship target. The sim is float-based under Burst
  `FloatMode.Strict`; there is **no fixed-point fallback** (`SimMath.Fixed` is a placeholder, not a swap — every sim
  array is `float`/`float3`). Determinism is guaranteed same-build, same-architecture. Online play, if it ships, is x64-only.
- Ragdolls/PhysX are cosmetic, presentation-only, never read back into sim.

## Code review checklist for anything under `Assets/_Project/Sim/`
- [ ] No `using UnityEngine;` (Unity.Mathematics, Unity.Collections, Unity.Burst only)
- [ ] No `Random`, `DateTime`, `Time`, `Environment`, `Dictionary` / `List<T>` (managed) in tick code
- [ ] Every `[BurstCompile]` on a job or static class carries `FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard`
- [ ] Parallel jobs write only to `index`; anything cross-slot goes through a fixed-order single-threaded pass
- [ ] Iteration order is slot order or an explicitly sorted order, never hash-map enumeration order
- [ ] New state arrays are registered in `SimWorld.Hash()` and in `Snapshot` (or explicitly marked transient)
- [ ] `sin/cos/atan2/sqrt/exp` go through `SimMath`
- [ ] Tests: `DeterminismReplayTests` still pass
- [ ] New sim system: a `DeterminismReplayTests` case runs with the system registered, and its arrays are covered by `SimWorld.Hash()`

## Same-build gate (replaces the cross-platform gate)

`TW → Determinism → Write Platform Report` steps the greybox sim for 2,000 ticks with seed `0xC0FFEE` and writes the
per-tick hash list to `DeterminismReport-<platform>-<arch>.txt` (gitignored). Run it on two Windows x64 machines, or
once from the editor and once from a player build, and diff the files. Record results here:

| Date | Build A | Build B | Result | Action |
|---|---|---|---|---|
| (pending) | Editor, Burst on | Player build | | |

A divergence between editor and player usually means a job ran managed in one and Burst-compiled in the other
(check `Burst → Enable Compilation` and that every sim job carries `[BurstCompile]`), not a platform issue.

## Burst must compile synchronously in the sim

Every sim job carries `[BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, ...)]`. Without it the editor runs a
freshly edited job as managed code while Burst compiles in the background, Mono evaluates float maths differently, and two
identical runs diverge (seen 2026-09-20: `SameSeedAndCommands_ProduceIdenticalHashes` failed at tick 9 on a cold cache).
Players are compiled ahead of time and are not affected.

## Never mutate a world from an editor eval

`SimHost` runs **two complete sims side by side**, `Local` and `Peer`, over `LoopbackTransport`, and compares their
hashes every tick (`SimHost.cs:115`). That is what makes a desync visible in ordinary play rather than only in a
multiplayer session that does not exist yet.

It also means any change made to one world and not the other diverges them permanently. Calling
`World.Spawn(...)` — or setting `Position`, `Hp`, a stance, anything — from `unity command eval` touches
`Local` only. The peer never sees it, the latch fires once, and every observation afterwards is worthless.

To put men on the field from tooling, issue a command: `h.Issue(SimCommand.Deploy(h.Local.World.Tick, team, kind))`.
`IssuePeerCommands` mirrors it, both worlds step it, and the hashes stay equal. If you must poke at a world
directly, expect the desync and do not report it as a finding.

This cost an hour on 2026-09-23. The trap that made it expensive was the arithmetic, not the rule: a `DESYNC at
tick 33` looks like one second of play if you assume 30 ticks a second, and a probe that ran fourteen seconds in
therefore looks innocent. It is not one second. `SimHost.Update` takes at most `max(8, TimeScale * 2)` ticks per
frame, and entering Play here is expensive — terrain build, prop composition, atlas bakes — so the first seconds
run at a few frames each. **Tick number is not wall-clock time.** Read it off `Local.World.Tick`, never off a
stopwatch.

## Where the two-world check was not being made

Until 2026-09-23 the lockstep coverage had a hole exactly where the game is actually played:

| Test | Map | Two worlds? |
|---|---|---|
| `LockstepLoopbackTests` | greybox corridor | yes |
| `BattlefieldTests.Generator_SameParamsSameMap` | generated battlefield | no — one world, built twice |
| *(nothing)* | **generated battlefield** | **two worlds** |

`GreyboxCorridor.unity` sets `GeneratedBattlefield = 1`, so every real session ran the untested combination, and
the systems the generated map adds — ambient bombardment, crater deformation, mud, wire, the river — were never
checked for agreement between two peers. `BattlefieldLockstepTests` closes it: two `MatchSim.CreateBattlefield`
worlds, hashes compared every tick, idle and with both sides deploying, with the greybox as a control. On failure
it names the first array that differs, because "Position diverged" and "the garrison's `holder` diverged" send you
to opposite ends of the codebase.
