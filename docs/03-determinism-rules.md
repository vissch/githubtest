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
