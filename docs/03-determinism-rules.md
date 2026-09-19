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
- Transcendentals via `SimMath` (LUT sin/cos/atan2, integer sqrt option) so cross-platform behaviour is ours.
- **Phase 0 gate:** cross-platform determinism test (Windows x64 / Linux x64 / macOS ARM64). If strict floats
  diverge, switch `SimMath` to Q32.32 fixed-point; the abstraction keeps the swap local.
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

## Platform gate (Phase 0)
Run `TW → Determinism → Write Platform Report` on each target machine. It steps the greybox sim for 2,000 ticks with
seed `0xC0FFEE` and writes the per-tick hash list to `DeterminismReport-<platform>.txt`. Compare files across
platforms. Record results here:

| Date | Platform A | Platform B | Result | Action |
|---|---|---|---|---|
| (pending) | Windows x64 | Linux x64 | | |
| (pending) | Windows x64 | macOS ARM64 | | |

If any pair diverges: switch `SimMath` to the fixed-point implementation (`SimMath.Fixed`) and re-run. If they still
diverge, restrict matchmaking to same-architecture peers until the source is found.
