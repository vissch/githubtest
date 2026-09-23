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
hashes (`SimHost.cs:112`, latched at `:114`, logged at `:115`). That is what makes a desync visible in ordinary play
rather than only in a multiplayer session that does not exist yet.

Not *every* tick, though, and the difference matters. The comparison is gated on `Local.World.Tick ==
Peer.World.Tick`. `LoopbackTransport.Send` draws an independent delivery delay per direction, so with the shipped
latency and jitter one driver routinely steps while the other stalls; every tick spent a step apart is skipped
permanently, not deferred. `LockstepLoopbackTests` does not have this problem because it records per-tick hashes
into a `ReplayRecorder` and compares afterwards; `SimHost` passes no recorder. **A quiet `SimHost` is weaker
evidence than it looks.**

It also means any change made to one world and not the other diverges them permanently. Calling
`World.Spawn(...)` — or setting `Position`, `Hp`, a stance, anything — from `unity command eval` touches
`Local` only. The peer never sees it, the latch fires once, and every observation afterwards is worthless.

To put men on the field from tooling, issue a command: `h.Issue(SimCommand.Deploy(h.Local.World.Tick, team, kind))`.
`IssuePeerCommands` mirrors it, both worlds step it, and the hashes stay equal. If you must poke at a world
directly, expect the desync and do not report it as a finding.

### Writing to both worlds is not enough — they must be aligned, every time

The obvious repair for the above is to make the same write to `Local` and `Peer`. That is still wrong. Between
frames the two worlds can be a tick apart (`SimHost.Update` steps them in the same loop, but either `TryStep` can
decline while it waits on the transport). The same write applied to two worlds on two different ticks **is** a
divergence, constructed by hand.

`SimHost.AlignWorlds()` steps whichever world is behind until the ticks match, and returns false when it cannot
because the network is not ready. `Editor/TankCapture.cs` is the worked example: it calls `AlignWorlds` at **all
three** of its mutation sites and refuses the operation on false — `"worlds a tick apart (waiting on the network):
try again"`.

Note *all three*. Aligning once does not stay aligned: the worlds drift apart again on the very next frame, so
alignment is a precondition of each individual write, not a mode you enter for a session. On 2026-09-23 a session
aligned before its first goal write and not before the two after it, and desynced at tick 5671.

The rule, in full:

1. Prefer a command through `h.Issue`. It needs no alignment and is what the game itself does.
2. If you must write state directly: write to **both** worlds, **immediately after** a successful `AlignWorlds()`,
   **before every single write**, and abort on false.
3. Anything else desyncs the session, and every measurement taken afterwards is worthless.

## The heightfield is not in the compared hash

`SimWorld.Hash()` does not fold in `MapData.Hash`. `DeformationSystem.Hash` folds a checksum of crater *inputs*
and three counters — not the heightfield it wrote. Nav-layer divergence is caught indirectly through
`FlowField.Hash`; **height divergence is caught by nothing until a unit walks on it.**

So anything that writes terrain is outside lockstep, whatever assembly it lives in. Presentation code is safe
today only because it does not write terrain — `RenderGroundGrid.Heights` is `[ReadOnly]`, and the drawn ground is
derived, never authoritative. That safety is a property of the current code, not of the architecture, and it is
the kind that stops being true quietly. The obvious next feature for a walker is a footprint; a footprint is a
dent in the ground. Pushed through a decal it is presentation. Pushed through `DeformationSystem` it is sim state
that two worlds do not compare, and nothing in the gate would say so.

If you write to the heightfield:

- it is sim state, so it belongs to a system, on the fixed tick, from `(seed, tick, systemId, slot)`;
- assert `a.Map.Hash(SimHash.Offset) == b.Map.Hash(SimHash.Offset)` in whatever two-world test covers it, as
  `BattlefieldLockstepTests` does — comparing the map hash is cheap and does not change what the shipped tick
  hash costs;
- the long-term fix is to fold `MapData.Hash` into `SimWorld.Hash()` directly, which is deferred only because it
  changes every recorded hash and invalidates stored replays. Do it at the next replay-format break.

This cost an hour on 2026-09-23. The trap that made it expensive was the arithmetic, not the rule: a `DESYNC at
tick 33` looks like one second of play if you assume 30 ticks a second, and a probe that ran fourteen seconds in
therefore looks innocent. It is not one second. `SimHost.Update` takes at most `max(8, TimeScale * 2)` ticks per
frame, and entering Play here is expensive — terrain build, prop composition, atlas bakes — so the first seconds
run at a few frames each. **Tick number is not wall-clock time.** Read it off `Local.World.Tick`, never off a
stopwatch.

## What the two-world tests do and do not reach

An earlier version of this section claimed nobody had ever run two lockstep worlds on the generated battlefield.
**That was false**, and it is left corrected here rather than deleted, because the mistake is instructive: the
counter-example was 26 lines below the test that was cited as proof, in the same file.

| Test | Map | Two worlds? | Kills anyone? |
|---|---|---|---|
| `LockstepLoopbackTests.TwoPeers_StayInSync…` | greybox corridor | yes, over a lossy transport | not asserted |
| `LockstepLoopbackTests.Stress_ThreeThousand…` | greybox corridor | yes | no — runs `combat: false` |
| `BattlefieldTests.TwoSims_…_ThroughABarrage` | generated battlefield | yes, 900 ticks + two HE barrages | not asserted |
| `BattlefieldTests.Generator_SameParamsSameMap` | generated battlefield | no — one world, built twice | n/a |
| `BattlefieldLockstepTests` | generated battlefield | yes, idle and deploying | no |

The real gap is the last column. **No two-world test anywhere kills a unit.** Death drives `Despawn`, which
returns the slot to a LIFO free list and bumps `Generation` — and slot-index reuse under a divergent kill order is
the textbook lockstep divergence. `TrenchAdvance` is the only command that produces deaths at scale, and it is
also the only path that puts men on the river, the fords, the bridge and the wire belt. The one test that does
`TrenchAdvance` at scale runs `combat: false`.

The second gap is quieter: **the compared value does not include the ground.** `MapData.Hash` is never folded into
`SimWorld.Hash()`, and `DeformationSystem.Hash` folds a checksum of crater *inputs* plus three counters, not the
heightfield it wrote. Nav-layer divergence is caught indirectly through `FlowField.Hash`; height divergence is
caught by nothing until a unit walks on it. So the crater deformation that the generated map adds — the headline
reason for testing that map at all — is outside the hash.
