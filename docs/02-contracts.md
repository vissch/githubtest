# Frozen contracts (Phase 0)

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

These three contracts are frozen at the end of Phase 0. Changing any of them requires a version bump of the
replay file format and a note in this file. Every track builds against them independently.

### 3.1 Command contract
```
struct SimCommand { uint tick; byte player; CommandType type; int a; int b; float3 pos; }
enum CommandType {
  DeployUnit(slot=a),                  // spend silver, queue at spawn point
  TrenchAdvance(trenchId=a),           // ">>"
  TrenchSelectAdvance(trenchId=a, unitMask=b), // roster "↑" partial advance
  TrenchLock(trenchId=a, on=b),        // lock icon
  TrenchFallback(trenchId=a),          // "↩"
  UnitStance(unitSlot=a, stance=b),    // 3D addition: prone/crouch/sprint override
  SupportFire(abilityId=a, pos, b=heading), // HE, gas, bomber, smoke, creeping barrage
  UnitAbility(unitSlot=a, abilityId=b, pos), // officer smoke call, bangalore, etc.
  SetRally(pos),                       // logistics: where reinforcements collect
  Surrender
}
SimWorld.Step(NativeSlice<SimCommand> cmdsForThisTick)
```
Commands are validated in-sim (cost, cooldown, ownership); invalid ones are dropped deterministically.

### 3.2 Map data contract
`MapData` blob: dims, cell sizes, heightfield, nav layers/cost, trench definitions (`TrenchDef`: id, side,
ordered traverse cells, fire-step cells, links), spawn points, supply road polyline, sector objectives
(`ObjectiveDef`: id, type OutpostLine/MainLine/ReserveLine/HQ, cells, owner), wire belts, mud polygons,
static emplacements, scripted trigger volumes.

### 3.3 Pose + Event stream contract
```
struct UnitPose { float3 pos; half yaw; ushort animRow; half animT; byte archetype; byte flags; byte lod; byte team; }
enum SimEventType { Shot, Hit, NearMiss, Explosion, CraterStamp, Death, StanceChanged, Suppressed, Pinned,
  UnitSpawned, UnitEnteredTrench, UnitLeftTrench, TrenchCaptured, ObjectiveCaptured, GasCloudSpawned,
  SmokeSpawned, VehicleTrackHit, VehicleStalled, VehicleDestroyed, WireBreached, AbilityFired, WaveStarted,
  MissionTriggerFired }
struct SimEvent { uint tick; SimEventType type; int a; int b; float3 pos; float3 dir; float scalar; }
```
Presentation consumes the ring buffer once per frame; audio, VFX, UI, ragdolls and mission dialogue all hang
off it. Mission scripting runs **inside** sim (deterministic) and emits `MissionTriggerFired`.

## Command validation rules (sim side)

| Rule | Behaviour on violation |
|---|---|
| `player` must own the trench / unit slot referenced | command dropped, `CommandRejected` debug counter incremented |
| `DeployUnit` requires silver ≥ cost and slot unlocked for this mission | dropped |
| `SupportFire` / `UnitAbility` require cooldown expired and ability unlocked | dropped |
| `pos` must lie inside the map bounds | clamped to bounds |
| Commands for a tick are sorted by `(player, arrivalIndex)` before execution | guarantees identical order on every peer |

Dropping is deterministic because validation reads only sim state, never presentation state.

## Event semantics

- Events are append-only within a tick and cleared at the start of the next `Step()`.
- `a` / `b` carry slot indices, trench ids, objective ids or ability ids depending on `type` (see the enum comments in `SimEvents.cs`).
- `scalar` carries damage, concentration or radius; `dir` carries shot direction or wind.
- Presentation must tolerate dropped events (ring buffer overrun is reported via `SimEvents.Overrun`) and rebuild from state, never rely on events for correctness.

## Change log

| Date | Change | Replay format |
|---|---|---|
| 2026-09-20 | Replay header gains `FormatVersion` (ushort), `MapHash` (ulong, `MapData.Hash()`), `DataHash` (ulong, baked stat tables; 0 until C2 wires it). `ReplayPlayer.Parse` rejects a mismatched format version. | v1 → **v2** |
| 2026-09-20 (M1) | Map data: `TrenchDef.WidthMeters` (tracked vehicles cross a trench ≤ 3.5 m), `MapData.CellTrenchId` (nav cell → trench id, derived), `MapData.Version` (bumped on every nav/height mutation; the tick hash covers it instead of the arrays). Per-slot state: `SimWorld.SourceTrench` (trench a unit last left under orders; the `↩` target), hashed. Layout unchanged, but hashes of replays recorded before this commit no longer verify. | v2 (hash content) |
| 2026-09-20 (playtest) | Deploy spawn jitter now draws from one RNG stream per deploy (`player + 16 * n`, n = that player's deploys this tick) instead of one per (tick, player); same-tick deploys no longer share a spawn point. `SeparationJob` gives coincident pairs antisymmetric normals and caps the push at 6 m/s. Layout unchanged; replays recorded before this commit no longer verify. | v2 (hash content) |
