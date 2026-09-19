# Architecture

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

## Sim / Presentation split with three frozen contracts
Simulation never references presentation. Presentation reads interpolated snapshots and an event stream.
The three contracts ([02-contracts](02-contracts.md)) are frozen in Phase 0; every later phase builds against them independently.

## Deterministic sim core = plain Burst Structure-of-Arrays; Entities = presentation world
Authoritative state is a `SimWorld` struct of `NativeArray`/`NativeList` keyed by **stable slot indices**
(free-list, generation counter), stepped by Burst jobs at a fixed tick.
- Hashing/snapshotting (desync detection, late join, replays) is a byte walk over contiguous arrays.
- No `EntityCommandBuffer` ordering subtleties in the sim.
- Entities 1.3 + Entities Graphics run the **presentation world**: one entity mirrors each visible sim slot;
  BatchRendererGroup instancing for non-VAT meshes (vehicles, guns, wire, props). VAT infantry uses
  `Graphics.RenderMeshIndirect` from a job-filled `GraphicsBuffer`.

## Lockstep and networking
Command frames with 2–3 ticks input delay, per-tick hash exchange, desync dump, deterministic replay files.
Netcode for Entities is prediction-based and unsuited to thousands of units.

## Map data model
| Grid | Cell | Format | Purpose |
|---|---|---|---|
| Heightfield | 1 m | `int16` cm | LoS raycast, cover, crater carving, terrain mesh source |
| Nav grid | 2 m | `byte layer` + `byte cost` | Flow fields, layer bits |
| Spatial hash | 1 m (=2·r) | `NativeParallelMultiHashMap<int,int>` | Separation, ballistics candidates, AoE |
| Gas/smoke field | 4 m | `half concentration` ×2 | Gas DoT, smoke LoS occlusion |

Nav layer bits: `Surface`, `Trench`, `Link` (ladder/ramp/parapet vault), `Blocked`, `Wire`, `Mud`, `Crater`,
`Bunker`. Tug-of-war axis is +Z; player A deploys at Z=0, player B at Z=800.

## Sim system order (fixed, per tick)
1. `CommandSystem` – validate + apply this tick's commands (deploy, orders, abilities)
2. `MissionRunner` – evaluate triggers, spawn waves, emit `MissionTriggerFired`
3. `EconomySystem` – silver income, deployment queue, logistics spawn
4. `FlowFieldManager` – recompute dirty fields (time-sliced)
5. `SpatialHashSystem` – rebuild hash
6. `TargetAcquisition` – staggered scan (1/3 of slots per tick)
7. `DirectFireSystem`, `IndirectFireSystem`, `BlastSystem`
8. `SuppressionSystem`, `StanceSystem`, `TrenchGarrisonSystem`
9. `GasSmokeSystem` – diffuse + damage
10. `DeformationSystem` – apply queued crater stamps, update cost field
11. `SeparationJob`, `MovementJob`, `VehicleKinematics`
12. `SectorControlSystem` – capture timers, win/lose
13. `DeathSystem` – free slots, emit `Death`
14. `SimHash` – hash state for this tick

## Lockstep flow
```
UI/AI ──SimCommand──▶ LockstepDriver ──frame(tick+delay)──▶ transport ──▶ peers
                             │
                     all peers' frames for tick T present?
                             │ yes
                        SimWorld.Step(T) ──▶ hash(T) ──▶ exchange & compare
                             │
                        SimPresenter (interpolate T-1..T) ──▶ UnitPose[] / SimEvent[]
```

## Presentation render tiers (infantry)
| Tier | Distance | Technique |
|---|---|---|
| Hero | 0–12 m | Pooled skeletal meshes (10–20), IK hit reactions |
| Horde | 12–45 m | VAT instanced, 2-frame lerp |
| Distant | 45–100 m | VAT low-poly, nearest frame, alternate-frame update |
| Horizon | > 100 m | 8-direction billboard flipbooks |

## Gas / smoke fields
Two `half` concentration grids at 4 m resolution. Each tick: advect by map wind, diffuse (5-point stencil),
sink into `Trench`/`Crater` cells (×2 accumulation), decay. Read by `HeightfieldRaycast` (smoke attenuation) and
`GasSmokeSystem` (damage, flee, capture block).
