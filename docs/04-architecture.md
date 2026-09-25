# Architecture

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

## Sim / Presentation split with three frozen contracts
Simulation never references presentation. Presentation reads interpolated snapshots and an event stream.
The three contracts ([02-contracts](02-contracts.md)) are frozen in Phase 0; every later phase builds against them independently.

## Deterministic sim core = plain Burst Structure-of-Arrays; presentation reads the pose stream
Authoritative state is a `SimWorld` struct of `NativeArray`/`NativeList` keyed by **stable slot indices**
(free-list, generation counter), stepped by Burst jobs at a fixed tick.
- Hashing/snapshotting (desync detection, late join, replays) is a byte walk over contiguous arrays.
- No `EntityCommandBuffer` ordering subtleties in the sim.
- Presentation is plain MonoBehaviours + jobs over `SimPresenter`'s pose stream; **no Entities** (dropped 2026-09-20,
  [11-plan-review](11-plan-review.md) §3 — nothing referenced it and its package pin broke the import).
  `BatchRendererGroup` instancing for non-VAT meshes (vehicles, guns, wire, props). VAT infantry uses
  `Graphics.RenderMeshIndirect` from a job-filled `GraphicsBuffer` — built only if the M1.5 fun gate shows 2,000 units matter.

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

## Sim system order
The order is fixed per tick and lives in code: `ISimSystem.Order` values from `SimSystemOrder`
(`Sim/Core/ISimSystem.cs`), registered in `Sim/Match/MatchSim.cs`. The table, with which systems are live and
which are stubs, is generated into `docs/reference/code-map.md` by `Tools/codemap.py`. (The list that stood here
until 2026-09-25 named four systems that were never built and missed six that were.)

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
