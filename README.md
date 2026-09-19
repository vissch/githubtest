# Trench Warfare 3D

Roadmap and project skeleton for rebuilding *Trench Warfare 1917* (2D lane RTS) as a 3D tactical title in
**Unity 6 LTS** with a deterministic lockstep simulation.

## Repository layout
| Path | What |
|---|---|
| `docs/` | The phased plan: architecture, contracts, determinism rules, units, abilities, missions, budgets |
| `trench-warfare-3d/` | Unity 6 project skeleton: assembly definitions, Phase 0 implementation, compilable stubs for every later phase, tests |
| `github-test1/` | Earlier Unity 2021.3 rendering test project, unrelated to the game, left untouched |

## Docs index
| Doc | Content |
|---|---|
| [00 Overview](docs/00-overview.md) | Vision, constraints, milestones |
| [01 Phases and dependencies](docs/01-phases-and-dependencies.md) | Systems per phase, dependency graph, parallel tracks, single- and two-developer schedules |
| [02 Contracts](docs/02-contracts.md) | Command, map data, pose/event contracts (frozen in Phase 0) |
| [03 Determinism rules](docs/03-determinism-rules.md) | Coding standard for the sim, review checklist, platform gate |
| [04 Architecture](docs/04-architecture.md) | Sim/presentation split, system order, lockstep flow, render tiers |
| [05 Performance budgets](docs/05-performance-budgets.md) | CPU/GPU/memory budgets, banned anti-patterns, profiling procedure |
| [06 Units and factions](docs/06-units-and-factions.md) | Every unit class, vehicle, faction roster, emplacement, trench command, stance |
| [07 Abilities](docs/07-abilities.md) | Unit abilities and off-map support abilities |
| [08 Missions](docs/08-missions.md) | Mission framework and the three missions (Ypres, Somme, Cambrai) |
| [09 Game modes and meta](docs/09-game-modes-and-meta.md) | Campaign, Survival, Operations, Sandbox, upgrades, skins |
| [10 Risks](docs/10-risks.md) | Risk register |

## Opening the Unity project
1. Install **Unity 6000.0 LTS** (any 6000.0.x patch; `ProjectSettings/ProjectVersion.txt` names the one the skeleton was written for).
2. Open `trench-warfare-3d/`. Package versions in `Packages/manifest.json` are known-good pins for Unity 6 LTS;
   if the Package Manager reports a newer patch, accepting it is fine. The first import generates `Library/` and `.meta` files.
3. Run **TW → Build Bootstrap Scenes** to generate `Bootstrap.unity` and `GreyboxCorridor.unity`.
4. Open **Window → General → Test Runner** and run EditMode and PlayMode tests.
5. Press Play in `GreyboxCorridor`: capsule units flow along the corridor under the debug overlay.

## What is implemented vs. stubbed
| Implemented (Phase 0 / M1 groundwork) | Stubbed with `// Phase:` header |
|---|---|
| `SimWorld` tick loop, slot allocator, economy, command validation, state hash, seeded RNG, platform-safe math | Direct/indirect fire, suppression, armour, gas/smoke, deformation, trench orders, garrison, stances |
| Map data contract, heightfield, greybox corridor generator | Wire, mud, crater stamps, map authoring tool |
| Layered flow field (trench/surface/link), spatial hash, separation, movement | Flow-field goal manager, vehicle kinematics |
| Heightfield line-of-sight raycast | Target acquisition, blast, burning |
| Lockstep driver + loopback transport with latency/jitter/loss, replay recorder/player | Unity Transport session, hash exchange, snapshots |
| Presenter (pose interpolation), event pump, tactical camera, debug overlay, greybox terrain view | VAT renderer, ragdolls, terrain chunks, VFX, audio, UI |
| ScriptableObject schemas, data baker, slice roster generator (British, German, French vehicles, all off-map abilities) | Mission runner, wave AI (schemas exist) |
| EditMode tests (determinism, replay, hash, flow field, raycast, commands) and PlayMode lockstep tests | — |

`trench-warfare-3d/validate.py` checks the skeleton without Unity (JSON, assembly graph, sim purity, phase headers).
The Entities and Entities Graphics packages are in the manifest but no assembly references them yet; B3 adds the
references when the presentation world needs them.

## Working in phases
Every assembly under `Assets/_Project` maps to a phase in [01 Phases and dependencies](docs/01-phases-and-dependencies.md).
Stub files carry a `// Phase: <id>` header naming the phase that implements them and the contracts they depend on.
The assembly definition references enforce the dependency direction (`Sim` never references `Presentation`, `UI`, `Net` or `Data`).
