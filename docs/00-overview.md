# Trench Warfare 3D — Overview

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

## Vision
Rebuild the lane-based 2D tug-of-war RTS *Trench Warfare 1917* as a 3D tactical title while keeping its identity:
attritional tug-of-war along one axis, five deployment slots, trench commands (`>>`, roster, lock, `↩`), off-map
support fire, and a silver-per-second in-match economy. 3D adds what the 2D game could only fake:

- a **corridor-bound sector** (300 m × 800 m) with lateral room for real flanking and enfilade fire,
- **dual-layer navigation** (inside trenches vs. over the top) with vaults, ladders and ramps,
- **real line of sight** against a heightfield, directional cover, and arced indirect fire that plunges into open trenches,
- **suppression and pinning** driven by near-miss volume, not hit-point damage,
- **deformable terrain**: barrages carve craters that become cover and reroute pathfinding,
- **angle-of-incidence armor** for 1917 tanks with track and engine failure,
- **sector-control infiltration** as the win condition instead of running off the screen edge.

## Hard constraints
| Constraint | Value |
|---|---|
| Engine | Unity 6 LTS (6000.0.50f1), URP 17, Burst 1.8, Collections 2.5, Mathematics 1.3 — **no Entities** (dropped 2026-09-20, [11-plan-review](11-plan-review.md) §3) |
| Multiplayer | Deterministic lockstep from day one; single-player is the same sim with an AI peer. Online transport (N2/N3) comes **after** the single-player slice |
| Platform / hardware | **Windows x64 only**; 60 fps at 1080p on GTX 1050 / Intel Iris Xe, quad-core CPU |
| Simulation budget | ≤ 4 ms per 20 Hz tick at 2,000 infantry slots; 0 B/frame GC allocation |
| Team | 1–2 developers; two parallel tracks maximum |

## What "done" looks like per milestone
| Milestone | Outcome |
|---|---|
| P0.5 Baseline is true | Project compiles, EditMode + PlayMode tests green on a fresh clone, CI workflow committed |
| M1 Greybox corridor | 2,000 capsules flow through trench links under local lockstep, hash-identical across two sims |
| **M1.5 Fun gate** | Riflemen + MG, two trench lines, one barrage, one gas cloud — capsules only. Playtest decides unit density and whether VAT is built |
| M2 First firefight | Riflemen on the fire-step, MG enfilade pins an advance, `>>` and `↩` work, VAT infantry rendered |
| M3 Shells and gas | Barrage craters, gas sinking into trenches, **Mission 1 playable** |
| M4 Combined arms | Tanks, wire, mud, creeping barrage, **Mission 2 playable** |
| M5 Vertical slice | **Mission 3 playable**, three missions on Normal/Hard at target performance |
| M6 Online (if a launch requirement) | Two clients over a real network with desync detection and replays |
| M7 Modes and meta | Survival, Operations, campaign shell, upgrades, skins |

## Reading order
1. [Phases and dependencies](01-phases-and-dependencies.md) — what to build, in what order, and what can run in parallel
2. [Contracts](02-contracts.md) — the three frozen interfaces every track builds against
3. [Determinism rules](03-determinism-rules.md) — the coding standard for anything under `Sim/`
4. [Architecture](04-architecture.md) — assemblies, system order, lockstep flow, render tiers
5. [Performance budgets](05-performance-budgets.md)
6. [Units and factions](06-units-and-factions.md), [Abilities](07-abilities.md), [Missions](08-missions.md)
7. [Game modes and meta](09-game-modes-and-meta.md), [Risks](10-risks.md)
8. **[Plan review and re-cut](11-plan-review.md)** — decisions of 2026-09-20; wins over the docs above where they conflict
