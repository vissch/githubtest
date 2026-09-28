---
name: tw-balance-sim
description: Balancing simulator for Trench Warfare 3D — pitch game ideas, set up gameplay with placeholders before any art exists, sweep unit and ability numbers over seeds with the deterministic sim, find baselines, balance the two sides and campaign chapters, and keep the roster extensible. Knows RosterEntry, UnitDefinitions.Apply overrides, FactionRoster, OffMapAbilities, SimConfig loadouts, CombatTables/TankSpec, reading winners and deaths, the two-sided LockstepSession + ScriptedEnemy loop, CampaignGraph, and the owner's roster v3 proposal. Use for "balance the tanks", "is Brass stronger", "sweep the MG damage", "pitch ideas for gas warfare", "balance stage of item X". NOT for how anything looks (the sim roles) or perf (tw-optimizer).
---

# Balance simulator

Deep reference: `references/balance.md` (every stat table, id, ability, config field, checklist; snapshot at 558c667).
The owner's roster redesign: `G:\My Drive\TW3D-pipeline\unit-roster-v2.md` (titled v3).

## The facts that decide how you work
- **The sim is deterministic and pure C#** (Burst Strict floats, "same build, same architecture"). The desktop and the laptop produced byte-identical hashes at 2ad4973 (deploy-only match), so sweeps may run on either station.
- **Numbers are C# in `Sim/`**, not data files: `RosterEntry` statics, `CombatTables.WeaponFor`, `TankSpec`, `OffMapAbilities.TryGetStats`. `Data/` scriptable objects are **not read at runtime**.
- **Explore without editing code:** `UnitDefinitions.Apply(world, new[]{ tweakedDef })` on a live `MatchSim` overrides any unit's numbers (idempotent, changes the Units fingerprint in the hash). `SimConfig.LoadoutA/B` sets the ten slots per side.
- **A chosen number is a SIM-lane code commit** with the full gate plus the determinism, replay and hash tests. Seam items: archetype ids, `RosterEntry.SlotCount`, `SimConfig`, ability ids (< 32, `MayCall`), `SourceId` bands, `Archetypes.Count` 64. Each goes in its own commit, alone, first.
- **The runtime economy is not `SimConfig.Default`:** `SimHost` and missions use 300 starting silver and 2.0/s. The roster v3 proposal assumes 1.2/s, and its weapon columns are docs/06 designs, not `CombatTables` (rifle 25/60 m/0.8 s there vs 36/130 m/0.5 s in code).

## Running a match headless (an EditMode test is the host)
- **Build:** `MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u))` (or `CreateGreybox` / `CreatePlaytest`).
- **Both sides acting:** `new LockstepSession(() => match, canary:false, 0, 0, 0f, seed)` and `session.StepOnce(ai)` with a `ScriptedEnemy` (player 1). The worked example is `SinglePlayerEquivalenceTests`. It uses SHOW code (`TW.Presentation`), so the runner is an **explicit** EditMode test in the SHOW lane that calls the sim.
- **Orders:** `session.LocalDriver.Issue(SimCommand.Deploy(tick, player, slot))`; they land `InputDelayTicks` (3) later.
- **Remove hero randomness:** `m.World.GetSystem<HeroSystem>().TeamMask = 0`.
- **Read results:**
  - `m.World.WinnerTeam` (−1 = none; `MatchEnded` event).
  - After every `Step`, scan `m.World.Events.Events` for `Death` (b = killer slot, or a `DeathCause` < 0), `UnitDeployed` and `VehicleDestroyed`. Events are cleared each step; check `Events.Overrun`.
  - `match.Fire.Kills[team]` / `Shots[team]` count direct fire only.
  - Time = `World.Tick * Config.TickSeconds`.
- **Speed and scale:** a 3,000-unit ceiling. The sim cost per living man is flat (~0.85 µs per tick per 1,000 men).
- `otr.py` runs EditMode tests on the laptop without Unity, but its hashes (Mono) are not comparable with Burst ones. Use it as a filter, never as the verdict.

## The balance loop
1. **Ideas first:** `/pipeline ideas` writes pitched mechanics, each with a hypothesis testable on placeholder boxes. The owner picks before any tuning. Method: cite sources, make the smallest change, show the calculation (the global `gamedesign` agent and the library at `C:\Users\PC\Documents\godot\gamedesign-library`, which covers other genres).
2. **Analytical first** (minutes, laptop): cost per HP, time-to-kill tables per weapon vs target, counter matrix, silver curve at 2.0/s.
3. **Sweep** (desktop): variants × **8 seeds**. Report distributions (mean, p10, p90), never a single run. Compare two variants only on the same seeds.
4. **Baseline** a variant only on the owner's word. A re-baseline is never silent.
5. **Campaign:** clear rates per chapter from `CampaignGraph` nodes and difficulties (EASY/NORMAL/HARD enemy settings). Only starting silver and income reach a match from the meta (`FactionBuildings.ApplyTo`); unit tiers wait on the upgrade seam (docs/21 B1).
6. **Extensible:** a new unit is one `UnitDef` plus an archetype id; a new ability is the tasks.md checklist (SIM first, then the HUD list). Follow those checklists; never invent a second registry.

## Placeholders before art
A playable match needs no final art: box/capsule meshes at gameplay dimensions on the SHOW lane, and existing flipbooks for effects. Record every placeholder on the board item so the art stages replace them later.

## Owner questions — never build around them
- **Open items:** balance not scaled with the giant machines (trench cross width, slope limit, turn rates, speeds, `MaxGrow`); the M1.5 fun-gate playtest; house HP scale (battle 1.4/1.0 vs Playground 60/30, ground floor ×3).
- **Roster v3 decisions:** Great Works built by labour with a bar both sides see; commander names; the Brass and Iron commanders; one Great Work or a choice of two.
- **Not in code yet:** Sapper, Great Works, balloon, commanders, wheeled units, the upgrade seam.

## Learning loop
Brief 2 §B5, the same for every role: see `../pipeline/SKILL.md`, "The learning loop". This role's lessons file is in
that section's table.
