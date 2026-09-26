# Abilities

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

## Unit abilities

| Ability | Owner | Trigger | Sim effect | Events |
|---|---|---|---|---|
| Hand grenade | Assault, Shield grenadier, Raider | Auto at ≤ 20 m vs trench/crater/bunker-rear cells, 6 s cooldown | Arc to target cell, blast r 4 m, 80 dmg, +40 suppression, ignores cover | Shot(arc), Explosion |
| Incendiary grenade | Russian assault | As above | Blast 40 + `Burning` cells 5 s (10 dmg/s, units flee cell) | Explosion, CellBurning |
| Bangalore torpedo | British/German assault (mission unlock) | Active on `Wire` cells within 10 m | 3 s place, wire breached 6 m wide | WireBreached |
| Wire cutters | Rifleman (passive) | In `Wire` cell for 8 s uncontested | Breach 2 m | WireBreached |
| Officer aura | Officer | Passive r 15 m | +15 % dmg, suppression gain ×0.5, un-Pin | — |
| Officer smoke marker | Officer | Active, target pos ≤ 60 m, 45 s cd | Smoke cell + 4 s later off-map mortar salvo 4× r 5 m | SmokeSpawned, Explosion×4 |
| Sniper priority | Sniper | Passive | Target weighting MG/Officer/Sentry/crew | — |
| Sentry plating | Sentry | Passive | Directional armor: rifle fails frontal | Hit(ricochet) |
| Flamethrower burst | Flamethrower | Auto ≤ 12 m | Cone; `Burning`; bunker embrasure bypass | CellBurning |
| Field gun HE / AP | Field gun | Auto: AP vs vehicles, HE vs infantry/bunkers | Direct, crater on ground impact | Explosion, CraterStamp |
| Mortar | Mortar team, Arditi | Auto 30–150 m on densest enemy cell in LoS-independent range | Indirect arc, plunges into trenches | Shot(arc), Explosion |
| Tank crush | Tanks | Contact | Wire breached, infantry in path 200 dmg | WireBreached, Death |
| Tank mobile cover | Tanks, Sentry | Passive | `CoverVolume` behind hull, arc 180° rear | — |
| Track/engine damage | Vehicles | On penetration | Immobilise / stall, repair when unengaged | VehicleTrackHit, VehicleStalled |
| Dismount | Cavalry | At objective / wire | Convert to rifleman slot | UnitSpawned |

## Off-map support abilities

All are `AbilityDefinition{cost, cooldown, warmupTicks, targetMode(Point/Line/Area/Heading), radius, length, payload}`.
Enemy AI uses the same definitions with a mission-set budget. Each fires `AbilityFired` (for the audio cue / whistle) then payload events.

| Ability | Cost | CD | Warm-up | Target | Sim effect | Counter-play |
|---|---|---|---|---|---|---|
| HE barrage (implemented) | 150 | 60 s | 4 s (spotting round) | Area r 25 m; patterns: disc / line ≤ 60 m, 4 m either side / box 16 m wide | 12 shells over 6 s, each 150 dmg r 8 m, +60 suppression, `CraterStamp` r 3 m; wire breached in radius; light vehicles (< 4 000 HP) flipped (destroyed) on direct hit | `↩` Fallback, hold below rim |
| Creeping barrage (implemented) | 250 | 120 s | 6 s | Line + heading, ≤ 60 m | 10 lifts of 4 shells (120 dmg r 8 m, crater 3 m), a lift every 4 s, 6 m further along the heading, 10 m either side; the caller's own men within 15 m behind a lift take nothing (`Impact.SafeBehind`) | Enemy: stay below rim, counter-battery timer |
| Chlorine gas (implemented) | 120 | 90 s | 3 s | Point + wind; pattern 1: creeping ≤ 64 m | Gas field source 40 concentration for 12 s, diffuses on 4 m grid with map wind vector, sinks into `Trench`/`Crater` cells (×2 concentration); 6 dmg/s per 10 conc, ignores armor/cover; units without mask flag flee; trenches left intact. Creeping: 8 sources of 30, 8 m and 3 s apart, 10 s each | Fallback, masks (M1 unlock gives 60 % resist), wind change |
| Mustard gas (M3) | 180 | 120 s | 3 s | Area r 20 m | Persistent 90 s, lower DPS, +30 suppression, blocks capture of cells while conc > 5 | Wait it out, capture around |
| Bomber run ("Trench destroyer") | 200 | 120 s | 5 s (flyover) | Line 60 m + heading | 6 bombs 400 dmg r 6 m, collapses trench cells hit (`Trench`→`Crater`, fire-steps removed, cover 50 %), destroys bunkers (2 hits), MG nests | Move out of line; AA lorry shoots plane (M3) |
| Smoke screen (implemented) | 60 | 30 s | 2 s | Line 40 m | 5 sources of 30 for 30 s in the smoke field (`GasSmokeSystem.Smoke`, diffusion 0.06, decay 0.006); `SmokeLos` counts the metres of conc > 10 on a line: a target is lost past 12.5 m of it, accuracy falls 8 %/m (floor 20 %), suppression gain ×0.5 for a man inside | Wait; enemy blind-fires last known cells |
| Strafe run (implemented, docs/21) | 180 | 90 s | 5 s (run-in) | Line + heading 80 m, half width 3 m | 32 bursts of 60 dmg r 5 m, +35 suppression, no crater, 2.5 m apart over 2 s; men standing on the line die, men 12 m off are untouched; a trench keeps its usual protection | Move off the line; below the rim |
| Beam (implemented, docs/21) | 300 | 180 s | 4 s (charge) | Line + heading 60 m, half width 2 m | A 6 s sweep: the head walks the corridor, the 4 m behind it burn; 300 dps on men (x0.7 in a trench), they catch fire, die of `DeathCause.Beam`; 1200 dps straight off a hull; the ground under the head is scorched (trees and wire wear, `Explosion` with `dir.y` = 4 every 4 ticks); one sweep in flight at a time (the cooldown outlasts it) | Move off the corridor; below the rim halves the loss |
| Mines and tripwires (implemented, docs/21 SIM-D; laid by the sapper once units-meta lands, a system call until then) | 1 charge | — | 1 s (arming) | Point / line up to 12 m | A mine: 260 dmg r 4 m, +50, crater 1.5 m, goes off under an enemy man within 1 m or an enemy hull (the nearer track); a tripwire: 90 dmg r 4.5 m, +40, crossed within 0.6 m; a shell's crater cooks off the mines within 1.5 x its radius | Shell the ground ahead; keep to the trenches (a mine never lies in one) |
| Off-map mortar salvo | via Officer | 45 s | 4 s | Point | 4× 120 dmg r 5 m | Move |
| Recon flight (new, M2) | 40 | 60 s | 0 | Line | Reveals enemy unit counts per trench in UI for 20 s; reveals minefields (M3) | — |
| Reinforcement surge (Survival only) | 100 | 90 s | 0 | — | Instantly spawns 1× of each infantry slot at rally | — |

Gas/smoke field: `GasSystem` job diffuses two `half` fields (gas, smoke) with wind advection each tick on
the 4 m grid; sink term into trench/crater cells; concentration read by LoS raycast and damage system.
