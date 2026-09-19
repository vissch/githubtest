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
| HE barrage | 150 | 60 s | 4 s (spotting round) | Area r 25 m | 12 shells over 6 s, each 150 dmg r 8 m, +60 suppression, `CraterStamp` r 3 m; wire breached in radius; light vehicles (< 4 000 HP) flipped (destroyed) on direct hit | `↩` Fallback, hold below rim |
| Creeping barrage (new, M2 unlock) | 250 | 120 s | 6 s | Line + heading | Barrage line advances 10 m every 4 s for 40 s ahead of an Advance; suppression wall; friendly units < 15 m behind line take no damage | Enemy: stay below rim, counter-battery timer |
| Chlorine gas | 120 | 90 s | 3 s | Point + wind | Gas field source 40 concentration, diffuses on 4 m grid with map wind vector, sinks into `Trench`/`Crater` cells (×2 concentration); 6 dmg/s per 10 conc, ignores armor/cover; units without mask flag flee upwind; trenches left intact | Fallback, masks (M1 unlock gives 60 % resist), wind change |
| Mustard gas (M3) | 180 | 120 s | 3 s | Area r 20 m | Persistent 90 s, lower DPS, +30 suppression, blocks capture of cells while conc > 5 | Wait it out, capture around |
| Bomber run ("Trench destroyer") | 200 | 120 s | 5 s (flyover) | Line 60 m + heading | 6 bombs 400 dmg r 6 m, collapses trench cells hit (`Trench`→`Crater`, fire-steps removed, cover 50 %), destroys bunkers (2 hits), MG nests | Move out of line; AA lorry shoots plane (M3) |
| Smoke screen | 60 | 30 s | 2 s | Line 40 m | Smoke field 30 s, LoS ray attenuates 8 %/m through conc > 10; snipers and MGs lose targets; suppression gain ×0.5 for units inside | Wait; enemy blind-fires last known cells |
| Off-map mortar salvo | via Officer | 45 s | 4 s | Point | 4× 120 dmg r 5 m | Move |
| Recon flight (new, M2) | 40 | 60 s | 0 | Line | Reveals enemy unit counts per trench in UI for 20 s; reveals minefields (M3) | — |
| Reinforcement surge (Survival only) | 100 | 90 s | 0 | — | Instantly spawns 1× of each infantry slot at rally | — |

Gas/smoke field: `GasSystem` job diffuses two `half` fields (gas, smoke) with wind advection each tick on
the 4 m grid; sink term into trench/crater cells; concentration read by LoS raycast and damage system.
