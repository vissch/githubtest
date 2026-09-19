# Game modes and meta-progression

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

All modes run the same deterministic `SimWorld`; a mode is a `MissionDefinition` plus a shell flow. Phase **A7**
(milestone M7) delivers everything below except the campaign missions, which arrive with M3/M4/M6.

## Modes
| Mode | Definition | Rules | Reward |
|---|---|---|---|
| Campaign | Nation-specific chapters of hand-authored missions (3 in the slice, see [Missions](08-missions.md)) | Mission script win/lose; Normal / Hard / Chapter Finale tiers | Gold 25 / 30–40 / 75 |
| Survival | Any faction, any map, 50 escalating waves generated from a `WaveTable` (composition weights × wave index), static income | Lose when own HQ falls; score = waves cleared | Gold per 5 waves; Reinforcement surge ability enabled |
| Operations | 20 consecutive sectors; surviving units and unspent silver carry over; no rewards until sector 20 or wipe | Lose everything on defeat | 150 gold lump sum (doublable) |
| Sandbox / Creative | Map authoring tool exposed in-game: silver, difficulty, weather, fog, map scale, obstacle/emplacement library (200+ items), cross-era rosters | Freeform | none |
| Multiplayer (PvP / co-op vs AI) | Symmetric versions of the mission maps, lockstep | Sector control | ranked later |

## Meta-progression (out-of-match)
- **Gold coins** from campaign/operations. Full national campaign ≈ 3,000 gold.
- **Army upgrades**: per unit, three tracks (Health, Damage, Accuracy), six tiers of +5 each.

| Tier | Cost | Cumulative |
|---|---|---|
| 1 | 15 | 15 |
| 2 | 20 | 35 |
| 3 | 40 | 75 |
| 4 | 60 | 135 |
| 5 | 80 | 215 |
| 6 | 100 | 315 |

One unit fully upgraded = 945 gold; a five-slot roster = 1,275–1,575 gold depending on faction.
Upgrades are applied as `UnitDefinition` stat deltas when the match blob is baked, so the sim stays data-driven.

- **Growth Fund**: achievement track (missions cleared, waves survived, tanks destroyed) paying gold, consumables
  (one-shot abilities) and uniforms.
- **Uniform skins**: presentation-only material/texture swaps on the shared VAT skeleton. Defaults and variants:

| Faction | Default | Variants |
|---|---|---|
| German Empire | 1916–18 Stahlhelm | 1914 Pickelhaube, Seebataillon, Alpenkorps, Asienkorps, Prussian Guard, WW2, GSG-9 |
| British Empire | Khaki + Brodie | CEF, Sikh sepoys, ANZAC, Gordon Highlanders, Redcoats, Hejaz irregulars, SAS |
| French Republic | Horizon blue + Adrian | 1914 red trousers, Chasseurs Alpins, Foreign Legion, Senegalese and Indochinese tirailleurs, Polish Blue Army, GIGN |
| United States | AEF doughboy | Harlem Hellfighters, USMC Belleau Wood, Union/Confederate, WW2 Pacific, holiday |
| Russian Empire | Gimnasterka | Siberian winter, Latvian riflemen, 226th Zemlyansky, Don Cossack, Women's Death Battalion, Teutonic Knight |

## Shell flow
`Main menu → Campaign map → Briefing (roster choice, ability loadout) → Battle → Debrief (gold, optional objectives,
replay save) → Army (upgrades, skins) → back`. Survival/Operations skip the briefing choices except faction and map.
