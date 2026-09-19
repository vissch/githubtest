# Missions

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

### 10.1 Mission framework (A6 + C3)
- `MissionDefinition` SO → blob: map id, player faction + roster overrides, unlocked abilities, starting silver,
  income rate, enemy faction, difficulty tier, `MissionScript` triggers, dialogue table, win/lose rules,
  rewards (gold 25/40/75), optional objectives.
- `MissionRunner` (sim system) evaluates triggers each tick in fixed order; emits `MissionTriggerFired(id)`;
  `WaveStarted(n)`.
- Presentation `MissionDirector` reacts: dialogue strip, camera nudges, objective tracker, banners.
- Campaign shell (A7): mission select → briefing → battle → debrief/rewards → army upgrades.

### 10.2 Mission 1 — "Hold the Salient" (Second Ypres, April 1915) — *defensive tutorial*
| | |
|---|---|
| Player | British Empire. Slots: Rifleman, Trench raider, Lewis MG, Sniper, **no vehicle**. Abilities: HE barrage (unlock at trigger 4), Smoke (unlock at trigger 6) |
| Enemy | German Empire, waves only, abilities: chlorine gas (scripted twice), HE barrage (Hard only) |
| Map (`ypres_salient`) | 300 × 600 m. Player: reserve trench (Z 60), main fire trench with 5 traverses (Z 120), outpost sap (Z 180). No Man's Land 180–420 with 2 wire belts (German side), shell-hole belt. German front trench Z 440, support Z 520, spawn Z 600. Wind default +Z→−Z (toward player) for gas, shifts at trigger 5 |
| Starting silver / income | 120 / +1.2 s |
| Objective | Survive 8 waves and keep the main line (objective `MainLine_A`) ≥ 1 friendly unit at all times; lose if enemy captures `ReserveLine_A`. Optional: lose fewer than 20 riflemen (gold +10) |
| Wave script | W1 (t=20 s) 6 riflemen walk in open → teaches garrison + MG. W2 (t=80 s) 10 riflemen + 2 sturmtruppen → teaches roster/sniper on MG. W3 (t=150 s) gas warning dialogue, chlorine drifts onto outpost sap → teaches `↩` Fallback and "gas sinks into trenches". W4 12 riflemen + 4 sturmtruppen advancing behind gas. W5 wind shifts (trigger), player HE barrage unlocked → teaches barrage on the wire gap. W6 sentry + 8 rifles; smoke unlocked; hint: sniper the sentry flanks. W7 counter: 16 mixed + enemy HE (Hard). W8 finale: 20 mixed + 2 sentries, then relief dialogue |
| Phase dependencies | A1, A2, A3, A5 (gas, HE only), A6, B1–B3, B5 (gas, impacts), B6, C1 map, C2 British/German infantry, C3 script |
| Milestone | Playable at **M3** with placeholder art; final at M6 |

### 10.3 Mission 2 — "Over the Top" (Somme, 1 July 1916) — *assault, wire, craters, creeping barrage*
| | |
|---|---|
| Player | British. Slots: Rifleman, Trench raider (+ Bangalore unlock at trigger 3), Lewis, Sniper → Officer swap allowed at debrief, Mark IV **Female** (unlock trigger 6, one at a time). Abilities: HE barrage, Smoke, Creeping barrage (unlock trigger 2), Recon flight |
| Enemy | German. Static: 4 MG nests, 1 concrete bunker at main line, 3 wire belts, field gun emplacement at reserve. Waves: counterattacks after each objective loss. Abilities: HE barrage every 90 s at densest player cell, gas once (Hard) |
| Map (`somme_montauban`) | 300 × 800 m. Player trench Z 80. No Man's Land Z 100–380 rising 6 m toward the German side (reverse-slope MG enfilade from a low rise at X 220, Z 300). Wire belts at Z 330/360/380 with two gaps (pre-sighted by MGs). German outpost line Z 400 (`OutpostLine_B`), main line Z 520 with bunker (`MainLine_B`), reserve Z 640 with field gun (`ReserveLine_B`), HQ dugout Z 720 (`HQ_B`) |
| Starting silver / income | 300 / +1.5 s |
| Objective | Capture Outpost → Main → Reserve → HQ in order. Lose if own trench captured or 12 min timer (Hard). Optional: capture Main line with creeping barrage active (+10 gold) |
| Script | T1 briefing; T2 at 30 s unlock creeping barrage + tutorial reticle; T3 first unit reaches wire → Bangalore unlock + hint; T4 outpost captured → counterattack wave 12 units after 20 s + enemy HE; T5 bunker LoS dialogue (use smoke/bomber not available → flank rear door with raiders); T6 Main line captured → Mark IV Female unlock, dialogue on mud bogging; T7 reserve field gun fires on tank → hint AT roles reversed (sniper the crew); T8 HQ captured → victory |
| Teaches | `>>` timing under creeping barrage, wire gaps as kill zones, crater hopping (units auto-use craters as cover when Pinned), enfilade from the rise, traverse-by-traverse clearing with grenades, tank as mobile cover |
| Phase dependencies | everything in M1 + A4 (craters, wire, mud), A5 (creeping barrage, field gun, armor), B2 (deformation), B4, C1 map, C2 Mark IV |
| Milestone | **M4** |

### 10.4 Mission 3 — "Iron Dawn" (Cambrai, 20 November 1917) — *combined arms, armour vs armour, full sector control*
| | |
|---|---|
| Player | British. Slots: Rifleman, Raider, Lewis, AT rifle **or** Officer (player choice at briefing), Mark IV Male / Mark V (Mark V unlock trigger 5), Pierce-Arrow AA lorry available via trigger 4. Abilities: HE, Creeping barrage, Smoke, Bomber run, Recon flight, Mustard gas (Hard only, moral choice flavour text) |
| Enemy | German. Static: 2 bunkers, 3 MG nests, 2 field guns, minefield at Z 300–340 (revealed by recon), 3 wire belts, Hindenburg-line double trench with a 4 m anti-tank ditch (Mark IV crosses with fascine ability — trigger 2 unlock, A7V cannot). Waves: counterattacks with sturmtruppen, flamethrowers, sentries; **A7V ×2** at reserve line; enemy abilities: HE, gas, bomber (Hard), AT rifles in waves |
| Map (`cambrai_flesquieres`) | 300 × 800 m, dry ground (mud 0) except the ditch; village ruins at Z 600 (bunker cells + rubble cover volumes); ridge at Z 560 (reverse slope: player tanks silhouetted when cresting → field guns get +acc) |
| Starting silver / income | 400 / +2.0 s |
| Objective | Outpost (`Z 380`) → Main Hindenburg (`Z 480`, ditch at 470) → Village (`Z 600`) → HQ (`Z 720`). Lose if own HQ falls. Optional: destroy both A7Vs (+15 gold), no tank lost (+10) |
| Script | T1 recon flight forced tutorial → minefield revealed; T2 first tank near ditch → fascine unlock (tank drops fascine, ditch cells become `Link`); T3 outpost captured → flamethrower counterwave; T4 bunker at main line blocks → bomber run unlock + AA lorry unlock (enemy bomber on Hard); T5 main line captured → A7V pair advances with sentries; Mark V unlock; T6 village: rubble cover volumes, grenade clearing, mustard gas (Hard) by enemy; T7 HQ captured → finale, campaign chapter reward 75 gold |
| Teaches | Armor incidence angles (flank the A7V), track/engine failure, indirect vs bunkers, mines/recon, AA, full sector control |
| Phase dependencies | all of A1–A6, B1–B7, N not required, C1 map, C2 full British + German + French vehicle set, A5 mines/fascine/AA additions |
| Milestone | **M6 vertical slice** |

Multiplayer uses the same maps in symmetric mode (mirrored trench lines, both HQs) — M5.

## Map layouts (top-down, Z increases to the right; X is lateral)

### Mission 1 — `ypres_salient` (300 × 600 m)
```
X=300 ┌──────────────────────────────────────────────────────────────┐
      │ [R]      [M~~~~~]   [o]   ....  ##  ..  ##   [G======]  [S] >│ spawn
      │ [R]      [M~~~~~]   [o]   ....  ##  ..  ##   [G======]  [S]  │
      │ [R]      [M~~~~~]         ....  ##      ##   [G======]  [S]  │
X=0   └──────────────────────────────────────────────────────────────┘
      Z=0   60        120       180   240 330 360 400   440        520  600
      [R] reserve trench   [M] main fire trench (5 traverses)   [o] outpost sap
      .... shell-hole belt   ## wire belt (gap at X 150)   [G] German front   [S] German support
      Wind: → toward player until trigger 5, then ← 
```

### Mission 2 — `somme_montauban` (300 × 800 m)
```
X=300 ┌────────────────────────────────────────────────────────────────────────────┐
      │ [P]     ~~~~mud~~~~   ^rise^ MG  ## ## ##  [O]   [M##B##]   [Rf]   [HQ]   │
      │ [P]     ~~~~mud~~~~          MG  ##    ##  [O]   [M#####]   [Rf]   [HQ]   │
      │ [P]                          MG  ## ## ##  [O]   [M#####]   [R ]          │
X=0   └────────────────────────────────────────────────────────────────────────────┘
      Z=0  80          200    300      330 360 380 400   520        640     720  800
      [P] player trench   ^rise^ low rise at X 220 (reverse-slope MG enfilade)
      ## wire belts (gaps at X 90 and X 230)   [O] outpost line   [M] main line, B = concrete bunker
      [Rf] reserve with field gun   [HQ] HQ dugout   Ground rises 6 m from Z 100 to Z 380
```

### Mission 3 — `cambrai_flesquieres` (300 × 800 m)
```
X=300 ┌────────────────────────────────────────────────────────────────────────────┐
      │ [P]    ## x x x ##  [O]  [H1|ditch|H2]  ^ridge^  [Village B B]  [FG]  [HQ] │
      │ [P]    ## x x x ##  [O]  [H1|ditch|H2]  ^ridge^  [Village   B]  [FG]  [HQ] │
      │ [P]    ##       ##  [O]  [H1|ditch|H2]  ^ridge^  [Village    ]        [HQ] │
X=0   └────────────────────────────────────────────────────────────────────────────┘
      Z=0 80   260 300-340 360 380   470-480   560       600            680   720  800
      x minefield (hidden until recon)   [H1|ditch|H2] Hindenburg double trench with 4 m anti-tank ditch
      ^ridge^ crest at Z 560 (tanks silhouetted)   B bunkers   [FG] field guns + A7V pair   dry ground, mud 0
```

## Trigger table format (used by `MissionScript` assets)
| Field | Type | Example |
|---|---|---|
| `id` | int | 4 |
| `condition` | enum + params | `ObjectiveCaptured(OutpostLine_B)` |
| `delayTicks` | uint | 400 (= 20 s) |
| `actions[]` | list | `SpawnWave(table=Counter_A, entry=SupportTrench, order=Advance)`, `EnemyAbility(HE, target=DensestPlayerCell)`, `Dialogue(m2_t4)` |
| `once` | bool | true |

Conditions: `TickAtLeast`, `WaveStarted(n)`, `WaveCleared(n)`, `ObjectiveCaptured(id)`, `ObjectiveLost(id)`,
`UnitsInVolume(volume, team, count)`, `SilverAtLeast(n)`, `AbilityUsed(id)`, `VehicleDeployed(id)`, `Timer(id)`.
Actions: `SpawnWave`, `UnlockSlot`, `UnlockAbility`, `LockSlot`, `SetEnemyBudget`, `EnemyAbility`, `SetWind`,
`SetFog`, `Dialogue`, `SetWinRule`, `SetLoseRule`, `RevealCells`, `StartTimer`, `EndMission(win|lose)`.
