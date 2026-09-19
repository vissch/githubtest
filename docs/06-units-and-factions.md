# Units, vehicles, factions, emplacements

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

Stats are baseline data (2D remake values scaled: cost in silver, HP, damage per shot, range m, RoF/s, speed m/s).
All are `UnitDefinition` fields and will be tuned; 3D columns are new behaviours.

### 5.1 Standard infantry (every faction)
| Class | Cost | HP | DMG | Range | RoF | Speed | 3D behaviours |
|---|---|---|---|---|---|---|---|
| Rifleman | 25 | 100 | 25 | 60 | 0.8 | 3.0 | Fire-step firing; prone in open when suppressed; captures objectives |
| Assault | 40 | 90 | 12 (×burst 5) | 25 | 4 | 4.2 | Sprint vault; auto grenade at ≤ 20 m onto trench cells (arc, ignores LoS); traverse clearing bonus |
| Machinegunner | 60 | 110 | 18 | 80 | 8 (belt 50, reload 4 s) | 2.2 | Kneel/prone deploy time 1.5 s; enfilade arc 60° sweep; triple suppression gain; cannot fire while moving |

### 5.2 Special units (slot 4, faction-specific)
| Special | Factions (default) | Cost | HP | DMG | Range | 3D behaviours |
|---|---|---|---|---|---|---|
| Sniper | British, German, Russian | 90 | 80 | 150 | 80 (+40 % in trench) | Target priority MG > Officer > Sentry; +100 % HP/+25 % acc in trench; reveals from 2 m eye height on fire-step |
| Sentry (armoured MG) | German, Austro-Hungarian | 125 | 300 (plate: front 8 mm) | 18 | 60 | Advances in open under rifle fire (rifle pen 4 mm fails frontally, flanks 2 mm succeed → flanking matters); acts as mobile `CoverVolume` for units behind |
| Mortar team | Russian, Ottoman, French (crapouillot) | 100 | 90 | 120 blast r 6 m | 30–150 | Indirect arc, 6 s cycle, needs stationary 2 s setup; ignores LoS; cannot fire < 30 m |
| Arditi (mobile mortar) | Italian | 110 | 110 | 90 blast r 5 m | 25–100 | Fires while stationary between sprints; grenades too |
| Anti-tank rifle | German, British (later) | 100 | 90 | 1400 pen 20 mm | 70 | Prone-only fire; 5 s cycle; module roll on vehicles |
| Field gun (crew) | French (75 mm), German (7.7 cm), Italian | 125 | 200 | 1500 blast r 4 m, pen 30 mm | 100 | Direct fire only, towed at 1.5 m/s, 4-crew; crater on miss if ≥ 75 mm |
| Shield grenadier | Italian (Farina), French | 80 | 140 (shield front 6 mm) | 12 + grenade | 20 | Frontal shield `CoverVolume` for self and one follower; grenades on trench cells |
| Officer | American, Japanese, British | 110 | 100 | 20 (pistol) | 30 | Aura r 15 m: +15 % DMG, halves suppression gain, cancels Pinned; active: smoke marker → off-map mortar salvo (see Off-map support abilities in [07-abilities](07-abilities.md)) |
| Flamethrower | German (Flammenwerfer), later others | 120 | 100 | 40/s cone 12 m | 12 | Ignores cover in trench cells (open top), sets `Burning` on cells (DoT 5 s), fuel 10 s, explodes on death |
| Cavalry / Camel cavalry | Russian, Ottoman, Arab | 70 | 120 | 25 | 40 | 8 m/s on surface only; cannot enter trench; wire stops them; dismount at objective |
| Women's Death Battalion (elite rifle) | Russian | 45 | 120 | 30 | 60 | Immune to Pinned (still Prone); morale aura small |

### 5.3 Vehicles (slot 5)
| Vehicle | Faction | Cost | HP | Armor F/S/R/T mm | Weapons | Speed | 3D behaviours |
|---|---|---|---|---|---|---|---|
| Mark IV (Male) | British | 350 | 10 000 | 12/8/6/6 | 2× 6-pdr sponsons (side arcs), 3× Lewis | 1.6 | Crosses trenches ≤ 3.5 m, crushes wire, mobile cover, bog chance high |
| Mark IV (Female) | British | 300 | 10 000 | 12/8/6/6 | 5× Vickers | 1.6 | Anti-infantry variant |
| A7V | German | 400 | 12 000 | 30/20/20/6 | 57 mm front, 6× MG08 | 1.8 | Cannot cross trenches > 2 m (must use ramps), very high bog chance |
| Saint-Chamond | French | 400 | 15 000 | 17/17/17/5 | 75 mm front, 4× Hotchkiss | 2.0 | Long overhang: cannot cross craters > 4 m, cannot climb slopes > 20° |
| Schneider CA1 | French | 300 | 8 000 | 11/11/11/5 | 75 mm BS, 2× Hotchkiss | 2.2 | Cheaper, flammable (fuel tanks) |
| Renault FT | American, French | 220 | 5 000 | 16/16/8/8 | 37 mm turret or MG | 3.0 | Turret 360°, cannot cross trenches > 1.8 m |
| Garford-Putilov | Russian | 300 | 6 000 | 6.5 all | 76.2 mm rear, 3× Maxim | 3.5 (road) 1.0 (mud) | Wheeled: road only effectively, reverse-fires main gun |
| Lancia 1ZM | Italian | 200 | 3 500 | 6 all | 2–3× MG | 5.0 | Fast flanker, wire-cutter rails |
| Pierce-Arrow AA lorry | British | 220 | 3 000 | 3 all | 2-pdr pom-pom | 5.0 | Long-range HE against emplacements |
| Motorcycle sidecar MG | German, Italian | 90 | 600 | 0 | 1× MG | 8.0 | Hit-and-run on roads |
| Ehrhardt E-V/4 | German | 250 | 4 000 | 7 all | 3× MG08 | 4.0 | Wheeled, road only |
| Austin armoured car | Russian, British | 220 | 3 500 | 8 all | 2× Maxim turrets | 4.5 | Wheeled |
| Mark V | British (mission 3 unlock) | 380 | 11 000 | 14/12/8/8 | as Mark IV | 2.0 | Better mud handling |

### 5.4 Faction rosters (default five slots)
| Faction | Rifleman | Assault | MG | Special (default / alt) | Vehicle |
|---|---|---|---|---|---|
| British Empire | SMLE Mk III | Trench raider (Webley + Mills bombs, cut-down SMLE) | Lewis | Sniper / Officer / AT rifle (late) | Mark IV / Mark V / Pierce-Arrow |
| German Empire | Gewehr 98 | Sturmtruppen (MP18 + Stielhandgranate) | MG 08/15 | AT rifle / Sentry / Flamethrower / Sniper | A7V / Ehrhardt / motorcycle |
| French Republic | Lebel / Berthier | Nettoyeur (revolver, VB grenades) | Hotchkiss M1914 | 75 mm field gun / Mortar / Shield grenadier | Saint-Chamond / Schneider / FT |
| United States | M1903 | M97 trench gun | BAR | Officer (smoke→mortar) | Renault FT |
| Russian Empire / Red / White | Mosin-Nagant | Assault (incendiary No. 76 grenades → `Burning` cells) | Maxim M1910 | Mortar / Sniper / Death Battalion / Cossack cavalry | Garford-Putilov / Austin |
| Italy | Carcano | Arditi (mortar) | Villar Perosa | Shield grenadier / Field gun | Lancia 1ZM |
| Austria-Hungary | Mannlicher M95 | Sturmtruppen | Schwarzlose | Sentry / Mortar | (armoured car, data only) |
| Ottoman Empire | Mauser 1893 | Assault | Maxim | Camel cavalry / Mortar | (data only) |
| Japan | Type 38 | Assault | Type 3 | Officer (sword aura) | (data only) |
| Bulgaria, Romania, Belgium, Serbia | data-only rosters reusing the above archetypes | | | | |

**Slice scope:** British and German fully implemented (all their specials and vehicles); French vehicles for
M3 enemy variety; every other faction exists as data with shared archetypes and placeholder art.

### 5.5 Static enemy / emplacement types
| Emplacement | Sim representation | Counter |
|---|---|---|
| MG nest (sandbag) | Static MG unit in a `CoverVolume` arc 120°, frontal 80 % | Flank, mortar, sniper, smoke + assault |
| Concrete bunker | `Bunker` cells, embrasure arc 60°, 95 % frontal, immune to small arms and gas-lite | Bomber run, field gun, flamethrower at embrasure, grenades at rear door |
| Wire belt | `Wire` layer cost + speed penalty | HE ≥ 105 mm, Bangalore, tanks |
| Observation post / HQ dugout | Objective structure, spotter grants enemy AI faster ability cooldown while alive | Capture (win condition) |
| Field gun emplacement | Static field gun with `CoverVolume` | Indirect fire, tanks from flank, sniper crew |
| Gas projector battery (Livens) | Scripted enemy ability source | Timer/mission trigger |
| Minefield (M3) | Hidden `Mine` cells: infantry 1 m trigger, vehicles track hit | Recon marker, HE barrage clears |
| Rail gun / off-map artillery | Enemy off-map ability with warm-up event (whistle) | Fallback command |

## Trench commands

| Command | 2D | 3D implementation |
|---|---|---|
| `>>` Advance | All garrisoned units leap and sprint to next trench | Units get goal = next enemy trench flow field; vault via nearest `Link`; stance Sprint; `Exposed` |
| `↑` Roster | Pick classes to move | Roster panel per trench with per-class toggles; `TrenchSelectAdvance` mask |
| Lock | Arrivals pass through | Trench flagged `Locked`: arriving units keep flow field to next objective, no garrison |
| `↩` Fallback | Cancel advance, return | Units in open get goal = source trench fallback field; Sprint; suppression gain halved for 3 s (grace) |
| (new) Hold fire / Peek | — | Toggle `SuppressFire`; units stay below rim (survive barrage, no return fire) |
| (new) Rally point | — | `SetRally` for reinforcement staging behind the line |

## Stances and cover

| Stance | Speed | Hit-box height | Cover bonus in open | Accuracy | Trigger |
|---|---|---|---|---|---|
| Standing | 1.0 | 1.6 | 0 | 1.0 | default surface |
| Crouch | 0.8 | 1.0 | +10 % | 1.0 | in trench below rim; crater rim |
| Fire-step | 0 | 0.5 exposed | +85 % (frontal arc 180°) | 1.25 | firing from trench |
| Prone | 0.4 | 0.4 | +40 % | 0.9 (MG 1.2) | suppression > 60, crater floor, player order |
| Sprint | 1.5 | 1.6 | −10 % | no fire | Advance / Fallback |
| Pinned | 0 | 0.4 | +40 % | no fire | suppression > 85 |

`CoverVolume { float3 center; float radius; float protectedArcCenter; float arcHalfWidth; float bonus; float height; }`
Cover applies only if the attacker's bearing lies within the arc and the shot is direct fire. Indirect fire and
grenades ignore cover (open-top), bunkers excepted.
