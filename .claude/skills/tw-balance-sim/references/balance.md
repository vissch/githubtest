> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

Path keys: R = repo root, P = trench-warfare-3d/Assets/_Project, T = trench-warfare-3d/Tools, D = docs.

## A. BALANCE simulator

### Unit data
**`RosterEntry`** (`P\Sim\Core\RosterEntry.cs`)
- **Fields** (:7-18): `byte Archetype`, `int Cost` (silver), `float Hp`, `float Speed` (m/s), `int CooldownTicks` (redeploy), `bool IsVehicle`, `byte Chassis` (a `ChassisKind`: Foot / Tracked / Legged / Wheeled).
- `public const int SlotCount = 10;` (:20). This is a seam item.
- `FillDefault(roster, playerOffset)` forwards to `FactionRoster.Fill`: offset 0 gets Iron, anything else Brass (:25-26).
- `static RosterEntry ForArchetype(byte)` (:30-55) returns `default` for an unknown id.
- **Infantry statics** (:58-73), as cost / hp / speed / cooldown ticks:

  | Unit | Cost | HP | Speed | Cooldown |
  |---|---|---|---|---|
  | Rifleman | 25 | 100 | 3.0 | – |
  | Assault | 40 | 90 | 4.2 | – |
  | Machinegunner | 60 | 110 | 2.2 | – |
  | Sniper | 90 | 80 | 3.0 | 200 |
  | Officer | 120 | 100 | 3.0 | 400 |
  | Shield | 70 | 140 | 3.2 | – |
  | Medic | 80 | 90 | 3.2 | 200 |
  | Repair | 90 | 100 | 2.8 | 300 |
  | Para | 0 | 90 | 3.4 | – (never a slot) |
  | Jetpack | 110 | 85 | 3.6 | 300 |

- **Machine statics** (:75, 85-93):

  | Unit | Cost | HP | Speed | Cooldown | Chassis |
  |---|---|---|---|---|---|
  | Breaker | 380 | 2800 | 2.2 | 650 | Tracked |
  | Maw | 350 | 3600 | 1.6 | 600 | Tracked |
  | Tusk | 260 | 2000 | 2.4 | 450 | Tracked |
  | Pincer | 320 | 2300 | 2.9 | 520 | Legged |
  | Kettle | 270 | 1600 | 2.3 | 560 | Legged |
  | Pavise | 360 | 2100 | 1.9 | 620 | Legged |
  | Censer | 240 | 1500 | 2.6 | 500 | Legged |
  | Banner | 400 | 1900 | 2.4 | 700 | Legged |
  | Redoubt | 330 | 3200 | 1.7 | 600 | Legged |

- **Ids** (:102-103, 116-127): Rifle 0, Assault 1, MG 2, Sniper 3, Maw 4, Tusk 5, Pincer 6, Kettle 7, Censer 8, Pavise 9, Banner 10, Redoubt 11, Officer 12, Shield 13, Medic 14, Repair 15, Para 16, Jetpack 17, Breaker 18. Ids 19-63 are free (`UnitDefinitionTests` uses spare id 20, `P\Tests\EditMode\UnitDefinitionTests.cs:22`).
- `Archetypes.Count = 64` and `Max = 63` (`P\Sim\Core\OrderGroup.cs:53-54`).
- Orders address groups, not ids (`OrderGroup.cs:16-20`, `Of()` at :26-42): Line, Marksman, Support, Raider.
- There is **no Sapper** in the code (grep finds 0 hits).

**`UnitDefinitions`** (`P\Sim\Match\UnitDefinitions.cs`)
- `struct UnitDef { byte Archetype; RosterEntry Roster; InfantrySpec Infantry; WeaponStats Weapon; TankSpec Machine; VehicleProfile Drive; }` (:23-31).
- `public static readonly UnitDef[] All = new UnitDef[0];` (:39). It is empty; the shipped units still come from the old switches.
- `Apply(SimWorld world)` (:45) and `Apply(SimWorld world, UnitDef[] defs)` (:48-70). The second:
  - writes `world.Units.Roster/Infantry`, `CombatCatalogueSystem.Weapon/Tank` and `VehicleKinematicsSystem.Profiles`;
  - skips an id `>= Archetypes.Count` (:55);
  - re-seals the fingerprints, then calls `world.FillRosters()` (:65-69).
- `MatchSim` calls `UnitDefinitions.Apply(World)` last (`P\Sim\Match\MatchSim.cs:136`).
- **A new unit is one `UnitDef`.** The worked example is `UnitDefinitionTests.Fake()` (:24-32).
- **Balance trick for tests:** call `UnitDefinitions.Apply(m.World, new[]{ tweakedDef })` on a live `MatchSim` to override any unit's numbers without editing sim code. It is idempotent (test at :55-63). It changes the `Units` fingerprint, which is folded into `Hash()` (`P\Sim\Core\SimWorld.cs:388`).

**`FactionRoster`** (`P\Sim\Core\FactionRoster.cs`)
- `SharedSlots = 3` (:22): slots 0-2 are Rifle, Assault, MG for every faction.
- **Slots** table, read by (faction, slot) (:25-51):
  - Iron: Rifle, Assault, MG, Officer, Shield, Repair, Maw, Pincer, Banner, Breaker
  - Brass: Rifle, Assault, MG, Sniper, Medic, Jetpack, Tusk, Kettle, Censer, Redoubt
  - British, German, French, Austro-Hungarian: Fiction units only (Maw, Tusk, Breaker). No walkers and no jetpack (:10-14).
- **Methods:** `Fill(roster, offset, faction)` :55, `Slot(faction, slot)` :61, `Pools` :70-90, `PoolCount/Pool/Fields` :98-107.
- **Ability bits** (:113-114): `HeBarrageBit=1`, `CreepingBarrageBit=2`, `ChlorineGasBit=3`, `SmokeScreenBit=6`, `StrafeRunBit=10`, `BeamBit=11`, `ParaDropBit=12`.
- `AbilityMask(faction) => Everyone | (Brass ? 1<<12 : 0)` (:117-118).
- `MayCall(faction, id)` requires `id < 32` (:119-120). **Ability ids must stay below 32.**
- `FactionId`: Iron 0, Brass 1, British 2, German 3, French 4, AustroHungarian 5; `Factions.Count = 6` (`P\Sim\Core\Faction.cs:13-21`).

### Abilities: `OffMapAbilities` (`P\Sim\Match\OffMapAbilities.cs`)
- `enum OffMapAbilityId : short` (:35-46): None 0, HeBarrage 1, CreepingBarrage 2, ChlorineGas 3, MustardGas 4, BomberRun 5, SmokeScreen 6, MortarSalvo 7, ReconFlight 8, ReinforcementSurge 9, StrafeRun 10, Beam 11, ParaDrop 12.
- `AbilitySlots = 16` (:102).
- `static bool TryGetStats(int ability, out AbilityStats s)` (:123-163). Only 1, 2, 3, 6, 10, 11 and 12 have stats (costs in silver, times in ticks):

  | Ability | Cost | Cooldown | Warm-up | Key numbers | Line |
  |---|---|---|---|---|---|
  | HE | 150 | 1200 | 80 | 12 shells, 150 dmg, r 8, supp 60, crater 3, spread 120 | :128-130 |
  | Creeping | 250 | 2400 | 120 | 40 shells, 120 dmg, 10 steps × 80 ticks × 6 m, SafeBehind 15 | :133-135 |
  | Chlorine | 120 | 1800 | 60 | conc 40, persist 240 | :138-140 |
  | Smoke | 60 | 600 | 40 | 40 m, conc 30, 600 ticks | :143-144 |
  | Strafe | 180 | 1800 | 100 | 32 × 60 dmg, 80 m | :147-148 |
  | Beam | 300 | 3600 | 80 | 60 m, sweep 120 | :152-153 |
  | ParaDrop | 260 | 1500 | 100 | r 12, 8 men | :156-157 |

- **Validation** (:193-200) rejects: a winner already set, a bad id or pattern, a target off the map, cooldown still running, not enough silver, the Beam cap, and `!FactionRoster.MayCall`.

### Match config: `SimConfig` (`P\Sim\Core\SimConfig.cs`)
- **Fields** (:12-39): `TickRate`, `InputDelayTicks`, `MaxSlots`, `Seed`, `SilverPerSecond`, `StartingSilver`, `EventCapacity`, `byte FactionA, FactionB`, `float HeroPity0, HeroPity1`, `FixedList32Bytes<byte> LoadoutA, LoadoutB`.
- **Loadouts:** empty means the faction's default. A shorter list falls back per slot (:32-38). `FillRosters` locks any slot whose entry has `Hp <= 0` (`SimWorld.cs:101-120`). Since the 2026-09-25 loadouts, the rosters are hashed (`SimWorld.cs:392-395`).
- **`Default`** (:45-56): TickRate 20, InputDelayTicks 3, MaxSlots 3584, Seed `0xC0FFEE`, SilverPerSecond 1, StartingSilver 120, EventCapacity 8192, Iron vs Brass.
- **Runtime economy is not the default.** `SimHost` sets `StartingSilver = 300` and `SilverPerSecond = 2f` (`P\Presentation\Core\SimHost.cs:36-37`, applied :129-130). `MatchLaunch.Request` has the same values (:49-50), as do `MissionCard` (:32-33) and `CampaignMission` (:36-37). Stress runs use `StressUnits*25` silver (SimHost.cs:129). `Data/Definitions.cs:157-158` holds 120 / 1.2, which is **not read at runtime** (`D\reference\tasks.md:448`).

### Combat tables
**`CombatTables`** (`P\Sim\Combat\CombatTables.cs`, placeholder data)
- **Constants** (:14-31): TrenchCover 0.75, CraterCover 0.35, CloseAssault 8 m / 600 dmg / 20 mm / 0.5 chance, MovingAccuracy 0.5, AdvanceFireRange 60, GarrisonFireSuppressionLimit 40, SmokeBlindMetres 12.5.
- **`WeaponFor(byte)`** (:33-74), as damage / range / rounds per second / accuracy:

  | Weapon | Damage | Range | RPS | Accuracy | Line |
  |---|---|---|---|---|---|
  | Rifle (default) | 36 | 130 | 0.5 | 0.55 | :72 |
  | Assault | 22 | 45 | 3 | – | :38 |
  | MG | 24 | 170 | 7 | 0.30 | :40 |
  | Sniper | 95 | 230 | 0.3 | 0.9 | :42 |
  | Breaker hull guns | 26 | 110 | 6 | – | :70 |

  Walkers carry no small arms (RangeMax 0, :62-68).
- `CooldownTicks()` :77; `RangeFalloff()` (1.0 inside half range, falling to 0.5 at max) :81-85.

**`TankSpec`** (`P\Sim\Combat\TankSpec.cs`)
- `For(byte)` defaults to Maw (:63-77).
- Entries: Breaker :84, Pincer :98, Kettle :110, Censer :128, Pavise :139, Banner :156, Redoubt :173, Maw :188, Tusk :197.
- Movement profiles: `VehicleProfile.*` in `P\Sim\Nav\VehicleKinematics.cs:95-127`. Redoubt has `Legs = 6` (:127), while its model has 4 (`D\reference\aosa\ASK.md:27-30`, A06).

**`DirectFireSystem`** (`P\Sim\Combat\DirectFire.cs`)
- `public readonly int[] Shots, Kills` per team since the match began. They are derived and not hashed (:41-43), incremented at :97 and :103.
- `NativeList<int2> Killed` holds this tick's (slot, killer) pairs (:61).
- Reach them as `match.Fire.Kills[team]`. `Fire` is null when `combat:false`.

### Winning, events, stats
- **Winner:** `SimWorld.WinnerTeam = -1` (:19). Set by `SectorControlSystem` when an HQ is captured (`P\Sim\Match\SectorControl.cs:1-7`, :118), or by Surrender (`SimWorld.cs:275-277`). It emits `MatchEnded` (a = winner).
- **Events per Step:**
  - `SimWorld.Step` clears `Events` first (`SimWorld.cs:233`), so read `m.World.Events.Events` after each `Step()`, or after `StepOnce` returns true.
  - Check `Events.Overrun` (`P\Sim\Core\SimEvents.cs:110`); capacity is 8192.
  - `Death`: a = slot, b = killer slot (>=0) or a `DeathCause` (<0: Blast -1, Gas -2, Crush -3, Burning -4, Beam -5) (:16-17, :89).
  - Also useful: `UnitDeployed` (a = slot, b = roster slot, dir.y = player, :70) and `VehicleDestroyed`.
  - Slots are reused after a death: read the team at dispatch, or check `Generation`.
- **`MatchStats` / `MatchReport`** (both in `P\Presentation\Core\MatchStats.cs`; there is no separate MatchReport file):
  - `MatchReport` fields (:14-27): Winner, DurationSeconds, MenLost, VehiclesLost, MenFielded, Kills, Shots, SilverNow/Start, TrenchesTaken/Held, ObjectivesTaken/Held, AbilitiesFired; `Accuracy(team)`.
  - Counted from events (:59-71). Kills and Shots come from `Local.Fire` (:88).
  - It needs a `SimHost` (MonoBehaviour), so a headless balance run should tally the events itself.

### Building matches
**`MatchSim`** (`P\Sim\Match\MatchSim.cs`)
- `CreateGreybox(SimConfig, bool combat=true)` :46-50
- `CreatePlaytest(SimConfig)` :53
- `CreateBattlefield(SimConfig, BattlefieldParams, bool combat=true)` :56-61. It sets `Bombardment.ShellsPerMinute`.
- Presets: `BattlefieldParams.ShelledForest(1917u)` (used at `StressPresetTests.cs:38`), `WinterLine`, `Landing` (`D\reference\tasks.md:70`).
- Registration order: :69-136.

**Two-sided match loop** (the worked example is `P\Tests\EditMode\SinglePlayerEquivalenceTests.cs`)
- `new LockstepSession(Func<MatchSim> newMatch, bool canary, int latencyTicks, int jitterTicks, float lossChance, uint seed)` (`P\Presentation\Core\LockstepSession.cs:43`). It is a SHOW assembly (`TW.Presentation`). In single player it sets `HashInterval = 0` (:59); a test sets it back to 1 (SinglePlayerEquivalenceTests.cs:45).
- `bool StepOnce(ScriptedEnemy ai)` (:65-77): at most one tick. Returns true when `Local` stepped.
- Player orders: `session.LocalDriver.Issue(SimCommand.Deploy(tick, 0, slot))` (SinglePlayerEquivalenceTests.cs:55). An order lands `InputDelayTicks` (3) later.
- **`ScriptedEnemy`** (`P\Presentation\Core\ScriptedEnemy.cs`):
  - Fields (:32-46): `Enabled`, `DeployEveryTicks` 40, `Attacks`, `DeploysTanks`, `AttackGarrison` 8, `UsesSupport`, `SupportReserve` 180, `StressUnits`, `StressAdvanceDelayTicks` 300, `StressSpread` false.
  - It plays **player 1**. The stress preset orders both sides (:70-74).
  - The test config is `DeployEveryTicks=20, AttackGarrison=6, UsesSupport=true, SupportReserve=100, StressUnits=60, StressAdvanceDelayTicks=120`, with `cfg.StartingSilver=4000` (:43-48). It runs 1000 ticks and asserts the enemy has more than 5 men.
- `SimCommand` (`P\Sim\Core\SimCommand.cs:7-34`): DeployUnit 1, TrenchAdvance 2, TrenchSelectAdvance 3, TrenchLock 4, TrenchFallback 5, UnitStance 6, SupportFire 7 (b = `AbilityArgs.Pack(heading, pattern, length)`), UnitAbility 8, SetRally 9, Surrender 10, TrenchHoldFire 11.
- **Removing hero randomness:** `m.World.GetSystem<HeroSystem>().TeamMask = 0` (`StressPresetTests.cs:32`).
- **`StressPresetTests`**: PerSide 200, 900 ticks, `ShelledForest(1917u)`, `ScriptedEnemy{StressUnits=PerSide, StressSpread=spread}`, both canary and single player (:19, :34-58). Knob `stress.spread=1`.

### Campaign and meta
**`CampaignGraph`** (`P\UI\Campaign\CampaignGraph.cs`)
- **Difficulties** (:22-27):
  - EASY: deploy every 60 ticks, garrison 12, no tanks, no support, reserve 400, tier 0
  - NORMAL: 40 / 8 / support / 180 / tier 1
  - HARD: 28 / 6 / tanks / support / 120 / tier 2
- **Mission defaults** (:35-38): Bombardment 8, StartingSilver 300, SilverPerSecond 2, "90 x 240 M". Seed `0xC0FFEEu ^ Seed` (:49).
- **Nodes** (:89-146), every one with `EnemyFaction = 1`:
  - lowlands: 3 missions (ShelledForest 1917/2201/2318)
  - the-coast: 3 (Landing 3001/3102/3203)
  - river-line: 4 (4101-4404)
  - high-ground: 4 (WinterLine 5001-5304)
  - the-citadel: 3 (6001-6203)
- Rewards 25 / 40 / 75 gold (:72).

**`FactionBuildings`** (the meta upgrade tracks, `P\UI\Campaign\FactionBuildings.cs`)
- Stage costs 40 / 80 / 150; track tiers 15 / 20 / 40 / 60 / 80 / 100; global rows 30 / 60 / 120; unlock 60 (:67-70).
- Per tier: SilverPerTier 50, IncomePerTier 0.25, ArmourMm 2, Ability% 8, PerTier 0.05 (:72-75).
- **Only starting silver and income reach a match** (`ApplyTo`, :334-340). Unit tiers, armour and ability masks wait for the sim upgrade seam, docs/21 B1 (header :1-8). `MatchLaunch.Request.AbilityMaskA/B` is HUD-only for now (`P\Presentation\Core\MatchLaunch.cs:64-70`; `D\reference\tasks.md:473-476`).

### Checklists (quoted)
**Adding a unit type** (`D\reference\tasks.md:175-195`)
> "This task spans both lanes. The SIM lane lands steps 1-2 as a seam commit first; the SHOW lane then does 3-6.
> 1. **Where it goes:** each faction fields ten slots (`RosterEntry.SlotCount`, the digit keys 1-0) from a larger pool (`Sim/Core/FactionRoster.cs`). A new unit joins a pool, or takes a slot from another unit.
> 2. Sim: a new archetype id (ids are a seam item; `Archetypes.Count` is 64) and one `UnitDef` in `Sim/Match/UnitDefinitions.cs`: its roster line with its `ChassisKind`, its `InfantrySpec`, its weapon, and for a machine its `TankSpec` and `VehicleProfile`. … code that asks whether a unit is a tank reads `ChassisKind` through `SimWorld.ChassisOf`, or `RosterEntry.ForArchetype(a).Chassis` where no world is at hand.
> 3. HUD: name, tooltip and portrait in `Presentation/Core/UnitLook.cs` …, the portrait stem in `UI/Skin/SkinSpec.cs` (`PortraitNames`), the pictures in `UI/Skin/Portraits/` and `UI/Resources/UnitArt/`. HudTextTests, HudBindTests and UnitArtTests fail until every archetype has them.
> 4. Art: infantry needs a figure in `Editor/VATBaker.cs` and a bake. Vehicles need a `Resources/Vehicles/<Name>/` folder … a walker also needs its id and model name in `TankRenderer`'s `CrabArchetypes` and `CrabNames` … (GaitTests).
> 5. The legacy IMGUI `Presentation/Camera/BattleHud.cs` reads the same `UnitLook` names; its icon array is `UnitLook.PortraitCount` long. No test runs OnGUI, so check it in Play with F9.
> 6. Tests to run: FactionRosterTests, UnitDefinitionTests, CrabTests or TankTests, GaitTests, HudTextTests, HudBindTests, UnitArtTests, then the full gate."

**Adding a support ability** (`D\reference\tasks.md:419-426`)
> "SIM first: `OffMapAbilityId` and its stats in `Sim/Match/OffMapAbilities.cs`, the asset in `Editor/SliceDefinitions.cs`, the factions that may call it (`FactionRoster.AbilityMask`). Then SHOW: `UI/HudView.cs` `SupportAbilities`; the slot counts in `UI/HudLayout.cs` and `Presentation/Camera/BattleHud.cs`; a `GameAction`, key and label in `Presentation/Core/KeyMap.cs`; `UI/HudHotkeys.cs`; name and card text in `UI/HudText.cs`; its icon in `UI/Skin/SkinSpec.cs`; `Presentation/Camera/TestPanel.cs`; the aim circle and effects in `CombatFx.cs`; the AI's choice in `Presentation/Core/ScriptedEnemy.cs`. Tests: SupportAbilityTests, HudBindTests, HudStructureTests, KeyMapTests, SkinAssetTests."

The patterns trap: "Add a pattern only to `AbilityStats.Patterns`, or the command is rejected" (tasks.md:116-118).

### Seam rules
- **Seam list** (`R\CLAUDE.md:57-64`):
  1. `SimWorld` fields and `Hash()`: append only, bump `ReplayRecorder.FormatVersion`, log it in `docs/02-contracts.md`.
  2. `RosterEntry.SlotCount`, archetype ids, `SimConfig`, asmdef reference lists, `ProjectSettings/`, the manifest and lock file.
  3. `docs/02` and `docs/03`.
  4. The house chunk mask, the env atlas grid, `VehicleSize`.
- "its own commit, alone, first" (:66-67).
- **SourceId bands** (`P\Sim\Core\SourceId.cs:18-33`): an ability is its own id 0..999 (`AbilityMax`), a unit's weapon is `UnitBase` 1000 + archetype, an emplacement is 1300 + kind, `CookOff` 2000, `Ship` 2001. See also tasks.md:120-123.
- **Current format:** FormatVersion 9 (`D\02-contracts.md:101`). The hash chain is pinned in `SimHashTests` (see C).

### Relevant decisions (`D\reference\decisions.md`)
- :13 Windows x64; determinism is same-build (Burst `FloatMode.Strict`).
- :16 "**3,000 units maximum.** `SimConfig.MaxSlots` = 3584".
- :17 8 unit types per player (ten slots since 2026-09-25).
- :28 giant machines: `VehicleSize` Walker 2.5, Tank 1.7.
- :43 trench bay factor 0.7.
- :50 Sapper, "a new `InfantryArchetype.Sapper` in both faction pools", laying mines by a `UnitAbility` command.
- :63 single player runs one world; the canary is opt-in.
- :70 units-meta: `ParaDrop` 12, streams 17-21, `FormatVersion` 9, every faction calls the six overhaul abilities and Brass alone the drop.
- **Open questions** (:72-96):
  - M1.5 fun-gate playtest not done (:74)
  - shelter protection (:75)
  - do houses give sim cover (:76)
  - **"Not scaled with the giant machines: trench cross width, slope limit, turn rates, speeds, `MaxGrow`. Balance, not geometry."** (:79)
  - record live matches (:81-82)
- `ASK.md` S04 (answered, :118-128): keep the 3,000 ceiling. Per living man the sim cost is flat (0.82 vs 0.87 µs per tick per 1,000 men).
- The laptop's House5 balance job (`G:\My Drive\TW3D-pipeline\LAPTOP_STEP_house5_balance.md:15-22`, done: `LAPTOP_DONE.txt` "house5--balance--e6300e32 PASS board 49a10ce") found two HP scales: battle 1.4 stone / 1.0 timber, playground 60 / 30.

### Roster v3 proposal (`G:\My Drive\TW3D-pipeline\unit-roster-v2.md`; the title reads "(v3)")
- **Core idea** (:3-4): each faction owns one step of the trench assault, and war-winning weapons are *built during the match by labour*.
- **Assault cycle** (:40-47), step / owner:
  - Prepare: British (industrial attrition)
  - Cross: Iron (machines)
  - Break in: German (storm troops)
  - Hold: French (Verdun)
  - Deny: Austria-Hungary (mountain war)
  - Exploit: Brass (raiders)
- **Factions** (:58-105). Each gets a doctrine, slots 3-9, a Great Work, a commander and cards:
  - British "Big Push": artillery 20%* cheaper. Livens battery; the Tank Theorist.
  - German "Infiltration": +30%* speed in smoke or gas. Railway gun; the Ace.
  - French "Voie Sacrée": 30%* faster redeploy and a forward dump. Ouvrage; the Defender.
  - A-H "Mountain war": +1 charge, mines arm in 0.5 s. Mine gallery; the Warden.
  - Iron "Machines first": 20%* faster redeploy. Leviathan; the Engineer-General.
  - Brass "Air-mobile": ParaDrop only. Dreadnought airship; the Raider.
- **Units** (:114-137):
  - Infantry rules: Mad minute, Terror, Back to duty, Sapper (builds at full speed, others at 1/3*).
  - AT rifle, Flamethrower, Mortar team, Field gun.
  - Machines: Mark V, A7V, Whippet, Supply lorry, AA lorry, Romfell. The rest of Iron and Brass stay as today.
- **Sky** (:145-153): the balloon speeds up artillery warm-up by 25%*, the AA lorry is the counter.
- **Great Works rules** (:161-169):
  - one per side;
  - paid in sapper-seconds plus 60* silver;
  - both sides see the progress bar;
  - an explosion sets it back;
  - target about 4 min with 2 sappers (~480* sapper-s).
  - Per-work table: :171-178.
- **Commanders** (:189-205): a command post (destroyed means the passive is off for 90 s*), a Resolve meter instead of a cooldown, and a local signature ability with a counter. Role titles only, for legal reasons (:194-196).
- **Costs** (:211-227). **Build order** (:229-235):
  1. Sapper plus the Great Work framework, proved on the Livens battery
  2. Balloon and AA
  3. The Defender and the Ace
  4. Ouvrage, mine gallery, railway gun
  5. Wheeled and historical armour
  6. Leviathan and airship
- **Five decisions** (:241-247):
  1. Great Works built by labour, bar visible to both sides?
  2. Real commander names?
  3. Keep the invented Brass and Iron commanders?
  4. One Great Work per side, or a choice of two?
  5. The card id 10 clash. **Already resolved:** ParaDrop = 12 (decisions.md:70; `tw3d-offmap-ability-id-10-clash.md:11-15`).
- **Where the proposal does not match the code:**
  - Its "1.2 silver/s" (:18) and price-in-seconds table (:25-27) assume 1.2. The game runs at 2.0/s from 300 (SimHost.cs:36-37).
  - Its weapon columns are the docs/06 designs (`D\06-units-and-factions.md:11-20`), not `CombatTables`. For example, the rifle is 25 dmg / 60 m / 0.8/s in the proposal and 36 / 130 / 0.5 in code.
  - The Sapper, Great Works, balloon, commanders and wheeled units do not exist in code.
