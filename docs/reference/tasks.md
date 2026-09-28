# Where things are: task → files → tests → how to see it

Read this second, after `CLAUDE.md`. Find the area your task touches, open the files it names, run the tests it
names, and look at the result the way "See it" says. Paths are under `trench-warfare-3d/Assets/_Project/`, except
`Tools/...` (under `trench-warfare-3d/`) and `docs/...` (repo root). A test class lives in the file of the same name
in its module's folder: `Tests/Sim/`, `Tests/Match/`, `Tests/Show/`, `Tests/UI/`, `Tests/Project/` or `Tests/PlayMode/`
(`Tests/EditMode/` holds only LandingFolder, for lanes cut before the split: `workflow.md`, section 5). Two lists at
the bottom are generated from the code by `Tools/codemap.py`: which component sets and reads each `SceneHooks`
member, and every test class by module.

A row that says **Tests: none** means nothing will go red if you break it. Look at it in Play. A sentence tagged
(until "<subject>" lands) describes code another lane has already fixed on its branch, in the commit with that subject;
`validate.py` fails once that commit is on your branch, so the sentence gets rewritten then.

A new link between presentation parts that must not reference each other (the effects and the debug panel, say):
add a member to `SceneHooks` (`Presentation/Core/RenderGround.cs`; the generated table at the bottom lists them)
rather than a new static or a reference to the other part. Audit R2 will move the hooks into services together.

## Worked examples

- **"Make the walkers leave deeper footprints."** Ground marks and footfalls, below. Three knobs: how dark and
  pressed-in the print looks is `_Shape` 2 in `Shaders/GroundMark_URP.shader`; its size is the pad passed to
  `SceneHooks.FootFall` in `TankRenderer.cs`; how long it lies is the `FootFall` lambda in `CombatFx.cs` `Start`
  (110 s mud, 300 s snow), which calls `AddMark` in `CombatFx.Ground.cs`. No test covers it. See it: spawn a
  walker (archetypes 6-11) and shoot it top-down, recipe in `workflow.md` section 6.
- **"Add a new unit type."** Adding a unit type, below: a checklist across sim, HUD, art and the capture tool,
  and the slot count is a seam change.
- **"An EditMode test fails."** Search this page for the test class name: the row that names it under **Tests**
  is its area, and that row's files are the code it guards. Before believing a red from an editor that has been in Play, read
  `workflow.md`, "False reds". A real red in the other lane's code is theirs to fix: write a note in `docs/inbox/`
  with the test name and the failure message, and do not patch their files.
- **"The game looks wrong after my change."** `workflow.md`, "See the game": capture with `CaptureRig`, then
  read the numbers with `python Tools/shotstats.py`. Do not judge brightness or colour by eye.

## Sim (SIM lane: `Sim/**`, `Net/**`, `Data/**`)

### Sim core: world state, commands, hash, replay
- **Files:** `Sim/Core/SimWorld.cs` (all per-slot arrays, `Hash()`), `Sim/Core/SimCommand.cs`, `Sim/Core/SimEvents.cs`,
  `Sim/Core/SimConfig.cs`, `Sim/Core/Replay.cs` (`FormatVersion`), `Sim/Match/MatchSim.cs` (system registration),
  `Sim/Core/ISimSystem.cs` (order constants). System order table: `code-map.md`. Helpers: `Sim/Core/SimHash.cs`
  (FNV-1a), `Sim/Core/SimRandom.cs` (every random draw from seed, tick, system and slot), `Sim/Core/SimMath.cs`
  (platform-independent maths), `Sim/Core/UnitPose.cs` (the pose stream to rendering), `Sim/Core/PerfMarkers.cs`
  (profiler markers), `Sim/Match/TerrainHashSystem.cs` (puts the ground in the hash). The cross-platform gate:
  `Editor/DeterminismPlatformReport.cs`.
- **Tests:** SimHashTests, DeterminismReplayTests, CommandValidationTests, HashIntervalTests, DeathEventContractTests
  (what `Death.b`, `dir` and `scalar` mean).
- **Trap:** `Hash()` is an ordered chain. Append, never insert. A new array must be hashed and must bump
  `FormatVersion`. Contracts in `docs/02-contracts.md`, rules in `docs/03-determinism-rules.md`.

### Lockstep, one world, canary
- **Files:** `Net/LockstepDriver.cs`, `Net/LoopbackTransport.cs`, `Net/CommandSeat.cs` (SIM lane), and on the
  SHOW side of the seam `Presentation/Core/LockstepSession.cs`, `Presentation/Core/SimHost.cs`,
  `Presentation/Core/ScriptedEnemy.cs` (the AI). The folder decides the lane: those three are SHOW files.
  Interfaces `Net/ILockstepTransport.cs`, `Net/ICommandSink.cs`; `Net/UtpTransport.cs` is a stub (multiplayer is
  deferred).
- **Tests:** LockstepLoopbackTests, BattlefieldLockstepTests, CommandSeatTests, SinglePlayerEquivalenceTests,
  CanaryFixture (makes every PlayMode SimHost run the canary).
- **Trap:** single player runs ONE world, so `SimHost.Peer` is null in Play. Write through
  `SimHost.WriteWorlds(...)`, never into `Local.World` by hand, or the canary desyncs.

### The enemy (`ScriptedEnemy`, SHOW)
- **What it does (2026-09-29):** deploys an armed man every `DeployEveryTicks` while it can pay, and keeps
  `SupportReserve` back only once it fields the men for the attack it wants (`Odds` times the player's front garrison,
  counting men still walking up; saving at parity held it at the player's count, seen in Play); locks its rear trenches so men walk through to
  the front; goes over the top when its front garrison is at least `AttackGarrison` and `Odds` + 1 (three) times the
  player's front garrison, or, at `Odds` (two) times, first lays an HE line 60 m along the player's garrison and, if the
  silver runs to it, a 40 m smoke screen just in front of it, and goes `BarrageLeadTicks` later (`PlannedAttack`);
  shells or gasses the player's front only out of silver it has beyond the reserve. When it goes over the top its
  machine gunners stay on the parapet and fire over the attack (`OverTheTop`: every group but `OrderGroup.Gun`, v22).
  On Normal and Hard (`Defends`, `SimHost.PeerDefends`, the missions' and campaign's difficulty; Normal since the
  owner's decision of 2026-09-29) it answers an attack: five or
  more of the player's men in the open 8-70 m before its front trench bring an HE line down on them (`Sos`, the SOS
  barrage), and while the player's front garrison is at least eight and no smaller than its own it keeps the
  barrage's 150 silver in hand. Off on Easy, which fires no support at all. One barrage broke a bare three-to-one
  attack every time (`TheSosBarrage_BreaksABareAttackAtThreeToOne`; seven of eight taken without it).
- **Tried and dropped (2026-09-29), so nobody repeats them:** (1) calling back a failed attack (`TrenchFallback` when it
  stalls, or loses half its men short of the trench): it fired every time and saved nobody, 17.3 of 20 lost against
  17.4, since the way back is as open as the way in. (2) Buying by a true round-robin: 23 men in ten minutes, a third
  of them officers, shields and jetpack men, and it broke a passive player once in eight instead of seven. (3) Every
  fifth man a machine gunner: the script against itself stalemated seven times in eight. Its deploys are 85 %
  riflemen because the rotation runs on the clock and skips a slot it cannot pay for; per silver a gunner kills about
  0.085, a sniper 0.05-0.08, a rifleman 0.019 (the script against itself, Iron against Brass, eight seeds). Brass beat
  Iron in 7 of the 8 matches that ended, from either side, on its gunners and snipers: faction balance, the owner's. `Side` is the seat it plays (1;
  0 puts it on the player's side). `Said` reports each attack decision with its count.
- **Why:** the assault ladder says a trench falls at about three to one bare and two to one behind support, and a
  garrison with machine guns falls only behind support (two gunners in ten: bare attacks fail at three to one, two to
  one behind smoke and a barrage takes it four times in four). The enemy before 2026-09-29 attacked with eight men
  whatever stood before them, and from two minutes in spent every coin on harassing fire and never deployed again.
- **Tests:** SteadyStepTests (over the scripts' own match, no man's step is turned back on the one before: to his post in the trench, closing on an enemy in the open), MatchLoopTests (ten-minute matches against a player who only defends: the enemy keeps deploying, attacks
  only with the odds, never sits on silver while it is short of men for the odds, and breaks him on two seeds of three;
  `Report_TheMatchLoop`, Explicit, prints four policies
  including the script against itself), CampaignGraphTests (difficulty presets), SinglePlayerEquivalenceTests,
  BehaviourBenchTests (`Report_HowEveryUnitBehaves`, Explicit: the script on both seats, fielding machines, three
  seeds; every unit scored on what a player sees go wrong, worst unit named; deterministic, so two reports are an A/B),
  BalanceSweepTests (`Report_TheSweep`, Explicit: the same matches, or the assault ladder, on other numbers. A variant
  is data, a unit's field, the match's config or a script's knob, written before the first tick; it reports attrition,
  time to breach, trench retention and who wins, per seed. `python Tools/sweep.py run Tools/sweeps/factions.json`:
  `workflow.md`, section 5. Its two plain tests hold that a patch writes the field it names and fails on any it
  cannot, and that a variant changing nothing is the same ladder).

### The stress preset (thousands of men for perf work)
- **Files (SHOW):** `Presentation/Core/SimHost.cs` (`StressUnits`, `StressOverride`), `Presentation/Core/ScriptedEnemy.cs`
  (`StressSide` deploys both armies at their spawn points), `Perf/BenchOptions.cs` (`stress=`),
  `Editor/CaptureRig.cs` (`Bench`, and `Stress`: a rough 2,000-man footprint check).
- **Tests:** SinglePlayerEquivalenceTests (runs the preset with 60 men a side), BattlefieldLockstepTests,
  StressPresetTests (the player's army spread over its trenches).

### The VFX pass (every effect filmed for a critique, 2026-10-01)
- **Files (SHOW):** `Editor/VfxLab.cs` (stages one effect at a time by name: a shell, the HE barrage, a machine's
  cook-off and burning wreck, a machine alight, chlorine, the smoke screen, the beam, the strafe run, an incendiary on
  a row, a star shell, rain), `Tests/Stills/VfxStills.cs` (films each as a contact sheet and a play-zoom shot;
  `TW_VFX_SCENES`, `TW_STILLS_FIELD`, `TW_STILLS_DIR`).
- **Tests:** VfxStills (Explicit: run by name with a graphics device; it only checks that stills were written).
- **See it:** numbers you can compare: `TW.Editor.CaptureRig.Bench("stress=1000 settle_ticks=1800 ticks=400
  quality=5 out=C:/abs/a.json")`, or `-twbench "..."` in a build, then `python Tools/perfcmp.py` (workflow section 7).
  `CaptureRig.Stress(1000, path)` runs 120 real seconds with no fixed tick or hash: never the same fight twice.
  Unattended, from batch mode (a knob's A/B, one launch a side): BenchRuns (`Tests/Stills`, explicit, run by name with
  a graphics device; `TW_BENCH_RUN` the bench string, its `out=` outside the checkout), then `Tools/aosa/cmp.py`.

### Map generation and ground (sim side)
- **Files:** `Sim/Terrain/BattlefieldGenerator.cs` (`BattlefieldParams` presets: `ShelledForest`, `WinterLine`, `Landing`),
  `Sim/Terrain/MapData.cs`, `Sim/Terrain/CraterStamp.cs`, `Sim/Terrain/Heightfield.cs`, `Sim/Terrain/WireBelt.cs`,
  `Sim/Terrain/MudField.cs`, `Sim/Terrain/PropDef.cs`, `Sim/Match/Deformation.cs` (the only thing that edits the map).
  Contracts `Sim/Terrain/MapStructs.cs`, `Sim/Terrain/NavLayer.cs`; `Sim/Terrain/GreyboxMapGenerator.cs` is the
  flat corridor the determinism tests use.
- **Tests:** BattlefieldTests, DynamicGroundTests, CoastTests, WinterMapTests.
- **Trap:** a feature tested only on the playtest map is untested. BattlefieldTests runs the generated map.

### Movement, flow fields, garrison, trench orders
- **Files:** `Sim/Nav/FlowFieldManager.cs`, `Sim/Nav/FlowField.cs`, `Sim/Nav/MovementSystem.cs` (`MoveJob` decides
  stance), `Sim/Nav/SeparationJob.cs`, `Sim/Units/TrenchGarrison.cs` (class `TrenchGarrisonSystem`),
  `Sim/Units/TrenchOrders.cs`, `Sim/Core/TrenchPost.cs`, `Sim/Core/StanceRules.cs`,
  `Sim/Nav/SpatialHash.cs` (1 m buckets, built in slot order so neighbour order is deterministic),
  `Sim/Core/Lane.cs` (each man's and machine's own line across the field), `Sim/Nav/VehicleKinematics.cs` (`Laned`).
- **Tests:** FlowFieldTests, FlowFieldManagerTests, GarrisonAndOrdersTests, GarrisonTests, TrenchSpreadTests,
  PlaytestMapTests, SpreadAndEngageTests (the march and the battle on ShelledForest, measured), LaneAndEngageRulesTests.
- **How men spread (2026-09-28, the owner: "walking in rows in seemingly defined paths"):** four rules, each of which
  put a company in file on its own. (1) A flow cell points at the neighbour its cheapest way on goes through, straight
  before diagonal (`FlowField.BuildJob`; it used to take the lowest integration alone, ties to the lowest direction, so
  every cell of open ground before a wide goal pointed north-east). (2) A trench wall is crossed anywhere, at
  `FlowField.ParapetCost` (`CanStepInfantry`; ladders were the only way in or out). (3) Every man keeps to a lane
  (`Lane.Of(slot, generation, width)`, stateless), is deployed on it (`SimWorld.Deploy`), and `MoveJob.Turned` turns
  the flow toward it by up to `Lane.Pull`, only where the cell that way is no further from the goal, and into wire or
  a trench only where the field itself goes. Lanes apply to trench and objective goals, not to a rally point or a
  cell. (4) Mud costs what it takes (`NavCosts.Mud` 2, was 4). What still gathers men is the map: a wire belt has two
  or three gaps, the river its fords.
- **See it:** `python Tools/otr.py SpreadAndEngage` (or the class in the editor) leaves three files in the temp
  folder, all named tw-...: the numbers (spread-engage, a text file) and the tracks of the first march and the first
  battle (tracks-march and tracks-battle, csv). `python Tools/tracks.py <csv> <png>` draws where everybody walked.
  Look at the picture before believing the numbers. Measured on ShelledForest 1917 / 1918, 48 men, before and after: men in
  file 0.44 / 0.47 and 0.21 / 0.16; 3 m strips of the width in use 20 / 19 and 26 / 27 of 30; traffic on the five
  busiest strips 0.57 / 0.56 and 0.37 / 0.28.
- **Trap:** `StanceSystem` is a stub. Stance is written in `MovementSystem.cs` as `StanceOf[i]`.
- **Trap:** `CommandType.TrenchSelectAdvance` carries a mask of `OrderGroup`s in `B` (since A5c, 2026-09-25), NOT of
  archetype ids: `TrenchOrders.Advance` tests `groupMask & w.Units.Infantry[archetype].Group`. Groups: Line (rifle,
  assault), Marksman, Support, Raider, and since replay v22 (2026-09-29) Gun (machine gunner, Sentry), so "send the
  line over" leaves the guns on the parapet to cover it. The HUD's category chips (`TrenchOrderCluster`,
  `HudController`) sent `1 << archetype` until `lane/show/order-chips`: picking the riflemen sent the guns too, and
  picking the gunners sent the officers and medics. GarrisonAndOrdersTests.

### Infantry combat
- **Files:** `Sim/Combat/TargetAcquisition.cs`, `Sim/Combat/DirectFire.cs`, `Sim/Combat/Suppression.cs`,
  `Sim/Combat/CombatTables.cs` (placeholder weapon data), `Sim/Combat/HeightfieldRaycast.cs` (line of sight).
  Data structs `Sim/Combat/CombatStructs.cs`, `Sim/Units/UnitStats.cs`, baked from ScriptableObjects by
  `Data/DataBaker.cs` (the assets themselves are made by `Editor/SliceDefinitions.cs`). What a man does besides
  shoot is data in `Sim/Core/InfantrySpec.cs` (no system switches on the archetype id): the officer's aura
  `Sim/Combat/Aura.cs` (`AuraSystem`, asked through `Sim/Core/IAuraProvider.cs`; `DirectFire` reads its `DamageMul`
  and `SuppressionMul`), the hero moment `Sim/Combat/HeroSystem.cs`, the jetpack's leap `Sim/Combat/Leap.cs`, the
  medic and the repair engineer `Sim/Units/Support.cs` (a machine with a healer's spec heals too: the Mercy), the shield
  bearer's plate in `DirectFire` and `TargetAcquisition`, `HuntsArmour` in the same two (a machine's small arms or a man's
  anti-tank rifle at a plate they beat), `NeverPinned` in `Sim/Combat/Suppression.cs` (the Death Battalion).
- **Tests:** CombatTests, HeightfieldRaycastTests, OfficerTests, HeroTests, ShieldTests, JetpackTests, SupportUnitTests,
  ProvingGroundBehaviourTests (the ambulance, the anti-tank rifle, the never-pinned rifleman; each fails on the sim
  before 2026-09-28), SpreadAndEngageTests, LaneAndEngageRulesTests, GrenadeTests, AssaultLadderTests, DeadGroundTests.
- **The fight on foot (2026-09-28, the owner: "go out of their way to attack each other"):** `Sim/Combat/Engage.cs`
  (`EngageSystem`, order `SimSystemOrder.Engage`, just before Movement). A man in the open goes after the man he is
  shooting at, or else the nearest enemy in the open within `HuntRadius` (70 m x `CombatTables.RangeScale` 0.8: the
  owner cut every attack reach by a fifth, 2026-09-28); he closes on him in a straight line
  over open ground and holds at `HoldDistance` (half his weapon's range, 48 m under a `>>` order, a braced gun at
  three quarters), where he kneels, faces him and shoots without the moving penalty. It writes
  `MovementSystem.Engage` / `EngageDir` and `MoveJob` walks it. Not hunted: men in a trench (stormed along the goal's
  field), machines (except by a weapon that beats their plate), anyone across wire or a trench. Not hunting: a
  garrison, pinned men, medics, engineers, a sapper on his errand. The numbers are constants at the top of the file.
- **The assault (2026-09-28, the owner: "make it even better, the best game"):** the front has to be able to move.
  Three rules in `DirectFire`, their numbers in `CombatTables`: (1) a man in the open running faster than
  `RunningTargetSpeed` is a harder mark the farther off he is (`RunningTarget`: no change inside 20 m, a third of the
  chance left past 60 m); (2) a miss at a man on the fire step suppresses him fully (the round strikes the parapet);
  (3) the bomb: a man in the open, not pinned, with a bomb left, whose target is a man in a trench 5-22 m off throws
  one instead of firing (`Throws`). It is in the air `GrenadeFlightTicks` (0.3 s and 0.04 s a metre: half a second at
  5 m, 1.2 s at 22 m; since 2026-09-29, v21, so the throw can be drawn) and goes off where it lands, thrower alive or
  not, as an `Impact` on `BlastSystem` (so the bay still saves some of it), he holds it while a friend stands within 4 m of the mark, riflemen carry two and assault men four
  (`GrenadesFor`), and the count is per slot in `DirectFireSystem` (refilled for the next man, in the hash).
  `SimEventType.GrenadeThrown` (thrower, target, from, flight, metres) is there for the picture to draw the throw:
  it lands at from + flight, `GrenadeFlightTicks(metres)` later.
- **The assault ladder** (`AssaultLadderTests`: a garrison of ten on ShelledForest 1917, four seeds a rung). Trenches
  taken out of four, before these rules and after them:

  | Attack | 1:1 | 2:1 | 3:1 |
  |---|---|---|---|
  | bare, before | 0 (garrison lost 0 %) | 0 (0 %) | 0 (9 %) |
  | bare, after | 0 (0 %) | 0 (36 %) | 4 |
  | smoke / barrage at two to one, before | | 0 / 2 | |
  | smoke / barrage at two to one, after | | 4 / 4 | |

  So: an equal attack with nothing behind it takes nothing; two to one is beaten off but costs the garrison a third,
  so the next wave finds a thinner line; three to one, or two to one behind smoke or a barrage, takes the trench.
  `Report_TheAssaultLadder` (Explicit) prints the whole table for a tuning pass. Since dead ground (below) a bare two
  to one over eight seeds takes 1 and costs the garrison 22 % (38 % without it); the test asks eight seeds for 15 %.
  **The guns (v22):** twenty attackers with four machine gunners against ten with two, eight seeds: bare, sending the
  guns over takes 0 and loses 18.3 a seed, leaving them on the parapet takes 1 and loses 14.1 (the garrison 3.5
  instead of 0); behind smoke and a barrage both take 8 of 8, and the attack loses 2.9 with the guns covering against
  5.6 with them going over (`GunsThatStayToCover_CostTheAttackFewerMen` asks under 0.8 of it).
- **Dead ground (2026-09-29):** `TargetAcquisition` does not let a shooter on the far side of a team's front trench see
  a man of that team on foot in the open more than `CombatTables.DeadGroundMetres` (10 m) behind it, measured at his
  column (`FrontZ`, rebuilt when a front moves). He is on the approaches. Before it a machine gun (170 m) in the enemy's
  front trench killed the player's reinforcements between their spawn and their lines: in ten minutes of a match the
  player lost 17 of 23 men without an attack being made, and no army ever grew past ten (`MatchLoopTests` on the show
  lane). Artillery, gas and aircraft still reach him.
- **Hand to hand and the pounce (2026-09-28, lane/sim/melee):** `Sim/Combat/MeleeSystem.cs` (order 1109, over Engage's
  orders): within 8 m a man charges the nearest enemy man (over open ground, into or along the foe's trench) and at
  2.5 m both fight (`Stance.Melee`, `MovementSystem.EngageMelee`), blows every 1.2 s (`MeleeBlow`); `KeepsRifle` says
  rifle or fists (fists men throw the weapon down: `UnitFlags.Disarmed`, `WeaponDropped`/`WeaponPickedUp`); `Melee`
  and `Disarmed` hold fire (DirectFire) and leaps. `Sim/Combat/PounceSystem.cs` (order 1125, after Kinematics): a
  walker with claws crouches, leaps onto a man within 10 m of its front and lands on him (`PounceCrouched`,
  `PounceLanded`), never onto a trench, a wall, a machine or its own men. Tests: `MeleeTests`. The picture's half is
  tw3d-board item `melee` (docs/inbox/2026-09-28-all-melee.md).

### Shells, barrages, gas, fire
- **Files:** `Sim/Combat/Blast.cs` (`BlastRules`: trench bay 0.7, traverse 0.5; `BlastShape`; `Impact.SafeBehind`),
  `Sim/Match/OffMapAbilities.cs` (HE disc / line / box, creeping barrage, chlorine point / creeping, smoke screen,
  strafe run, the beam, the paratroopers `ParaDrop` (Brass only); `ScheduledPayload`, `PayloadKind`), `Sim/Core/AbilityArgs.cs` (heading, pattern, length in
  `SimCommand.B`), `Sim/Match/AmbientBombardment.cs`, `Sim/Combat/GasSmokeField.cs` (the gas field and the smoke
  field), `Sim/Combat/SmokeLos.cs` (metres of thick cloud on a line; read by `TargetAcquisition` and `DirectFire`),
  `Sim/Combat/Burning.cs` (`BurningSystem`: men and ground alight, reads `Blast.Resolved` for `BlastShape.Incendiary`),
  `Sim/Combat/BeamSystem.cs` (`BeamSystem`: the sweeping beam, started by the Beam ability; men, hulls, fire, the
  scorch of `BlastShape.Beam`), `Sim/Combat/Mines.cs` (`MineSystem`: mines and tripwires, `BlastShape.Mine`; a crater
  cooks them off; its trigger reads the match's vehicle profiles), `Sim/Units/Sapper.cs` (`SapperSystem`: the man who
  lays them, on a `CommandType.UnitAbility` carrying `UnitAbilityId.LayMine` / `LayTripwire` from
  `Sim/Units/SpecialAbilities.cs`; his charges are `InfantrySpec.MineCharges`), `Sim/Match/Deformation.cs`.
- **Tests:** SupportAbilityTests, DirectionalBlastTests, BurningSystemTests, AbilityArgsTests, StrafeRunTests,
  BarragePatternTests, SmokeScreenTests, BeamTests, MineTests, SapperTests, AirDropTests.
- **Laying a mine** is a sapper's errand (2026-09-28): `CommandType.UnitAbility`, `a` = his slot, `b` = the ability id in
  the low byte and `AbilityArgs` above it, `pos` = the point. `MineSystem.Place` stays the system call underneath
  (tests and tools may still call it). DeterminismReplayTests deploys a sapper and orders him by command, so the
  serialized replay verifies `SapperSystem`; no HUD issues the order yet (the Proving Ground's panel will).
- **Trap:** a sapper walks on a flow-field CELL goal, and the goal table is small (`FlowFieldManager.MaxGoals`). Ask for
  one with `TryGetGoal` (it answers -1, `GetGoal` throws) and make a finished one over with `Retarget`; SapperTests
  runs more errands than the table has goals.
- **Trap:** a line starts at `pos` and runs along the heading (0 = +Z, 90 = +X) for the length; `B = 0` is the plain
  ability at its own length, so every older caller still works. Add a pattern only to `AbilityStats.Patterns`, or the
  command is rejected as one the ability does not offer.
- **Trap:** a trench never caves in, by owner decision (`decisions.md`).
- **What caused an explosion** is `Impact.Source` (`Blast.cs`), sent as `Explosion.a`, in the bands of
  `Sim/Core/SourceId.cs`: an ability is its own id (0-999), a unit's weapon 1000 + archetype, a map gun 1300 + kind,
  a cook-off 2000, the fleet 2001. Anything new that queues an `Impact` takes its number there. Nothing in SHOW reads
  `Explosion.a` today.

### Objectives, money, victory, the debrief
- **Files:** `Sim/Match/SectorControl.cs` (an objective flips when enough infantry hold it; sets `WinnerTeam` when a
  side holds them all), `Sim/Core/SimWorld.cs` (`Silver` income each tick, deploy cost, `CommandType.Surrender`),
  and on the SHOW side `UI/ObjectiveTracker.cs` (the objectives list and the centre banner),
  `Presentation/Core/MatchStats.cs` (what the debrief counts, from the event pump), `UI/Shell/DebriefScreen.cs`.
- **Tests:** BattlefieldLockstepTests, CombatTests and PlaytestMapTests reach `WinnerTeam`; ShellUxmlTests loads the
  debrief. Nothing tests `SectorControl`, `MatchStats` or `ObjectiveTracker` directly.

### Stubs: placeholders for planned phases, not wired
`Sim/Combat/IndirectFire.cs`, `Sim/Match/Logistics.cs`, `Sim/Match/MissionScript.cs`, `Sim/Match/WaveAi.cs`,
`Sim/Units/Grenades.cs` (unregistered systems that throw, `code-map.md`; `Sim/Units/SpecialAbilities.cs` is only the
`UnitAbilityId` enum now, its stub system deleted 2026-09-28),
`Net/HashExchange.cs`, `Net/Snapshot.cs`, `Presentation/Audio/EventAudioRouter.cs`, `Presentation/VFX/EventVfxRouter.cs`
(each its assembly's only file). Building one out is a feature, and for the sim ones a hash change.

### Fire in the sim
- **Files:** vehicles burn: `Sim/Units/VehicleModules.cs` (`Fire`, `StartFire`, `UnitFlags.Burning`, the
  `VehicleOnFire` event). Men and ground cells burn in `Sim/Combat/Burning.cs` (`BurningSystem`, registered in
  `MatchSim`; it reads `Blast.Resolved` for incendiary bursts, the beam lights men too, and since 2026-09-28
  `Sim/Combat/DirectFire.cs` lights the man and the ground a weapon with `WeaponStats.SetsBurning` hits: the
  Flamethrower). Changing what it hashes is
  a hash and replay change (a seam commit).
- **Tests:** TankMobilityTests (vehicle fire), BurningSystemTests (men and ground), FlamethrowerTests (a hit lights the
  man and the ground, a miss the ground, a plate does not stop it, a rifle lights nothing).

### Vehicles in the sim: tanks and walkers
- **Files:** `Sim/Core/RosterEntry.cs` (`VehicleArchetype`, default roster, `SlotCount`), `Sim/Combat/TankSpec.cs`,
  `Sim/Combat/Armor.cs`, `Sim/Combat/TankGunnery.cs`, `Sim/Units/VehicleModules.cs` (legs, tracks, crew, fire,
  when a vehicle is destroyed), `Sim/Nav/VehicleKinematics.cs` (`VehicleSize`, `VehicleProfile`, momentum and steering, trench crossing, crushing),
  `Sim/Core/ChassisKind.cs` (what a unit stands on, `Foot`/`Tracked`/`Legged`/`Wheeled`: a field of `RosterEntry`,
  read through `SimWorld.ChassisOf`; it replaced the id ranges of `IsTank`/`IsWalker`/`IsArmoured`),
  `Sim/Units/Breaker.cs` (the Breaker's halt, wind-up, charge and back-off). The wreck itself is a map prop made in
  `Sim/Match/Deformation.cs` (`Sim/Terrain/PropDef.cs`, `MapData.AddProp`); which vehicle it was, what killed it and
  how whole it was are kept in `DeformationSystem.Wrecks` (`WreckRecord`), not on the prop. A wreck breaks in stages
  (`PropRules.Next`: Wreck, BrokenWreck, Scrap, Cleared; a cleared prop keeps its index), each with less cover, scrap
  no longer blocking. Its hit points (`PropRules.StartHp(kind, scale)`) are sized to its machine through
  `PropDef.Scale` (`PropRules.WreckSize`); `DeformationSystem.Shake` wears it a stage a blast, measured
  `WreckBlastReach` off its middle, and a cook-off spares the wreck it made. A heavy machine grinds down a wreck it
  brushes or pushes against and any machine flattens scrap it drives over (`Sim/Nav/VehicleKinematics.Wrecks.cs`);
  both wear through `Sim/Terrain/PropHarm.cs`, one stage at a time, as do machine-gun rounds a wreck's cover stopped
  (`Sim/Combat/DirectFire.Wrecks.cs`, `CombatTables.WearsWrecks`). A shot at a wreck names it in `Shot.b` as
  `Sim/Core/PropTarget.cs` encodes it (`-2 - prop`): a gun with nobody to shoot at fires at a wreck its enemies are
  behind (`DirectFire.Wrecks`, `Shelter`).
- **Tests:** TankTests, TankMobilityTests, DriveFeelTests (momentum, pivot share, look-ahead steering; the Breaker charges where the men are), CrabTests, ChassisTests, WalkerArmamentTests, BreakerTests, WreckRecordTests, WreckDecayTests (the wreck's stages).

### Factions, rosters and the unit table
- **Files:** `Sim/Core/Faction.cs` (`FactionId`: Iron and Brass, the greybox pair, and four historical armies; a side's
  faction is `SimConfig.FactionA`/`FactionB`), `Sim/Core/FactionRoster.cs` (each faction's ten slots, its pool, and the
  off-map abilities it may call: `AbilityMask`, `MayCall`; `RosterEntry.FillDefault` forwards here),
  `Sim/Core/UnitCatalogue.cs` (the match's unit table; its `Fingerprint` is folded into `Hash()`),
  `Sim/Combat/CombatCatalogue.cs` (the weapon and machine tables), `Sim/Match/UnitDefinitions.cs` (`UnitDef`: a unit
  added from 2026-09-26 on is one entry here, written into every table), `Sim/Core/OrderGroup.cs` (the groups an order
  names, and `Archetypes.Count` 64). The ten a player chose travel as `SimConfig.LoadoutA`/`LoadoutB`.
  `UnitDefinitions.All` holds the Skimmer (19, a hovercraft) and the Salvo (20, a half-track rocket truck), in no
  faction's slots or pool: the Unit Sandbox is the only way onto the field (2026-09-28). Since replay v16 it also holds
  the Proving Ground's sixteen (`UnitDefinitions.ProvingGround`, ids 21-36): the playground's prototypes (Brute, Croaker,
  Hopper, Mercy, Frog) and docs/06's ideas as stand-ins on the specs the sim already has (Sentry, AT rifle, Death
  Battalion, Mark IV, Mark V, A7V, Renault FT, Whippet, Austin, Sapper, Flamethrower), fielded by no faction either; a
  stand-in that has no model of its own is drawn as a shipped one (the SHOW lane's `TankRenderer.Machines` names it). The
  sapper's `InfantrySpec.MineCharges`, the Death Battalion's `NeverPinned` and the flamethrower's `WeaponStats.SetsBurning`
  are data until their SIM commits land (SapperSystem at `SimSystemOrder.Sapper`; DirectFire reading `SetsBurning`).
  `SimConfig.Endless` (the level's rule: `SectorControl` never names a winner) rides the config and the replay header.
  The Salvo's gun fires a rack
  (`TankSpec.Rockets`): `Sim/Combat/TankGunnery.cs` holds each rocket (`PendingRocket`) until its land tick and says
  so in a `RocketFired` event, which `Presentation/Camera/TankRenderer.Salvo.cs` flies on the sim's clock.
  A machine holds at `TankSpec.StandOffMetres` (`TankGunnerySystem.StandOff`); a machine's small arms hunt light
  machines only with `InfantrySpec.HuntsArmour` (`Sim/Combat/TargetAcquisition.cs`, `Sim/Combat/DirectFire.cs`).
- **Tests:** FactionRosterTests, UnitCatalogueTests, UnitDefinitionTests, LoadoutTests, ProvingGroundUnitTests (the
  sixteen: in the table with their numbers, in no roster, a loadout may name them, each machine drives, each armed unit
  fires, the endless match, the same on two worlds), DefinedUnitTests (the
  Skimmer's and Salvo's numbers, that no faction fields them, that each drives and fights, that each rocket bursts on
  the tick and at the point its event named and not before, the same every run and in the canary).
- **See it:** in Play, **TW > Unit Sandbox** (`Editor/UnitSandbox.cs`) spawns any unit type for either side at its
  rally point, 1 to 10 at a time, and tops up both sides' silver: every model and unit, whatever the rosters hold.
  Editor only; it spawns through `TankCapture.Spawn` into both lockstep worlds and changes no roster or faction.
- **Trap:** every faction calls the six abilities of the overhaul; Brass alone drops paratroopers (`decisions.md`,
  2026-09-27). The HUD offers our seat `HudView.SeatMask` (the launch mask cut to `FactionRoster.AbilityMask`), so an
  Iron player has no drop card and its key arms nothing; the legacy IMGUI bar keeps the cell, greyed.
- **Trap:** `VehicleSize` is baked into mesh vertices at load. Every gap authored in the composer depends on it.

### Sea landing
- **Files:** `Sim/Match/SeaLanding.cs`, `Sim/Core/ISeaLift.cs`, drawn by `Presentation/Terrain/LandingCraftView.cs`,
  `Presentation/Terrain/Ocean.cs`, `Presentation/Terrain/Shore.cs`, `Shaders/Sea_URP.shader`.
- **Tests:** LandingTests, CoastTests.

### Adding a unit type (checklist)
This task spans both lanes. The SIM lane lands steps 1-2 as a seam commit first; the SHOW lane then does 3-6.
1. **Where it goes:** each faction fields ten slots (`RosterEntry.SlotCount`, the digit keys 1-0) from a larger
   pool (`Sim/Core/FactionRoster.cs`). A new unit joins a pool, or takes a slot from another unit.
2. Sim: a new archetype id (ids are a seam item; `Archetypes.Count` is 64) and one `UnitDef` in
   `Sim/Match/UnitDefinitions.cs`: its roster line with its `ChassisKind`, its `InfantrySpec`, its weapon, and for a
   machine its `TankSpec` and `VehicleProfile`. The chassis is a field, so a walker may take any id; code that asks
   whether a unit is a tank reads `ChassisKind` through `SimWorld.ChassisOf`, or `RosterEntry.ForArchetype(a).Chassis`
   where no world is at hand.
3. HUD: name, tooltip and portrait in `Presentation/Core/UnitLook.cs` (both HUDs read it; `UI/HudText.cs`
   forwards; `PortraitCount` past the new id), the portrait stem in `UI/Skin/SkinSpec.cs` (`PortraitNames`, then
   `Tools/gen_artspec.py`), the pictures in `UI/Skin/Portraits/` and `UI/Resources/UnitArt/` (a machine's can be cut
   from its render: `Tools/portraitcut.py`), the `.tw-portrait-<Name>` class in `UI/Skin/dustfront.components.uss`,
   and an icon in the IMGUI `BattleHud`. HudTextTests, HudBindTests and UnitArtTests fail until every archetype has them.
4. Art: infantry needs a figure in `Editor/VATBaker.cs` and a bake. Vehicles need a `Resources/Vehicles/<Name>/`
   folder and a `<Name>Atlas.jpg` beside it (`pipelines.md`; `Tools/mechsplit.py TW_BATTLE=1` writes both from a
   playground split) that `Presentation/Camera/TankModel.cs` loads, and one row in `TankRenderer`'s `Machines` table
   (name, archetype, root part, draw scale); a walker's row goes among the first `WalkerRows`. A walker's legs are
   solved by `Presentation/Camera/WalkerGait.cs` from the model, so check it stands and walks (GaitTests). A machine
   defined only in `UnitDefinitions` is unknown to `RosterEntry.ForArchetype`: code with no world at hand that asks
   it (the IMGUI icon check, `RiderLab`) sees an empty id. Otr cannot load an FBX hierarchy: tests that load a model
   (GaitTests, DefinedMachineModelTests) report ENGINE offline and run in the gate.
5. The legacy IMGUI `Presentation/Camera/BattleHud.cs` reads the same `UnitLook` names; its icon array is
   `UnitLook.PortraitCount` long. No test runs OnGUI, so check it in Play with F9.
6. Tests to run: FactionRosterTests, UnitDefinitionTests, CrabTests or TankTests, GaitTests, HudTextTests,
   HudBindTests, UnitArtTests, then the full gate.
   `TankCapture.Spawn` finds the new archetype in the live roster by itself.

## Presentation (SHOW lane)

### Infantry rendering (VAT)
- **Files:** `Presentation/Units/VATRenderer.cs` (LOD tiers, the living; the figure scale is `UnitScale` in
  `Presentation/Core/FigureMetrics.cs`, shared with the picker), `Presentation/Units/VATRenderer.Fallen.cs` (the dead:
  the throw arc, the tumble, the heap per 2 m cell, the charred), `Presentation/Core/IZoomSource.cs`,
  `Presentation/Units/VatCodec.cs`, `Presentation/Units/VatAssetData.cs`, `Presentation/Units/ProceduralSoldier.cs`
  (far tier and fallback), `Presentation/Units/VatTint.cs` (what `VatInstance.Tint` carries),
  `Presentation/Units/FallenFlight.cs` (a gagged body's path: hold, throw, bounces, skid, rest; its turns and squash),
  `Presentation/Units/VATRenderer.Gags.cs` (a gagged body laid down and drawn; the heap stacked by who lands first),
  `Shaders/VAT_URP.shader`, bake: `Editor/VATBaker.cs` + `Editor/InfantryClipTable.cs` (menu TW/VAT/Bake Infantry).
- **Tests:** VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, DeathVarietyTests (VatPad char bits, the tumble's pitch),
  VatTintTests (the Tint layout, C# against the shader), FallenFlightTests (the gagged body's path).
- **Trap:** any clip/discard in `VAT_URP.shader` goes behind `_TW_LIMBCUT` (VatEarlyZTests). Index instance data
  with `GetIndirectInstanceID_Base`, not `GetIndirectInstanceID`. `VatInstance.Tint` is packed by `VatTint`: the team
  in bit 0, a fallen man's tumble pitch in bits 1-5, his roll in 6-10, a squash in 11-16 and a feet pivot in 17 (all
  zero on a living man), and the blood on his uniform in 18-23 (0..63: hp lost, `AnimationController.Wound`; a corpse
  `VatTint.FallenWound`; the shader scales it by `_TWGore`, the GORE slider, which `CombatFx` sets each frame). An
  unwounded living man's Tint is his team. Build it with `VatTint.Pack`, never by hand, and never read `Tint`
  as the team on the C# side. `VatPad` packs limbs, grime, seed and char into 24 bits: nothing more fits.

### Animation (which clip a man plays)
- **Files:** `Presentation/Core/AnimationController.cs` (the per-man priority ladder, run every tick by SimHost),
  `Presentation/Core/AnimationController.Death.cs` (how a man dies: the Death event's cause and knock, density in the
  heap, the death ring `TryDeath` the effects read, `Char`), clip names in `Editor/InfantryClipTable.cs`.
  Design: `docs/15-character-controller.md` (section 9 is the death ladder).
  The absurd deaths (owner, 2026-09-28): `Presentation/Core/DeathGags.cs` chooses a gag on top of the ladder's death
  (punt, jig, headshot pop, rocket, fountain, pancake, fling, wilt, plank, skid, boots), seeded, capped;
  `Presentation/Core/AnimationController.Gags.cs` feeds it and puts the plan in the `DeathRecord`. The knob
  `fx.deathAbsurd` (0 today's deaths exactly, 1 the new look, 2 ludicrous) defaults to 1 (the owner turned it on, 2026-09-30);
  `DeathGags.Pin` overrides it for a test or a capture. What a shell takes off a man at the knob above 0:
  `Presentation/Camera/GibPlan.cs` (pure: the limbs lost, then exactly those parts fly; torn in two or blown apart in a
  heap; kit at GORE 0), thrown by `CombatFx.OwnGibs`. A machine's absurd death (same knob): `Presentation/Camera/TankRenderer.Deaths.cs`
  (the turret leaps and flips, road wheels roll away, a track pays out, the Skimmer's fan glides off, the hull hops, a
  hover machine drops onto its skirt, a walker belly-flops) and `Presentation/Camera/TankRenderer.Fizzers.cs` (a dead
  Salvo's rockets fizz off), on `Presentation/Camera/VehicleGags.cs` (the flights, pure); tests FigurePartsTests (the
  parts cut from the figure), GibPlanTests and VehicleDeathTests.
- **Tests:** BlastReactionTests (knockdown and daze), DeathVarietyTests (the death ladder and its records),
  DeathGagTests (the gags: nothing at 0, one per cause, the trench rule, the caps, no allocation),
  TickAllocationTests (no per-tick allocation).
- **See it:** `Animation.Follow(slot)` then `Animation.TraceText()` through eval, for one man's decisions. The gags in
  Play: `TW.Editor.DeathLab.Absurd(1)`, then `DeathLab.Scene("shot" | "mg" | "wounds" (hit, not killed: the hit blood) | "shell" | "heap" | "gas" | "fire" |
  "beam" | "crush" | "machine" | "maw" | "salvo" | "skimmer" | "walker", x, z)` (`Editor/DeathLab.cs`: every death made
  by the sim's own systems). Unattended, from batch mode: DeathStills (`Tests/Stills`, explicit, run by name with a
  graphics device; `TW_STILLS_DIR` outside the checkout, `TW_DEATH_ABSURD`, `TW_DEATH_SCENES`, `TW_STILLS_FIELD`
  (`Winter` for daylight)) films each scene as a strip and a contact sheet.
- **Trap:** read a dead man through `Animation.TryDeath(slot, eventTick)`, never `State[slot]`: events are
  dispatched once per frame after every tick ran, and the slot may hold another man by then.

### Tanks and walkers drawn
- **Files:** `Presentation/Camera/TankRenderer.cs` (`Machines`: every machine with an atlas of its own, the walkers and
  since 2026-09-28 the Skimmer and the Salvo, `Resources/Vehicles/Skimmer`, `/Salvo`; a row's Rockets draw its shot as a rack of rockets, `Presentation/Camera/TankRenderer.Salvo.cs`; a turret on a machine armed only
  with small arms follows `SimWorld.TargetSlot`; a part called Fan spins; `SideColourOn` names the parts that wear the side's
  colour on a machine without horns), `Presentation/Camera/TankModel.cs` (parts, sockets, leg rigs; `Assemble` joins the legs the Tripo sheets left as loose
  pieces on the machines in `JoinedLegs`, Kettle and Redoubt: a hip onto the body, a knee or ankle onto the end of the piece above),
  `Presentation/Camera/TankRenderer.DriveStyle.cs` (how each machine rides: `StyleFor` per archetype, its spring and
  damping, squat and dive, turn lean, engine note and beat, the running gear's rhythm, a walker's swing, step height
  and footfall thump; a machine with no row rides `PlainStyle`),
  `Presentation/Camera/TankRenderer.Weight.cs` (the weight layer, Dust Front lessons Phase 1, every look behind a knob,
  on by default since the owner's word of 2026-09-29, 0 the old machines: `tank.weight` rides each machine its own style on an acceleration followed from the
  sim's speed and keeps a walker's kicks and footfalls on layers of their own; `tank.recoil` recoil by the shot's weight;
  `tank.shotRock` degrees a six-pounder rocks its hull; `tank.gunHullFlash`; `tank.traverseSettle`; `tank.squat` the
  share of each style's squat drawn against the sim's held acceleration, 0.4),
  `Presentation/Camera/TankRenderer.Probe.cs` (a read-only look at one machine's ride, for `Editor/WeightLab.cs`: traces
  of a stop, a shot or a walk as CSV, and knobs set from eval), `Presentation/Camera/TrackDust.cs` (each track's dust,
  a pivot's too; pure, and nothing calls it until `lane/show/pipe-vfx`, which owns the dust, does),
  `Presentation/Camera/HullRide.cs` (its pure maths: the exact spring solve, the felt acceleration, shot weights, the
  recoil's shape, kick sizes, a walker's kick layer, a turret's settle),
  `Presentation/Camera/MachineSockets.cs` (a walker answers the first of a numbered socket pair with its one socket),
  `Presentation/Camera/TankRenderer.Lights.cs` (a machine's own lights, each behind a knob, on by default (2026-09-29), 0 unlit: `tank.lamps` four running
  lamps on the hull's corners, `tank.lampHue` 0 the side's colour, 1 Dust Front's red, 2 the lanterns' amber (the default), `tank.lampSize`, `tank.lampPull`, `tank.exhaustGlow`
  the exhausts and the Maw's furnace, `tank.lightReach`; glow cards NightLights draws, and with `lights.machinePool` (4) a
  burning machine's, a cook-off's and the furnace's real lights), `Presentation/Camera/MachineLamps.cs` (where the lamps and
  the furnace sit, read off the hull in its own frame), `Presentation/Core/MachineLightSlots.cs` (which machine light
  keeps a slot of the pool: priority, then brightness; the pool is `Presentation/Terrain/NightLights.Machines.cs`),
  `Presentation/Camera/WalkerGait.cs` (planted feet; `SwingScale`, `ArcScale` from the style; `TiltCapPitch`, `TiltCapRoll`), `Shaders/Tank_URP.shader`, `Shaders/TankDisc_URP.shader`,
  import rules `Editor/TankImport.cs` (the walkers' nodes put at `crabs.json`'s places by `Editor/CrabManifest.cs`). Infantry riding the machines (a prototype, presentation only):
  `Presentation/Camera/RiderSeats.cs` (seats read off each hull and their way up), `Presentation/Camera/TankRenderer.Riders.cs`,
  `Presentation/Units/VATRenderer.Extras.cs` (the riders drawn; a seated man is hidden from the normal pass),
  `Editor/RiderLab.cs` (the seat sheet and captures). A wreck breaking in stages (2026-09-28):
  `Presentation/Camera/TankRenderer.WreckStages.cs` (a dead machine's root part drawn as its carcass, chunks coming away
  as the sim's wreck prop loses hit points, the stage changes, shards, cleared heaps sinking, the map's own wrecks adopted
  as husks), `Presentation/Camera/WreckModel.cs` (the root part cut into chunks at load, the chunk in UV4.x),
  `Presentation/Camera/WreckStages.cs` (`WreckStageRules`: which chunks are gone at each stage); `Shaders/Tank_URP.shader`
  collapses a chunk whose bit is set in the instance's `_Chunks`. A round fired at a wreck (`Shot.b` a prop) is drawn by
  `Presentation/Camera/CombatFx.Wrecks.cs` (a tracer to the wreck's side), and the shooter faces it (AnimationController). `Editor/WreckLab.cs` shells a point or reads the wreck
  nearest it, for staging one by hand.
- **Tests:** GaitTests (plus the sim tests above), WreckModelTests (the carcass and the stages' masks), WalkerPivotTests (every walker's and the Cutter's part and socket
  where crabsplit.py built it, at both LODs: `Editor/TankImport.cs` puts them there from `crabs.json`), WalkerJointTests (the joined legs hang together at both LODs), DriveStyleTests (no two machines ride alike), HullRideTests (the weight layer's maths: exact, stable on long frames, no chatter, shots weighed, kicks sized; PlainStyle is the old ride), MachineSocketTests (every walker finds its exhaust and fire), MachineLampTests (four lamps on the upper corners of all ten hulls, front at the front; the Maw's furnace), MachineLightPoolTests (forty burning machines never light more than the pool; a cook-off takes a fire's light, a furnace cannot; the lamps' colour and card size), TrackDustTests (a pivot throws dust where the hull-speed gate threw none), RiderSeatTests, DefinedMachineModelTests (the Skimmer's and Salvo's
  models: both LODs, parts under the Hull, the barrel forward, drawn the size of their footprint). WalkerStills (`Tests/Stills`) captures the walkers
  for the rig scoreboard in `docs/20-rig-scoreboard.md`; WreckStills (`Tests/Stills/WreckStills.cs`) films a Maw killed and its wreck
  shelled through every stage to nothing (a contact sheet, `TW_STILLS_DIR`).
- **See it:** `TW.Editor.TankCapture.Spawn(team, archetype, x, z)`, then read `World.Position[slot]` back: the sim
  moves units to their deploy zone. Freeze with `SimHost.TimeScale = 0` before framing.
- **Measure behaviour in a match:** `Editor/MachineStudy.cs` (`-executeMethod TW.Editor.MachineStudy.CommandLine -twstudy <dir>`,
  `-twstudyseed`, `-twstudysec`): every machine in a real match for 150 s, scored for jams, spinning in place, a hunting
  nose and rubbing hulls, with FLAG lines in its summary file and each jam's place in its jams file. Run it before and after a
  driving change (2026-09-29: integration 368 s jammed and 5 flags, `lane/sim/drive-feel-v20` 2 s and none).
- **Trap:** vehicle meshes must import Read/Write enabled or scaling silently does nothing.
- **Already there, check before adding:** `WalkerGait` `Carry` sets the body's height, pitch and roll from the
  planted feet, and `Step` leads each step by the walker's velocity; `TankRenderer` applies them to the hull.

### Combat effects: tracers, bursts, smoke, camera shake
- **Files:** `Presentation/Camera/CombatFx.cs` (event dispatch `OnSimEvent`, tracers, bodies, bursts, materials),
  `Presentation/Camera/CombatFx.Mines.cs` (our mines and tripwires marked on the ground until they go off or a
  crater cooks them; a trigger's flash; the burst is the Explosion's), `Presentation/Camera/CombatFx.Deaths.cs` (a Death: the body from the controller's record, gibs by density, a burning
  man's pool and smoulder; `UnitAlight` lights and douses the drawn torch), `Presentation/Camera/CombatFx.Gags.cs`
  (a gagged death: the body through `VATRenderer`'s gag overload, a popped helmet, a pancake's squirt, the beam's
  boots, dust at each landing; its blood: a trail of gore along the flight, a splat where each arc lands, a streak
  along a skid, scorch under the boots, in a pool of its own (`MaxGagMarks`), GORE-scaled; a blood card from the
  `BloodSpurt` book when the build has one, looked up by name),
  `Presentation/Camera/CombatFx.HitBlood.cs` (a man hit and still standing bleeds: red droplets out of his far side
  along the round's way, more for a heavier hit, a `BloodSpurt` card when the build has it, and a few drops on the
  ground in the gag marks' pool with a shorter life; GORE-scaled, 0 is none; not behind `fx.deathAbsurd`),
  `Presentation/Camera/CombatFx.Abilities.cs` (the aim's disc or corridor, the strafe's aircraft and tracers, the
  beam's charge and sweep, the scorch, the smoke screen's cards), `Presentation/Core/SimClock.cs` (the sim's clock in seconds for everything the picture times against the sim: the
  aircraft, the beam, the fires), `Presentation/Core/AimShape.cs` (the shape the aim describes and `SceneHooks.AimPreview`, the delegate the aim's owner sets
  so the effects draw it without knowing the panel), `Presentation/Camera/AbilityAim.cs` (the aim
  state machine: point, or press-drag-release with patterns and snapping; `TestPanel` drives it),
  `Presentation/Camera/CombatFx.Chunks.cs` (thrown dirt, splinters, smoke balls, cook-offs),
  `Presentation/Camera/CombatFx.Bodies.cs` (gibs, tree breaks, muzzle and chest positions),
  `Presentation/Camera/CombatFx.Ambient.cs` (birds, ambient smoke), `Presentation/Camera/CameraShake.cs`,
  `Presentation/Camera/FlipbookFx.cs` + `Shaders/Flipbook_URP.shader` (painted flipbooks, textures in `Resources/VFX/`).
  `CombatFx` is one partial class: an event arrives in `CombatFx.cs` and is handed to the part that draws it.
  **Colours:** sixteen books' tints (Splash, Column, Wings, Spurt, Puff, Smoke, and the VFX pass's MortarBurst, ShellFall,
  LeanBurst, ShellLean with Column, ShellPlume, Smoulder, WreckSmoke with Smoke, SmokeBank, GroundRing, DustPuff with Dust) are overwritten every scene by
  `CombatFx.ApplyTints` from
  `BiomeProfile` (`SmokeTint` and the other `*Tint` fields, through `SceneTints`), so change a colour there. At
  night a burst also lights its own smoke: `NightLights` sets `_TWBurst`, read by `TWBurstLight` in
  `Shaders/TWAtmosphere.hlsl`. `FlipbookFx.Book` maps to `Sheets` by position, and three
  sheets are named "Puff": count the rows, the smoke book is the one with `Erode = true`. The VFX pass's 17 books
  (2026-09-28) follow Bloom, one row each, named `<Book>` or `Fire<Book>` (FlipbookOrdinalTests).
  `CombatFx.cs` also draws the called-strike target markers, and `CombatFx.Abilities.cs` the ability aim, from
  `SceneHooks.AimPreview` (whoever owns the aim sets it; `TestPanel` today). The IMGUI banner (`Banner`, `OnGUI`)
  draws only when the legacy HUD is on (F9).
  **The hand grenade** (`CombatFx.Grenades.cs`, 2026-09-29): on `GrenadeThrown` the bomb is drawn each frame on its arc
  from the thrower's hand to where the sim sets it off, a dark lump with its fuse sputtering sparks, timed by the sim's
  clock (`SimNow`) so it comes down with the burst at any speed and holds when paused; its `Explosion`
  (`SourceId.Grenade`) is drawn small (flash, spurt, puff, a few clods, a small kick) instead of as a shell. The
  thrower plays `Clip.Throw` from its swing (`AnimationController`, the `threw` latch).
- **Tests:** BlastReactionTests (camera feels a burst), ComponentLookupAllocationTests, AbilityAimTests (the aim),
  ShotStaggerTests, TracerGlowTests, ColumnLightTests, ColumnPlayTests, GrenadeLookTests (the drawn bomb lands on the
  burst and lobs a man's height).
- **The AOSA look knobs** (`fx.*`, read in `Awake`/`Start` from `Presentation/Core/Knobs.cs`): the night column, soil
  heave and smoke in the shell-burst handler (`fx.columnSoil`, `fx.columnCap`, `fx.columnPlay`, `fx.smokeNight*`; the VFX
  pass's burst recipes, `fx.recipes`, default 0 = the old burst, `CombatFx.Recipes.cs`; the medic's glint behind it, `CombatFx.Support.cs`, the gas and smoke banks, `CombatFx.Banks.cs`), when a
  tick's rifle shots are shown (`Presentation/Core/ShotStagger.cs`, `fx.shotStagger`), and where a tracer is drawn among
  the smoke (`Presentation/Camera/TracerLook.cs`). Every knob's default is the look as it was; the AOSA loop's record of
  what each one did is `docs/reference/aosa/`.
- **Trap:** effects drawn only up close sit behind `if (!close) return;` in `CombatFx.Ground.cs` `CloseLife`. Keep
  that guard in front of anything close-only, or the standard view pays for it.

### Ground marks and footfalls
- **Files:** `Presentation/Camera/CombatFx.Ground.cs` (`AddMark`, `MaxMarks`, `MachineMarkReach`, the marks pool,
  `DrawClose`), `SceneHooks.FootFall` is wired in `CombatFx.cs` `Start`,
  `Shaders/GroundMark_URP.shader` (shape 0 boot, 1 rut, 2 walker pad, 3 blood, 4 scorch: the last two drawn by
  `CombatFx.Gags` from a pool of their own, and keep their colour on snow; snow vs mud handled in the shader),
  `Presentation/Camera/TankRenderer.cs` (calls `FootFall` where a walker's foot lands).
- **Tests:** none.
- **See it:** a top-down capture in Play. A paused editor draws no marks, so do not diff paused frames.

### Flamethrower
- **Files:** `Presentation/Camera/Flamethrower.cs` (presentation only, driven by the debug panel's flame tools; the
  sim's flamethrower unit, archetype 36, burns men for real since 2026-09-28, see "Fire in the sim", and is not yet
  wired to this picture: route a `Shot` from a `SetsBurning` weapon to `Jet`), `Shaders/Flame_URP.shader`, books cut by `Tools/firebooks.py`. Pools, pyres and the torch on a burning
  man expire by the sim's clock (`BornSim`/`LifeSim`, `Flamethrower.SimNow` from `Presentation/Core/SimClock.cs`), so a
  paused match keeps its fires; their flicker and cards still animate on `Time.time`. A man's death douses his torch
  (`flames.Douse` in `CombatFx.Deaths.cs`), so the slot's next tenant is not drawn alight.
- **Tests:** none.
- **See it:** `Tools/flameshots <prefix>` captures the four reference shots deterministically, and
  `Tools/flamecheck.py` rejects a shot with no fire in it.
- **Trap:** `FlipbookFx` indexes its sheets by the `Book` enum's ordinal and is only `Ready` when every sheet loads.
  A missing PNG or a row out of order silently disables every flipbook in the game. Add the enum entry, the row and
  the PNG together, and check `books.Ready`.

### Debris and destruction of props and houses
- **Files:** `Presentation/Camera/DebrisRenderer.cs` + `Shaders/Debris_URP.shader` (GPU-flown pieces, ring buffers,
  `ZoomShare`; a pool whose last piece has sunk is not drawn), `Presentation/Camera/DebrisRenderer.Parts.cs` (a man's
  parts as pieces, life-size: stand-ins built in code), `Presentation/Camera/DebrisRenderer.Figure.cs` (the same parts
  cut at load from the baked soldier by limb, the cloth mask in vertex alpha: only the uniform takes the side's tint),
  `Presentation/Terrain/PropDestruction.cs` and `Presentation/Terrain/PropWear.cs` (one partial class,
  with `Presentation/Terrain/PropDestruction.Ram.cs`: `props.ram`, on by default (2026-09-29), a heavy machine on the move wears
  what its footprint covers until its ground-storey house chunks and heavy kit props break, once a tick beside `Crush`;
  and `LampOut`, a lantern that goes by any cause puts its lamp out through `SceneHooks.LampOut`),
  `Presentation/Terrain/TrenchSection.cs` (`TrenchSectionRules`: a lining section intact -> damaged -> gone, heavy
  ordnance, the pieces' life), `Presentation/Terrain/HouseKit.cs` (chunked buildings, `MaxChunks`, `ChunkMask`).
  Design: `docs/16-destruction.md` ("The lining breaks in two steps").
- **How harm works:** `PropDestruction.Strike` applies each explosion to the props in reach, per `Rule` (hp,
  pieces, dust; built in `BuildRules`, one per kit list such as `kit.TrenchWalls`). A prop with hp left is only
  chipped (`Chip`: flying bits, no change of look); at zero it is `Finish`ed (`Collapse`/removed, debris thrown).
  Trench lining is the exception: `TrenchSection` gives it a damaged state between. A shelter (`Rule.Shelter`:
  dugouts, bunkers, MG nests) is never finished: `ShedBags` throws sandbags off it. Budgets: `MaxLoose`,
  `MaxRemembered`, `PropDestruction.SpallBudget` (declared in `PropWear.cs`, a part of the same class), and the
  `DebrisRenderer` rings.
- **Tests:** DebrisTests, PropWearTests, HouseKitTests, TrenchSectionTests.
- **Trap:** a prop is identified by its position rounded to 0.25 m. Convert a drawn instance to a key through
  `PropWear.Home` first, or it forgets its damage. A lining panel and its broken twin share the panel's key
  (`KeyOf`): key a twin by itself and it becomes a second, undamaged thing. The twin is put where the panel stood by
  `BattlefieldProps.AddInstance` and kept there by `Replace`; the composer keeps emitting the whole panel.

### Battlefield props and composition
- **Files:** `Presentation/Terrain/BattlefieldComposer.cs` (seeded placement: the steps, the sites, the map's props; one
  class in six files), `Presentation/Terrain/BattlefieldComposer.Trench.cs` (the lining),
  `Presentation/Terrain/BattlefieldComposer.Ground.cs` (debris, graves, gatherings, margins, the winter ground),
  `Presentation/Terrain/BattlefieldComposer.Landmarks.cs` (landmarks, horizon),
  `Presentation/Terrain/BattlefieldComposer.Buildings.cs` (village, rear), `Presentation/Terrain/BattlefieldComposer.Scatter.cs`
  (the scatter step: builds the rules' input from the map and the layout, emits the placements),
  `Presentation/Terrain/BattlefieldKit.cs` (modules, env atlas cells) + `Presentation/Terrain/BattlefieldKit.Keys.cs`
  (module keys and scale rules), `Presentation/Terrain/BattlefieldProps.cs` (instanced pages, `Generation`, `Enforce`),
  `Presentation/Terrain/BattlefieldBlueprint.cs`, `Presentation/Terrain/PropLayout.cs` (`Style`) + `Resources/Layouts/`
  (owner's hand edits), `Editor/EnvPropEditor.cs` (with `Presentation/Terrain/PropHandle.cs`, the editor stand-in for
  one batched prop), `Editor/EnvKitImport.cs`. The procedural kit: `Presentation/Terrain/BattlefieldGeometry.cs` (worn
  solid primitives), `Presentation/Terrain/BattlefieldPigment.cs` (the painted surface sheet), and
  `Presentation/Terrain/BattlefieldBackdrop.cs` (what lies beyond the fought-over ground).
- **Scale (docs/21 phase 1):** `Presentation/Terrain/AssetScaleTable.cs` (the soldier unit and every module's class and
  bounds), `Presentation/Terrain/AssetScaleReport.cs` (measures the composed field), `Editor/AssetScaleAudit.cs`
  (writes the report: menu TW/Audit/Asset Scale or `-executeMethod TW.Editor.AssetScaleAudit.Run`), `Tools/looks.py`
  (rescales the looks in the layout asset).
- **Scatter (docs/21 phase 2):** `Presentation/Terrain/ScatterField.cs` (`ScatterInput`: cells, banks, what stands up,
  footprints, ladders, roads, season, coast, rear bands; `ScatterField`: Traffic, Vertical, Patch, Wet, Open) and
  `Presentation/Terrain/ScatterLayers.cs` (grass, accents, flowers, frost, the camp's kit in trenches, dugouts and the
  rear; the caps). Design: `docs/13-environment-system-rebuild.md`, "Rule-based scatter".
- **Tests:** EnvAtlasTests, AssetScaleTests, ScatterRulesTests.
- **Trap:** `BattlefieldKit.EnvSets` / `EnvCols` / `EnvRows` must match `Tools/envatlas.py`. Walkers need ~10 m
  gaps between placed structures. The scale clamp lives in `BattlefieldProps.Emit` and `Placement`, not in
  `Styled` (which returns early without a look, and hand edits never pass through it); a building is reported,
  never clamped, because its chunks are placed by their own matrices. A new public `Module` field on the kit needs a
  row in `AssetScaleTable` (AssetScaleTests keys every field). The scatter's placements are computed once a map and
  re-emitted every pass: put nothing in `ScatterLayers.Place` that reads the surface (a crater), that filter is the
  emit loop's in `BattlefieldComposer.Scatter.cs`.

### Terrain view, weather, night, biomes
- **Files:** `Presentation/Terrain/GreyboxTerrainView.cs` (ground mesh, adds most environment components; owns the
  crater colour texture `colorTex` and uploads it in `Update` whenever a crater painted it),
  `Presentation/Terrain/BattlefieldSurface.cs` (surface kinds; `RefreshHollows` finds where water pools in craters,
  about 11 ms, whole-map and order-dependent, marker `TW.Terrain.Hollows`, run under `HeavyWork`; no test covers it),
  `Presentation/Terrain/Atmosphere.cs`, `Presentation/Terrain/NightLights.cs` (with `NightLights.Machines.cs`: the
  machines' glow cards after the flash pool's, `lights.maxMachineGlows`, and their own light pool, `lights.machinePool`),
  `Presentation/Terrain/Rain.cs`, `Presentation/Terrain/Storm.cs`, `Presentation/Terrain/QuietFog.cs`,
  `Presentation/Terrain/FogWisps.cs`, `Presentation/Terrain/SmallLife.cs`, `Presentation/Terrain/WaterRings.cs`,
  `Presentation/Terrain/BiomeProfile.cs`, `Presentation/Core/RenderGround.cs` (shared ground height, `SceneTints`),
  shaders `Toon_URP`, `Water_URP`, `TWAtmosphere.hlsl`, `TWLocalLights.hlsl`, `TWWater.hlsl`.
  The owner's night look (2026-09-29; its `decisions.md` row rides on `lane/show/vehicle-weight`), every part behind a knob, on by default since the owner's word, 0 the old night:
  `Presentation/Terrain/Atmosphere.NightLook.cs` (`look.lift` the distance into lighter, greyer blue haze: the fog, the
  bank and the clear colour lift and the fog closes within `look.liftReach` x the distance looked at, while the low mist
  and the bank lift by only `look.liftMist` / `look.liftBank` of it, since both lie over the near ground too; `look.wet`
  less pale sky in the puddles and the flames' glints on wet mud), `Presentation/Terrain/NightLights.Pools.cs`
  (`look.pools`: each camera hands its nearest 32 flames to the Toon shader as painted light pools, past the eight real
  lights an object may take; `look.poolReach`; `look.hazeLight` the lifted haze's lightness, `look.puddleSky` / `look.waterDim` still water darker at night (the mirrored sky, `Water_URP`'s lit body), `look.grade` the night grade's umber shadows (`look.gradeSat`, `look.gradeDark`), `look.inkFade` the ink lines fading with the distance fog (`InkLines_URP`); `look.moonSheen` (the fine wet sparkle's share at the play view), `look.poolAmber` / `look.poolVary` (lamp pools deeper amber, each lamp's reach varied), `look.poolUnblue` (a pool's faint edge takes blue out of the moonlit ground so it reads amber, not violet), `look.propRim` (props, not the ground, take the men's warm pool rim in Toon), `look.glowHue` / `look.glowFade` (a far glow keeps its colour in the haze and fades as it thickens, `Glow_URP`), `look.moreFires` (burning trees in no man's land with flames, glow and a painted pool but no real light, read at Start), `look.rainCurtain` (the distant rain curtains' alpha, their colour 1.15 x the lifted haze), `look.horizonFires` (the horizon's fire glows faint, with a small orange core) and `look.warmFlare` (the star shell's glow small and warm, its light neutral), both read at Start; `look.poolSoft` / `look.poolShoulder` the bands toward a soft falloff and a roll-off for bright sums; `look.poolsThroughHaze` the pools' share added after the fog so far lamps shine through the haze; `look.firePools` the fires that come and go, through
  `SceneHooks.FirePool`: a burning machine or wreck from `TankRenderer.Burning`, a flamethrower's fires from
  `SceneHooks.FireLight`; the men (`VAT_URP`) and machines (`Tank_URP`) take the pools too, with a warm rim on the edge
  turned to a fire, `TWPoolsOnFigure`), shader include `TWLightPools.hlsl`; `Editor/LookLab.cs` sets knobs
  from `unity command eval` for before-and-after stills.
  Rare comic sound words over the field (the owner kept them, rare): `UI/Selection/ComicWords.cs` (CRACK on a near
  miss, CLANG on a shell a hull stopped, KRUMP on a burst of 6 m or more, KA-BOOM on a cook-off; one word at most each
  `fx.comicGap` s (8), the weightiest since the last, two on screen at most, only where the camera sees it; `fx.comicWords`
  0 off; built and ticked by `SelectionController` beside DeathMarks, styled `.hud-comic` in `BattleHud.uss`). Tests:
  ComicWordsTests.
- **Tests:** NightLookTests, BiomeProfileTests, PaintedHorizonCompressionTests, WinterLevelTests, ScorchTilePainterTests (a crater
  repaints only its own tile of the ground colour, `Presentation/Terrain/ScorchTilePainter.cs`), HollowRescanTests and
  DrainageTests (crater hollows and rill drainage, presentation only).
- **Trap:** post-processing only runs because `Settings/TW-Renderer.asset` references URP's `PostProcessData`; with
  it null the whole grade silently does nothing while the volume stack still reports its values.
- **Trap:** `SimHost.Ground` picks the terrain but not the look. For a winter test set the biome look too
  (`feature-flags.md`). `BiomeProfile.ForGround` pairs them: `WinterLine` is the only daylight look (overcast snow);
  every other ground is night.

### Render pipeline and quality settings
- **Files:** `Settings/TW-URP.asset` is the pipeline in effect at every quality level (shadow distance 220 m,
  cascades), with `Settings/TW-Renderer.asset`. `ProjectSettings/QualitySettings.asset` also lists shadow
  distances (15 to 150): URP ignores them. `Editor/BootstrapSceneBuilder.cs` rebuilds the pipeline asset with its
  own copy of 220. No runtime code sets pipeline values today; a runtime knob belongs in `Atmosphere.cs`, which owns
  the per-scene look. `Editor/InkLinesSetup.cs` installs the screen-space ink pass on the renderer.
- **Trap:** changing a pipeline asset's value from code in the editor writes the asset to disk. Restore it, or
  change a copy.
- **Tests:** none.

### Camera
- **Files:** `Presentation/Camera/TacticalCamera.cs` (standard view: fov 25, pitch 25, zoom 30; `FrameFrom`),
  `Presentation/Camera/CameraShake.cs`, `Editor/GameViewFit.cs`. Where the view meets the ground (the shake, ambient
  kicks, storm bolts, fog and focus distance): `Presentation/Core/ViewGround.cs`.
- **Tests:** ViewGroundTests (also fails when a presentation file writes the formula out again; Rain and NightLights
  still have their own copy until their lanes land).
- **Trap:** setting `Camera.main.transform` does nothing; the controller overwrites it every frame. Use `FrameFrom`.

## Interface (SHOW lane)

### Battle HUD (UI Toolkit, the live one)
- **Files:** `UI/HudController.cs`, `UI/HudView.cs`, `UI/HudText.cs` (every word), `UI/HudLayout.cs`,
  `UI/HudMinimap.cs`, `UI/TrenchOrderCluster.cs`, `UI/HudBootstrap.cs`, `UI/Resources/Hud/BattleHud.uxml`,
  `UI/HudHotkeys.cs` (keys, through `KeyMap`), `UI/HudTooltip.cs`, `UI/HudDialogue.cs` (the speaker strip) and
  `UI/HudCommentary.cs` (what is said on it), `UI/ObjectiveTracker.cs`; `Presentation/Core/HudBridge.cs` is the
  seam between the camera assembly and UI (the camera may not reference UI). A unit's name, tip and portrait, and the
  words of the barrage, gas and drop cards, are `Presentation/Core/UnitLook.cs`, keyed by archetype, so both HUDs say
  the same.
- **Tests:** HudBindTests, HudStructureTests, HudLayoutPlayTests (PlayMode: the bar fits, `HudLayout.BarWidth`),
  UnitArtTests. `HudText` strings are tested in HudBindTests. HudTextTests checks that the unit names and tooltips
  match the sim's numbers. HudLayoutTests, despite its name, tests the legacy `BattleHud`.
- **Centre banner** (a trench taken, an ability, the match end): `ObjectiveTracker.BannerFor` (`UI/ObjectiveTracker.cs`,
  pure, tested in HudTextTests) picks the words and rank, `Presentation/Core/BannerRules.cs` decides which banner
  replaces which. `CombatFx.cs` draws an IMGUI copy only while the legacy HUD is on (F9).
- **See it:** `TW.Editor.HudCapture.Shoot(path)`. The ordinary capture paths do not include the HUD.
- **Trap:** the support cards are `HudView.SupportAbilities` (seven: HE, chlorine, paratroopers, creeping barrage,
  smoke screen, strafe run, beam); `HudLayout.SupportSlots` counts them, but the legacy `BattleHud` keeps
  `SupportSlots = UnitLook.SupportCards` (the first three; the line abilities have cards only in the Toolkit HUD).

### Legacy IMGUI HUD and debug panel
- **Files:** `Presentation/Camera/BattleHud.cs` (F9 switches to it), `Presentation/Camera/TestPanel.cs`,
  `Presentation/Camera/DebugOverlay.cs`.
- **Tests:** HudLayoutTests, HudTextTests test its static helpers only. No test runs OnGUI.
- **See it:** in Play press F9 for the legacy HUD; the "Debug panel" button is top right. `Tools/tw shot` captures
  the game view with its IMGUI.

### Support fire: arming, aiming, calling it in
- **Files:** arming an ability is checked in three places that must agree: `TestPanel.Arm` / `Armed` (the debug
  panel, and the one the aiming circle reads), `UI/HudController.cs` `ToggleArm`, legacy `BattleHud.SupportSlot`.
  The range readout is `UI/Selection/AimReadout.cs` (`Radii`: hit and reach), the cursor `UI/Selection/SelectCursor.cs`,
  the circle and target markers `Presentation/Camera/CombatFx.cs` `Update`. Sim side (SIM lane):
  `Sim/Match/OffMapAbilities.cs` (`TryGetStats`: which abilities exist and their radius).
- **Tests:** SelectionTests (AimReadout), SupportAbilityTests (sim).
- **Trap:** an ability without a radius is aimed with `AbilityAim.PointFallbackRadius` (8 m); `AimReadout.GasReticleM`
  reads the same constant, and a line ability is counted along its corridor (`ShowLine`).
- **See it:** in Play, arm with the HUD card or a key (F5 F6 F7: HE, gas, paratroopers; C M V B: creeping barrage,
  smoke, strafe, beam), then `TW.Editor.HudCapture.Shoot(path)`.
- **Adding a support ability (checklist).** SIM first: `OffMapAbilityId` and its stats in
  `Sim/Match/OffMapAbilities.cs`, the asset in `Editor/SliceDefinitions.cs`, the factions that may call it
  (`FactionRoster.AbilityMask`). Then SHOW: `UI/HudView.cs`
  `SupportAbilities`; the slot counts in `UI/HudLayout.cs` and `Presentation/Camera/BattleHud.cs`; a `GameAction`,
  key and label in `Presentation/Core/KeyMap.cs`; `UI/HudHotkeys.cs`; name and card text in `UI/HudText.cs`; its icon
  in `UI/Skin/SkinSpec.cs`; `Presentation/Camera/TestPanel.cs`; the aim circle and effects in `CombatFx.cs`; the AI's
  choice in `Presentation/Core/ScriptedEnemy.cs`. Tests: SupportAbilityTests, HudBindTests, HudStructureTests,
  KeyMapTests, SkinAssetTests.

### Selection
- **Files:** `UI/Selection/SelectionController.cs`, `UI/Selection/SelectionModel.cs`, `UI/Selection/UnitPicker.cs`
  (pick radius), `UI/Selection/SelectionMarkers.cs`, `UI/Selection/HoverCard.cs`, `UI/Selection/SelectionPanel.cs`,
  `UI/Selection/UnitStatus.cs`, `UI/Selection/GarrisonStats.cs`, `UI/Selection/TrenchScope.cs` (the selection as
  "these troop categories of trench t", which the trench's over-the-top sends), `UI/Selection/GroupAlerts.cs` (a control
  group chip flashes HIT, holds PINNED, greys LOST), `UI/Selection/DeathMarks.cs` (a skull where a man dies, counted
  when deaths bunch); aiming support fire is its own row above.
- **Tests:** SelectionTests (also the pure rules of TrenchScope and GroupAlerts).

### Menus, settings, keys, match launch
- **Files:** `UI/Shell/ShellRouter.cs`, `UI/Shell/ShellBoot.cs` (keeps the shell alive through scene loads),
  `UI/Shell/ShellScreen.cs` (one screen: a UXML bound to the router), `UI/Shell/ShellAssets.cs` (built by
  `Editor/UI/ShellAssetsBuilder.cs`), the screens `UI/Shell/MainMenuScreen.cs`, `UI/Shell/MissionSelectScreen.cs`
  (with `UI/Shell/MissionCatalog.cs`, `UI/Shell/MapThumbnail.cs`), `UI/Shell/ArmouryScreen.cs`,
  `UI/Shell/SettingsScreen.cs`, `UI/Shell/PauseMenuScreen.cs`, `UI/Shell/DebriefScreen.cs`; `UI/Shell/SettingsApplier.cs`,
  `Presentation/Core/AudioLevels.cs` (volume buses), `Presentation/Core/InputFocus.cs` (who owns the keyboard),
  `Presentation/Core/GameSettings.cs`, `Presentation/Core/SettingsStore.cs`, `Presentation/Core/KeyMap.cs`,
  `Presentation/Core/MatchLaunch.cs` (the `Request` a mission starts from; `UI/Shell/MissionCard.cs` `ToRequest` is
  the one place the game builds one), `Presentation/Core/MatchClock.cs` (owns `SimHost.TimeScale`). A side's faction
  and its chosen ten go from `MatchLaunch.Request` (`FactionA`/`FactionB`, `LoadoutA`/`LoadoutB`) through `SimHost`
  into the sim's `SimConfig`. Unlocks are data in `Data/Definitions.cs` (SIM lane), not yet read at runtime.
- **Tests:** ShellUxmlTests, ShellRouterPlayTests, GameSettingsTests, KeyMapTests, MatchClockTests, MatchLaunchPlayTests,
  LaunchLoadoutTests.
- **Settings sliders:** each slider's range is a row in `GameSettings.Sliders`; loading clamps to it and
  `SettingsScreen` sets the slider from it. A new slider needs a row, or GameSettingsTests fails.

### Campaign shell
- **Files:** `UI/Campaign/HomeFrontScreen.cs`, `UI/Campaign/StrategicMapScreen.cs`, `UI/Campaign/StagingScreen.cs`
  (the three campaign screens; their UXML is hand-written under `UI/Resources/Shell/`, loaded by name like the
  Armoury), `UI/Campaign/ShellPick.cs` (is the mouse over a plate, for the 3D views' picking),
  `UI/Campaign/CampaignGraph.cs` (the country nodes, their missions in order, prerequisites and gold: a
  code table, no asset), `Data/FactionMap.cs` (SIM lane: which of the sim's factions a campaign nation fields), `UI/Campaign/FactionBuildings.cs` (the Home Front's buildings, their stages and upgrade
  lines per faction, priced and capped against the profile), `Presentation/Core/CampaignProfile.cs` +
  `ProfileStore.cs` (profile.json beside settings.json, versioned, written through a .tmp swap),
  `Presentation/Core/CampaignSession.cs` (the mission in flight across the scene load),
  `Presentation/Core/MetaViews.cs` (the seam to the 3D views: `IHomeFrontView`, `IStrategicMapView`,
  `MetaServices`), `Presentation/Meta/` (its own assembly, `TW.Presentation.Meta`: `HomeFrontDiorama.cs` and
  `HomeFrontStages.cs` (sliced houses shown to a stage by chunk mask), `StrategicMapView.cs` and
  `ContinentMesh.cs` (the generated continent, pins, the front line, the fog sheet), `MapFog.cs` (the fog mask:
  clear round every reachable node, dense elsewhere), `MetaCamera.cs` (orbit / map),
  `MetaMeshes.cs`, `MetaBoot.cs` (installs the view factories at start-up)).
- **Tests:** CampaignGraphTests, CampaignProfileTests, FactionBuildingsTests, HomeFrontDioramaTests,
  StrategicMapMeshTests.
- **Trap:** `CampaignSession` must not register with `SceneStatics` (the router resets those on every scene load,
  which is exactly when the session has to survive); `MatchLaunch.QuitToMenu` clears it. Unit tiers, armour
  plate and the ability mask are stored in the profile but reach the sim only once the upgrade seam lands
  (docs/21 B1); today only the depot's silver and income go into the launch request, and the HUD fields only the
  cards the request's `AbilityMaskA` allows (`HudView.Offered`: cards and keys; the sim itself does not refuse a
  masked ability yet). The debrief pays a
  campaign win through `ProfileStore.Current`: a test that binds it sets `ProfileStore.Persist = false` and
  `ProfileStore.Use(profile)` first, or it writes the player's profile.json.

### UI skin
- **Files:** `UI/Skin/SkinSpec.cs` (the sprite table), `Editor/UI/UiSkinGenerator.cs`, `Editor/UI/UiSkinImport.cs`,
  `Editor/UI/UiSkinVerifier.cs`, `Editor/UI/UiAssetBuilder.cs`. The game's logo: `UI/Shell/GameLogo.cs` (its layers in
  `Resources/Logo`, animated or flat wherever a screen marks a `tw-logo`), import rules `Editor/UI/LogoImport.cs`. `docs/17-ui-art-spec.md` is generated by `Tools/gen_artspec.py`.
- **Tests:** SkinAssetTests.
- **Trap:** a USS imported before the PNG or font it names keeps "Invalid asset path" warnings until
  reimported (menu TW/UI/Reimport Skin Sheets). `-unity-slice-scale` needs a unit (`1px`). Unit portraits are USS
  classes `.tw-portrait-<Name>`, not Resources.

### Asset playground (a test range, never shipped)
- **Files:** `Playground/Playground.unity` with `Playground/Runtime/PlaygroundHost.cs` (the scene it builds at Play,
  its panel and command strings), `Playground/Runtime/PlaygroundLibrary.cs` (the asset list it spawns from),
  `Playground/Runtime/PlaygroundCamera.cs`, `Playground/Runtime/PlaygroundFx.cs` (the game's FlipbookFx and
  DebrisRenderer), `Playground/Runtime/VehicleRig.cs` (a destructible vehicle, the same parts at every LOD) with
  `Playground/Runtime/VehicleManifest.cs` (`tank3.json`), `Playground/Runtime/FlyerDrive.cs`,
  `Playground/Runtime/WalkerDrive.cs` (on the game's gait), `Playground/Runtime/UnitRig.cs` and
  `Playground/Runtime/Retarget.cs` (figures on the game's clips at other proportions), `Playground/Runtime/BuildingRig.cs`
  (a kit house brought down chunk by chunk), `Playground/Runtime/Tumble.cs` (loose pieces), `Playground/Runtime/LodPicker.cs`
  and `Playground/Runtime/LodTint.cs` (LOD choice and colour match), `Playground/Editor/PlaygroundSetup.cs` (menu
  TW/Playground/Build) and `Playground/Editor/PlaygroundImport.cs` (import rules for `Playground/Art`). Design:
  `docs/22-asset-playground.md`; driving it and its stills: `pipelines.md`, "Asset playground".
- **Tests:** PlaygroundAssetTests (parts and rigs per LOD, the LOD-identical breakup).
- **Trap:** nothing here is in the build settings, and no battle code references it: an asset proven here still has
  to be moved into `Resources/` and the battle's renderers.

## Tooling

### Statics that outlive a match
- **Files:** `Presentation/Core/SceneStatics.cs` (`Reset` per scene load; `ResetSession` and `Register` when Play
  ends), `Editor/PlayModeStaticsReset.cs` (calls it on EnteredEditMode), `SceneHooks.Reset` in `Presentation/Core/RenderGround.cs`.
- **Tests:** StaticLifecycleTests (a new mutable static in presentation or UI fails until it registers a reset or
  is explained there), SceneStaticsTests (what a scene load keeps).
- **Trap:** never clear `SceneHooks` or `HudBridge`'s `PointerOverUi` / `WheelClaimed` from `SceneStatics.Reset`:
  it runs after the new scene has wired its hooks, and `HudBootstrap` may already have built the HUD.

### Fresh-clone setup
- **Files:** `Editor/BootstrapSceneBuilder.cs` (`SetupAll` runs only the steps whose output is missing; its menu items
  ask before replacing the committed URP asset or scenes).
- **Tests:** FreshCloneSetupTests (on this repo SetupAll would replace nothing).

### Performance and allocations
- **Files:** `Perf/PerfBench.cs`, `Perf/BenchOptions.cs` (every bench option is parsed in `Parse`: the list of
  keys), `Perf/BenchScenarios.cs` (`scenario=`: what the window stages on top of the stress battle),
  `Perf/HitchAttribution.cs` (which TW marker carried a hitch), `Perf/AllocProbe.cs`, `Presentation/Core/HeavyWork.cs`,
  `FrameBudget` in `Presentation/Core/RenderGround.cs`, `Presentation/Core/Knobs.cs` (run-time knobs: `knobs=`,
  `-twknob`, `TW_KNOBS`; a report lists every knob it read), `Presentation/Core/ShotLog.cs` (the per-shot log of an image
  run). Budgets and past runs: `docs/05-performance-budgets.md`; the AOSA loop's runs: `docs/reference/aosa/`.
- **Tests:** AllocProbeSanityTests, TickAllocationTests, ComponentLookupAllocationTests, VatAtlasMemoryTests,
  BenchOptionsTests, FrameBudgetCoverageTests (every gameplay draw goes through `FrameBudget`), HitchAttributionTests,
  KnobsTests, ShotLogTests. An unknown option is listed in the report's `warnings` (`BenchOptions.Unknown`), not
  refused, so read the warnings before trusting a run.
- **Trap:** the report carries the frame budget twice, as `frame_draw_calls`/`frame_vertices`/`frame_indirect_draws`
  (the overhaul's names) and `frame_budget_*` (the AOSA loop's); both read `FrameBudget` on the same frame.
- **Trap:** `GC.GetAllocatedBytesForCurrentThread` reads 0 in Unity. Count allocations with `AllocProbe`.
- **Trap:** a bench `shot=` includes the HUD. `shot_tick=N shot_hud=0` hides it (and `CombatFx`'s world overlays,
  `CombatFx.ShowOverlays`) and holds the clock so the still repeats; such a run's timings are not real time.

### The gym: every clip, unit, ability, death and event, one at a time, in the battle's drawing
- **Files:**
  - `Perf/GymCatalogue.cs`: the entries, read from the enums and the unit table. `EventHow` has one row per
    `SimEventType` and no default.
  - `Perf/GymDirector.cs`: staging through `SimHost.WriteWorlds` / `Issue` / `IssuePeer` only. Editor and development
    builds only.
  - `Presentation/Core/AnimationController.Pin.cs`: hold one man on one clip, presentation only.
  - `Editor/Gym.cs`: **TW > Gym**, `Gym.Run`, `Gym.CommandLine`.

  The agents' guide to it is the `tw-gym` skill (on `lane/show/pipe-skills2` until that lands).
- **Tests:**
  - GymCatalogueTests: a new event, ability or death kind with no gym row fails.
  - AnimationPinTests: the pin holds, replays a one-shot, lets go on a new generation, and loses to a death.
  - GymPlayTests: a pinned clip is drawn, and a barrage called through the gym is accepted.
- **See it:** in Play in GreyboxCorridor, **TW > Gym** (Quiet, a tab, Play, a band button). Unattended:
  `Tools/tw eval 'return TW.Editor.Gym.Run("tabs=clips max=20");'`, or `-executeMethod TW.Editor.Gym.CommandLine -twgym
  "<options>"`.
  - The run goes to `%LOCALAPPDATA%\TrenchWarfare\gym\<run>` (`TW_GYM`), never into the checkout.
  - Each entry gets a JPG of the bands and a JSON sidecar, and the run's summary file lists every flag.
- **Trap:** a death or effect raised inside `WriteWorlds` never reaches the picture, because `SimWorld.Step` clears
  events first. The gym stages the cause and lets a system do the killing inside a tick.
- **Trap:** `desync` is `null` in a run without the canary. Only a canary run can see one.

### The gate, the Long tier and the tools' own check
- **Files:** `gate.ps1` (what a run before a commit leaves out, the 300 s budget), Tools/gate_scope.py (which test
  modules a lane's changes reach, and whether it changes a tool), Tools/toolcheck.py (validate, selftest and every
  tool's tests, as one check), Tools/land.py (what a lane must pass to land).
- **Tests:** `python Tools/toolcheck.py`, no Unity. The gate's and `land.py`'s own rules are cases in
  Tools/selftest.py (`gate_cases` with a stand-in for Unity, `land_cases` on a throwaway repo).
- **How:** a test over 3 s gets `[Test, Category("Long")]`; a new tool's tests go in a `test_<tool>.py` beside it and
  are found by name. `workflow.md`, sections 1 and 5.

### Windows build
- **Files:** `Editor/BuildWindows.cs`, `Resources/ShaderKeep/`.
- **Tests:** ShaderInclusionTests (every `Shader.Find("TW/...")` must be in Always Included Shaders or used by a
  material under `Resources/ShaderKeep/`, or it breaks the player).

## Generated indexes (do not edit: `python Tools/codemap.py`)

Which component sets, reads or calls each `SceneHooks` member (the hand rows above no longer repeat this):

<!-- gen:hooks -->
| SceneHooks member | Set by | Read / called by |
|---|---|---|
| `CloseUp` | CaptureRig, PerfBench, TacticalCamera | Atmosphere, BattlefieldProps, CombatFx, CombatFx.Chunks, CombatFx.Grenades, CombatFx.Ground, NightLights, PropDestruction, SmallLife |
| `IsWater` | WaterRings | CombatFx, CombatFx.Ambient, CombatFx.Chunks, CombatFx.Ground, CombatFx.Mines, NightLights, TankRenderer |
| `AddRing` | WaterRings | CombatFx, CombatFx.Chunks, LandingCraftView, TankRenderer |
| `Sparks` | CombatFx | CombatFx.Abilities, Flamethrower, NightLights, TankRenderer, TankRenderer.Fizzers |
| `AimPreview` | TestPanel | CombatFx.Abilities |
| `SmokeSources` | NightLights | CombatFx.Ambient |
| `TanksDrawn` | TankRenderer | CombatFx, CombatFx.Ground, VATRenderer |
| `VehicleTracks` | TankRenderer | CombatFx.Ground |
| `VehicleGunPort` | TankRenderer | CombatFx.Bodies |
| `DrawnWreck` | TankRenderer | BattlefieldComposer |
| `IsTankSlot` | TankRenderer | CombatFx, CombatFx.Deaths, CombatFx.Ground |
| `Flash` | NightLights | CombatFx.Chunks, TankRenderer, TankRenderer.Riders, TankRenderer.Salvo |
| `FireLight` | NightLights | Flamethrower |
| `FirePool` | NightLights.Pools | NightLights, TankRenderer |
| `CookOff` | CombatFx | PropDestruction |
| `FootFall` | CombatFx | TankRenderer |
| `MachineLight` | NightLights, NightLights.Machines | TankRenderer.Lights |
| `LampOut` | NightLights, NightLights.Machines | PropDestruction.Ram |
| `Standing` | BattlefieldProps | TankRenderer.Deaths |
<!-- /gen:hooks -->

<!-- gen:tests -->
- **Match:** AssaultLadderTests, BalanceSweepTests, BehaviourBenchTests, DefinedUnitTests, LaunchLoadoutTests, MatchLoopTests, SinglePlayerEquivalenceTests, SteadyStepTests, StressPresetTests
- **PlayMode:** GymPlayTests, HudLayoutPlayTests, LockstepLoopbackTests, MatchClockTests, MatchLaunchPlayTests, ShellRouterPlayTests
- **Project:** FreshCloneSetupTests, ShaderInclusionTests, SkinAssetTests, WalkerPivotTests
- **Show:** AllocProbeSanityTests, AnimationPinTests, AssetScaleTests, BenchOptionsTests, BiomeProfileTests, BlastReactionTests, CampaignProfileTests, ColumnLightTests, ColumnPlayTests, ComponentLookupAllocationTests, DeathGagTests, DeathVarietyTests, DebrisTests, DefinedMachineModelTests, DrainageTests, DriveStyleTests, EnvAtlasTests, FallenFlightTests, FigurePartsTests, FrameBudgetCoverageTests, GaitTests, GibPlanTests, GrenadeLookTests, GymCatalogueTests, HitchAttributionTests, HollowRescanTests, HouseKitTests, HullRideTests, KeyMapTests, KnobsTests, MachineLampTests, MachineLightPoolTests, MachineSocketTests, NightLookTests, PaintedHorizonCompressionTests, PropWearTests, RiderSeatTests, ScatterRulesTests, SceneStaticsTests, ScorchTilePainterTests, ShotLogTests, ShotStaggerTests, StaticLifecycleTests, TickAllocationTests, TracerGlowTests, TrackDustTests, TrenchSectionTests, VatAssetTests, VatAtlasMemoryTests, VatEarlyZTests, VatTintTests, VehicleDeathTests, ViewGroundTests, WalkerJointTests, WreckModelTests
- **Sim:** AbilityArgsTests, AirDropTests, BarragePatternTests, BattlefieldLockstepTests, BattlefieldTests, BeamTests, BreakerTests, BurningSystemTests, ChassisTests, CoastTests, CombatTests, CommandSeatTests, CommandValidationTests, CrabTests, DeadGroundTests, DeathEventContractTests, DeterminismReplayTests, DirectionalBlastTests, DriveFeelTests, DynamicGroundTests, FactionRosterTests, FlamethrowerTests, FlowFieldManagerTests, FlowFieldTests, GarrisonAndOrdersTests, GarrisonTests, GrenadeTests, HashIntervalTests, HeightfieldRaycastTests, HeroTests, JetpackTests, LandingTests, LaneAndEngageRulesTests, LoadoutTests, MeleeTests, MineTests, OfficerTests, PlaytestMapTests, ProvingGroundBehaviourTests, ProvingGroundUnitTests, SapperTests, ShieldTests, SimHashTests, SmokeScreenTests, SpreadAndEngageTests, StrafeRunTests, SupportAbilityTests, SupportUnitTests, TankMobilityTests, TankTests, TrenchSpreadTests, UnitCatalogueTests, UnitDefinitionTests, WalkerArmamentTests, WinterMapTests, WreckDecayTests, WreckRecordTests
- **Stills:** BenchRuns, DeathStills, VfxStills, WalkerStills, WreckStills
- **UI:** AbilityAimTests, CampaignGraphTests, ComicWordsTests, FactionBuildingsTests, GameSettingsTests, HomeFrontDioramaTests, HudBindTests, HudLayoutTests, HudStructureTests, HudTextTests, SelectionTests, ShellUxmlTests, StrategicMapMeshTests, UnitArtTests, WinterLevelTests
<!-- /gen:tests -->
