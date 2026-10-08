# The Ridge: one side holds the high ground (design, 2026-10-08)
Read at 2bad681c. Every "today" names the file it was read in; paths are under `trench-warfare-3d/Assets/_Project/`.

## 1. The rule
One battlefield where the ground climbs from one front trench to the other. The low side (the hollow) is today's field;
the high side (the crest) sits about 9 m above it, on top of a long open slope that ends in a short steep bank. From
the crest you see every metre of the slope and nobody below sees what stands behind you; from the hollow you climb in
full view, your machines crawl on the bank and your shells burst short against the lip. **No new combat rule:** the
height does it through rules the sim already has. Two mission cards, one map mirrored.

## 2. What the player sees and does (standard view: fov 25, pitch 25, zoom 30, yaw 21; decisions.md 2026-09-21)
- **Uphill card (enemy on the crest).** The field tilts away from him; the enemy line is a lit rim against the sky.
  His men cross the slope with no fold to wait in, reach the wire on the bank and bomb upward; his walkers take the
  bank at full speed while his tanks grind. He never sees the enemy's reinforcements arrive, only the rim refill.
- **Downhill card (he holds the crest).** His men come up behind the hill unseen; his parapet guns cover the attack
  all the way down. The camera rides the ground (`Presentation/Camera/TacticalCamera.cs` samples it).

## 3. Numbers (all placeholders; the balance simulator sweeps them)
- **Rise 9 m:** five times today's biggest fold (ground 1.2 m +/- 1.65 m, `Sim/Terrain/BattlefieldGenerator.cs` step 1).
- **Slope: 55 m at about 0.08, then a 15 m bank at 0.30, straight up to the parapet (no flat lip).** A machine slows
  above half its `SlopeLimit` (`Sim/Nav/VehicleKinematics.cs`, `SlopeFactor`): at 0.30 the two machines with limit 0.4
  (`Sim/Match/UnitDefinitions.cs`) drop to half speed, tanks (0.55) to about 0.9, walkers (0.86 and up) not at all.
- **Field 110 x 240 m, no river, no sea.** Length 240 keeps no man's land at 100 m (decisions.md 2026-10-01), so the
  reaches stand: machine gun 136 m, rifle 104 m (`Sim/Combat/CombatTables.cs`, x `RangeScale` 0.8). From the crest
  the gun covers the slope and 36 m behind the hollow's front trench; the rifle just reaches that trench.
- **Hidden rear:** a standing man 15 m or more behind the bank's top is unseen from below (my sums on the eye
  heights in `Sim/Combat/HeightfieldRaycast.cs`; a test must measure it, I ran none).

## 4. Edge cases
1. **In a trench:** today the ground is levelled 5 m either side of a trench line and blended over 7 m more. A flat lip
   before the crest trench blinds it: a trench man's muzzle is 0.3 m over the ground just outside his trench
   (`Sim/Combat/TargetAcquisition.cs`, `Muzzle`); by my sums the whole slope drops from his sight. Level behind only.
2. **Shells:** a hole's depth is capped per hole (`CraterStamp.MaxDepth`); a blast loses 0.45 behind ground (`Blast.cs`).
3. **Indirect fire and gas:** mortars skip the sight check (`TankGunnery.cs`, `Indirect`) and reach the hidden rear;
   gas moves by the wind and sinks only into trench and crater cells (`GasSmokeField.cs`), not downhill.
4. **Map edge, 3,000 units, a death mid-climb:** the rise depends on Z only; no per-tick cost; no new death rule.
5. **Replay:** the map is rebuilt from the params blob; a field appended as `SeaMargin` was reads 0 in an old blob.
6. **Men on foot:** `Sim/Nav/MovementSystem.cs` never reads the height: they climb at full speed (question 1).

## 5. What it needs
- **Sim (generator only):** `BattlefieldParams.Rise` (metres, signed: + raises team 1's side, - team 0's; 0 = today),
  appended to `Serialize`; the dug-in height follows the profile; a `Ridge(seed)` preset. Tests: rise 0 leaves the
  three presets' hashes alone (no pinned hash found in `BattlefieldTests.cs`); crest sees the foot, foot not the rear.
- **Show:** `Ground.Ridge = 3` and its two switches (`MatchLaunch.Field`, `BiomeProfile.ForGround`: night mud), two
  mission cards (`UI/Shell/MissionCard.cs`, `Ground`), a standard-view capture of each proving the hill reads.
- **Seams:** no archetype id, no ability id, no event. The params blob grows by one float; `Ground` gains a value.
  Replay `FormatVersion` (37, `Sim/Core/Replay.cs`): my reading is no bump, as no rule changes; the builder confirms.
- **Cut:** a range bonus for height, gas flowing downhill, bodies rolling down the bank, a reverse slope, campaign placing.

## 6. What it must not break
- **Dead ground is removed** (decisions.md 2026-10-06; not built: `TargetAcquisition.InDeadGround` is still in the
  code). The design leans on no part of it: what hides the crest's rear is the ground itself, through the sight check.
- **No machine duels from spawn** (2026-10-01); **no constant bombardment** (2026-09-28); **40 to 60 % per faction**
  (2026-10-06): the simulator reports each card's side split, which that band does not judge.

## 7. Open to the owner
1. **Should men on foot climb the bank slower?** It would cost infantry time under the parapet; it is a sim rule on
   every map (shell holes exempt) and breaks old replays. **Default: no.** Build the map, judge it in play.
2. **Once dead ground goes, the hollow's last 36 m to its own trench lie under the crest's guns:** his "the man werent
   able to enter the trench" case, on one side only. **Default: accept it as the hollow's price on this map.**
