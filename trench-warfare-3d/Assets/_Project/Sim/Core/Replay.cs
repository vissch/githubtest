// Phase: P0 (implemented)
// Replay file layout (version in ReplayRecorder.FormatVersion): header (magic, format version, config, world init,
// map id, map hash, data hash) + per tick (command count, commands, state hash). Bump FormatVersion whenever a
// contract in docs/02 or this layout changes;
// MapHash/DataHash let a player reject a replay recorded against different map or unit data instead of desyncing.
// Used by DeterminismReplayTests, the platform gate report, desync dumps (N3) and the spectator/replay UI.
using System.Collections.Generic;
using System.IO;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim
{
    public sealed class ReplayRecorder
    {
        public const uint Magic = 0x31525754; // "TWR1"
        // v4 (2026-09-24): MapData is folded into the tick hash by TerrainHashSystem -- the heightfield, the nav
        // layers and the holes the shells have dug are compared state now, which docs/03 deferred "to the next
        // replay-format break". Blast impacts also carry a direction and a shape.
        // v5 (2026-09-26): BurningSystem is registered (its per-slot fire and burning cells join the hash), a blast's
        // dead carry their knock in the Death event, and Impact has an Incendiary shape. Header layout unchanged.
        // v6 (2026-09-26): SupportFire.b carries AbilityArgs (heading, pattern, length); ScheduledPayload gains Dir,
        // Radius, Kind and Ticks (hashed); GasSmokeSystem's smoke field and its sources join the hash while a screen
        // is up; the HE scatter draws from SimRandom.SystemId.Abilities; Impact gains SafeBehind. Layout unchanged.
        // v7 (2026-09-26): BeamSystem is registered (its running sweeps join the hash); OffMapAbilityId.Beam = 11 is a
        // real ability whose BeamStart payload carries heading x length in Dir; BlastShape.Beam = 4. Layout unchanged.
        // v8 (2026-09-26): MineSystem is registered (mines and tripwires join the hash, order 1130); BlastShape.Mine = 5.
        // Before v8 landed anywhere (2026-09-27): BlastShape.Strafe = 6 (a strafe's rounds throw nobody) and the ability dice
        // are salted by ability id as well as player and round, and BurningSystem hashes every slot's timer. Layout unchanged.
        // v9 (2026-09-27, the units-meta lane landed on the overhaul): SimConfig carries FactionA/FactionB, HeroPity0/1 and
        // the ten archetypes each side chose (Loadout*), written as a count and that many bytes per side, so the header
        // layout changed; its systems (aura, support, hero, leap, breaker, air drop, the combat catalogue) join the hash.
        // That lane had numbered these v5 and v6 on its own branch; those numbers were the overhaul's by the time it landed.
        // v10 (2026-09-28): UnitDefinitions.All holds its first two units (the Skimmer 19 and the Salvo 20, fielded by
        // no faction), so the match's unit table, whose Fingerprint UnitCatalogue and CombatCatalogue fold into every
        // tick's hash, is not the one v9 recorded against. Layout and hash chain unchanged.
        // v11 (2026-09-28): the Salvo's shot is a rack of rockets held in the air (TankGunnerySystem.Rockets, hashed at the
        // end of that system's part of the chain) and each bursts on its own later tick; SimEventType.RocketFired;
        // SimRandom.SystemId.Salvo = 22; TankSpec gains Rockets and RocketSpeed (the combat table's fingerprint).
        // v12 (2026-09-28): TankSpec gains StandOff (the Salvo holds while it has a target) and VehicleProfile Clearance
        // (added to Radius; the Skimmer and Salvo keep 1.2 m more round them); the Salvo's rack is 16 rockets, not 12.
        // v13 (2026-09-28, the balance critic's round): TankGunnerySystem hashes the stand-off hold (HoldTarget, HoldTicks,
        // Release, HoldHp) after the rockets; TankSpec.StandOff (bool) became StandOffMetres/StandOffPatience; InfantrySpec
        // gains HuntsArmour and LooksDownMetres; a machine's armour-hunting small arms fire armour-piercing at machines.
        // v14 (2026-09-28, critic round 3): TankGunnerySystem hashes HoldGoal after HoldHp; the hold is reset for a new unit
        // in a dead machine's slot, and holds only on the machine's first goal.
        // v15 (2026-09-28, critic round 4): PendingRocket gains LaunchTick, Shooter, ShooterGen (hashed with it): a rocket
        // still in its tube when its machine dies is dropped; a rack's landing points are clamped to the map.
        // v16 (2026-09-28, the Proving Ground): sixteen definitions (archetypes 21-36) in UnitDefinitions.All and two
        // InfantrySpec fields (MineCharges, NeverPinned) change the unit tables' fingerprints from tick 0; the header gains
        // SimConfig.Endless; UnitAbilityId 13-14, SapperOrdered/SapperLaying and SimSystemOrder.Sapper appended, unused yet.
        // v17 (2026-09-28, the sapper): SapperSystem joins the chain at order 120 with its per-slot state (Charges, Phase,
        // Kind, Goal, Args, LayTicks, Back, Target); CommandType.UnitAbility is consumed (b = UnitAbilityId in the low
        // byte, AbilityArgs above it); MineSystem's trigger reads the match's vehicle profiles.
        // v18 (2026-09-28, spread and the fight): infantry cross a trench wall anywhere (FlowField.CanStepInfantry,
        // ParapetCost), every man on foot keeps to a lane (Lane) and is deployed on it, and EngageSystem joins the chain
        // at order 1108 with its state (Hunt, HuntGen, the generation seen, MovementSystem.Engage): men in the open
        // close on the enemy and hold to shoot. Layout unchanged; every battle runs differently from the first deploy.
        // v19 (2026-09-28, the assault): a running man in the open is a harder mark the farther off he is
        // (CombatTables.RunningTarget), a miss at a man on the fire step suppresses him fully (the parapet), and a man
        // in the open bombs the trench man he fights from 5-22 m (DirectFireSystem, whose hash now folds its per-slot
        // bombs and their generation; SimEventType.GrenadeThrown appended, SourceId.Grenade 2002). Layout unchanged.
        // v20 (2026-09-29, dead ground): a man on foot in the open more than CombatTables.DeadGroundMetres behind his own
        // side's front trench is not a target for a shooter on the far side of it (TargetAcquisition). Layout unchanged.
        // v21 (2026-09-29, the bomb's flight): a thrown bomb is in the air CombatTables.GrenadeFlightTicks and goes off
        // where it lands on that tick (DirectFireSystem hashes the bombs in the air). Layout unchanged.
        // v22 (2026-09-29, the guns): machine gunners (and the Sentry) answer OrderGroup.Gun, not Line, so a
        // TrenchSelectAdvance naming the line leaves them in the trench. The unit table's groups are hashed. Layout unchanged.
        // v23 (2026-09-29, how machines drive): VehicleProfile gains Accel, Brake and PivotSpeed, so the hashed profile
        // table's bytes change; VehicleKinematicsSystem drives with momentum (Velocity carries the way from tick to tick),
        // follows the field 3-6 cells ahead before its lane bends it, steers round a hull on its line, and the Breaker charges where the men are. Same
        // inputs, different tracks: a v22 replay does not replay. Layout and chain unchanged.
        // v24 (2026-09-28, the range cut, lane/sim/melee): every attack reach at CombatTables.RangeScale 0.8 of its design
        // (weapon and main-gun ranges, minimum ranges, stand-offs, AdvanceFireRange, EngageSystem.HuntRadius): the
        // catalogue fingerprints change. Layout and chain unchanged.
        // v25 (2026-09-28, hand to hand, lane/sim/melee): MeleeSystem (order 1109: Foe, FoeGen, Contact, Swing, Dropped,
        // Calm, StruckBy, the generation seen) and PounceSystem (order 1125: Phase, Ticks, Cooldown, From, To, Target, the
        // generation seen) join the chain; MovementSystem.EngageMelee and Stance.Melee; UnitFlags Melee, Disarmed and
        // Pouncing (bits 16-18); events MeleeBlow, WeaponDropped, WeaponPickedUp, PounceCrouched, PounceLanded appended;
        // random streams Melee 23 and Pounce 24.
        // v26 (2026-09-28, wrecks break in stages: seam): PropKind gains BrokenWreck, Scrap and Cleared (appended),
        // SimEventType gains PropWorn (appended), VehicleCrushed.b = 3 is wreckage. Nothing produces them yet.
        // v27 (2026-09-28, wrecks break in stages: blasts): a wreck has hit points sized to its machine (PropDef.Scale) and
        // blasts wear it down a stage at a time; generated maps' wrecks start at 600, so their MapHash changes; the prop
        // checksum folds (prop, kind) as an int2.
        // v28 (2026-09-28, wrecks break in stages: machines): a heavy machine grinds down a wreck it brushes or pushes
        // against and any machine flattens scrap it drives over (VehicleKinematicsSystem, every 10 ticks a machine), through
        // the same PropHarm as a blast; the kinematics checksum folds what the tracks wore.
        // v29 (2026-09-28, wrecks break in stages: gunfire): a machine gun's round that a wreck's cover stopped (the same
        // roll: a miss that would have hit with no cover) wears that wreck by its share of the cover (DirectFire.Wrecks).
        // v30 (2026-09-28, wrecks break in stages: guns turn on them): a gun with nobody to shoot at fires at a wreck that
        // shelters its enemies (Shot.b = PropTarget), keeping their heads down; a machine gun's hits wear it (DirectFire.Wrecks).
        // v31 (2026-10-01, round a hull that cannot move): Avoid reads which side a hull ahead is on off the machine's
        // own nose, not the wanted line, so a machine pressed behind a ditched or knocked-out hull goes round it instead
        // of swinging its nose either way; a hull's push grows from nothing at the cone's edge (it stepped in at 0.3).
        // Same inputs, different tracks. Layout and chain unchanged.
        // v32 (2026-10-01, a garrison man squeezes past his mates): between a man on his way to his post and his trench
        // mates the separation push is the soft one at a body's width, not the 2 m spacing with its hard core. Same inputs,
        // different steps. Layout and chain unchanged.
        // mates the separation push is the soft one at a body's width, not the 2 m spacing with its hard core; and
        // EngageSystem walks the way to a man's quarry cell by cell, a man already closing keeping on while CloseKeep
        // metres are open. Same inputs, different steps. Layout and chain unchanged.
        // v33 (2026-10-01, the machines' reach, the owner: "reduce range"): the machines' guns reach 110-150 m and the
        // Kettle's mortar and the Salvo's rockets 155 m (they reached 200-380 m), so no machine reaches the enemy's front
        // trench from its spawn; the Salvo holds to fire at 155 m. The hashed hull table's bytes change. Same inputs,
        // different battles. Layout and chain unchanged.
        // v34 (2026-10-01, men under fire, the owner: "as realistic as possible"): SimWorld.Alarm (float per slot, how
        // lately and how hard he was fired on) is appended to the hashed arrays after Knock; SuppressionSystem raises it to
        // his suppression and lets it down 2.5 a second, and a man in the open with it up moves in rushes (bent double,
        // then down on his belly) and makes for shell holes ahead; on open ground a man slips past a mate who is down,
        // and the push between two men up has no step at a metre. Hash content changes; the system chain is unchanged.
        // v35 (2026-09-30, the Bullfrog): VehicleArchetype.Bullfrog 37 in UnitDefinitions.All: the unit table's and the
        // combat catalogue's fingerprints change. Layout and chain unchanged.
        // v36 (2026-10-06, the fleet fires): SeaLanding resolves the blast system when it fires, not at Initialize, so
        // the fleet's guns can fire on a coast map (a naval shell, its crater, SimEventType.ShipFired) once
        // SeaLandingSystem.FleetFires is switched on (off in a match: the owner, 2026-10-06), and a craft that cannot
        // put its men ashore (the field is full) refunds the hold instead of losing it. A battle with a full field
        // plays differently: a v35 replay does not replay. Layout and chain unchanged.
        // v37 (2026-10-06, the leap lands safely): a man in the air is really out of the fight. LeapSystem owns the
        // Airborne clear (MoveJob no longer clears it on touchdown), so the landing grace holds for
        // LandingGraceTicks; his own landing burst, mines, blast, ground fire and gas no longer reach him while he
        // is Airborne; he is not garrisoned, not put InTrench and not shoving men about until he is down.
        // Same inputs, a different battle (he keeps his hp and joins the line later): a v36 replay does not replay.
        // Layout and chain unchanged.
        // v38 (2026-10-09, [U6]): a side wins its own trenches back nearest first, Reserve then Main (SectorControl.Unlocked).
        // v41 (2026-10-09, [N5.2]): a knocked-out hull does not fire at a wreck. DirectFire.AtWreck returns for a
        // KnockedOut slot, so a dead hull stops machine-gunning the wreck its enemies stand by (shots, suppression
        // and wreck wear) until it cooks off. Layout and chain unchanged.
        // v42 (2026-10-09, [N5.6]): a gun turns on a wreck only on its own scan tick (slot % 3 == tick % 3, the
        // stagger TargetAcquisition acquires on), so a man whose mark ducked on an off tick keeps his cooldown for
        // the scan that can give the man back. Layout and chain unchanged.
        // v43 (2026-10-09, [N5.3]): a wreck shelters only the men in its shadow, and a screen on the line blinds the
        // shot. DirectFire.AtWreck lists and suppresses only enemies past the wreck on the shooter's line and within
        // its body (WreckBlastReach x size, halved), holds its cooldown when nobody is in that shadow, and returns when
        // SmokeBlindMetres of thick smoke lie on the line. A gun no longer pins a trench bay on a wreck's flank.
        // Layout and chain unchanged.
        // v44 (2026-10-09, [N5.4]): a wreck's size comes from the dead machine, not from its slot. The
        // VehicleDestroyed event carries the archetype + 1 in dir.x (dir.y stays the hull yaw) and Deformation reads
        // it, so a slot let again in the same step no longer shrinks the wreck. Layout and chain unchanged.
        // v45 (2026-10-09, [N5.1]): a crab's leap no longer crushes a man caught under its flight: VehicleKinematics'
        // crush check now skips a hull with UnitFlags.Pouncing set. Behaviour only, no layout change.
        // v46 (2026-10-12, [N5.3b]): smoke no longer stops fire at a wreck (the owner's answer): DirectFireSystem's
        // AtWreck dropped its SmokeBlindMetres test, so a gun fires at a wreck through a screen again and the men in
        // its shadow are kept down. Behaviour only, no layout change, hash chain unchanged.
        // v48 (2026-10-12, [S4] [C5]): the hash chain gains two pieces of state. SimWorld.Hash() appends the free
        // list at the very end (its LIFO order decides the next Spawn's slot), and BlastSystem.Hash() folds Pending
        // (a burst queued after order 720 waits there for the next Step). Hashes move, behaviour and balance do not.
        // v50 (2026-10-13, [N4] [N6]): a merged crater now CARVES the ground its widening claims (a hole at
        // MaxDepth still digs), and the crush box holds the whole hull rectangle, tested symmetrically. Behaviour
        // only, no layout change; crater heights and crush outcomes move, so hashes and balance move.
        // v51 (2026-10-13, [U3]): the wreck record's provenance comes from SimWorld.VehicleDeaths, captured at the
        // death, not off the slot (a bail-out crew can hold it by then, the free list is LIFO), and DeformationSystem
        // reads that list instead of the droppable event buffer. Record contents move where the bug triggered, and
        // where the event buffer had overflowed, which deaths leave a wreck moves too.
        // v52 (2026-10-13, [U11]): BreakerSystem, SapperSystem and SupportSystem hash their per-slot arrays to
        // SimWorld.HighWater instead of MaxSlots, as SimWorld.Hash() already did. No field added, moved or dropped
        // and no balance change; the hash of any match with an unfilled world moves, so older replays do not verify.
        // v53 (2026-10-15, [found-shelter-body-uses-full-wreckblastreach-reach-20261015-0]): a wreck's shadow is as
        // wide as its body is, half of WreckBlastReach x size, the same figure its shelter's reach is built from.
        // Body was the full WreckBlastReach, twice that, so a man up to 2 m off the firing line was kept down. No
        // field added, moved or dropped; shots, suppression and wreck wear move, so hashes and balance move.
        public const ushort FormatVersion = 53;
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public int MapId;        // MapData.MapId of the map the replay was recorded on (0 = unknown)
        public ulong MapHash;   // MapData.Hash() of the map the replay was recorded on (0 = unknown)
        public ulong DataHash;  // hash of the baked unit/weapon/ability tables (0 = unknown; MatchSim.NewReplayRecorder wires it)
        public byte[] MapParams = System.Array.Empty<byte>();   // what a generated map is rebuilt from (BattlefieldParams.Serialize); empty for fixed maps
        public readonly List<SimCommand[]> Commands = new List<SimCommand[]>();
        public readonly List<ulong> Hashes = new List<ulong>();

        public ReplayRecorder(SimConfig config, SimConfig.WorldInit init, int mapId, ulong mapHash = 0, ulong dataHash = 0)
        { Config = config; Init = init; MapId = mapId; MapHash = mapHash; DataHash = dataHash; }

        public void Record(NativeArray<SimCommand> tickCommands, ulong hashAfterStep)
        {
            // S5: a world with HashInterval != 1 reports LastHash 0 on the ticks it skipped. A recording of those
            // zeros verifies against any run at all, so refuse the zero hash here rather than ship a verify that
            // cannot fail.
            if (hashAfterStep == 0)
                throw new System.ArgumentException("ReplayRecorder.Record was given hash 0: the world must hash every tick (HashInterval == 1)", nameof(hashAfterStep));
            Commands.Add(tickCommands.ToArray());
            Hashes.Add(hashAfterStep);
        }

        public byte[] Serialize()
        {
            using var ms = new MemoryStream();
            using var w = new BinaryWriter(ms);
            w.Write(Magic);
            w.Write(FormatVersion);
            w.Write(Config.TickRate); w.Write(Config.InputDelayTicks); w.Write(Config.MaxSlots); w.Write(Config.Seed);
            w.Write(Config.SilverPerSecond); w.Write(Config.StartingSilver); w.Write(Config.EventCapacity);
            w.Write(Config.FactionA); w.Write(Config.FactionB); w.Write(Config.HeroPity0); w.Write(Config.HeroPity1);
            w.Write(Config.Endless);   // v16
            WriteLoadout(w, Config.LoadoutA); WriteLoadout(w, Config.LoadoutB);
            WriteF3(w, new float3(Init.SizeMeters, 0f)); WriteF3(w, Init.SpawnA); WriteF3(w, Init.SpawnB); w.Write(Init.GoalZA); w.Write(Init.GoalZB); w.Write(Init.TestLevel);   // v39
            w.Write(MapId);
            w.Write(MapHash); w.Write(DataHash);
            w.Write(MapParams.Length); w.Write(MapParams);
            w.Write(Commands.Count);
            for (int t = 0; t < Commands.Count; t++)
            {
                var cs = Commands[t];
                w.Write(cs.Length);
                foreach (var c in cs)
                {
                    w.Write(c.Tick); w.Write(c.Player); w.Write((byte)c.Type); w.Write(c.A); w.Write(c.B); WriteF3(w, c.Pos);
                }
                w.Write(Hashes[t]);
            }
            return ms.ToArray();
        }

        static void WriteF3(BinaryWriter w, float3 v) { w.Write(v.x); w.Write(v.y); w.Write(v.z); }

        static void WriteLoadout(BinaryWriter w, in Unity.Collections.FixedList32Bytes<byte> l)
        {
            w.Write((byte)l.Length);
            for (int i = 0; i < l.Length; i++) w.Write(l[i]);
        }
    }

    public sealed class ReplayPlayer
    {
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public int MapId;
        public ushort FormatVersion;
        public ulong MapHash, DataHash;
        public byte[] MapParams;
        public SimCommand[][] Commands;
        public ulong[] Hashes;
        public int TickCount => Commands.Length;

        // S8: a command is 26 bytes on the wire (Tick 4 + Player 1 + Type 1 + A 4 + B 4 + Pos 12) and each tick
        // also carries an 8-byte hash tail, so a tick count t needs at least 12 * t bytes left (the smallest
        // per-tick record, zero commands) and a command count n needs 26 * n + 8.
        const int CommandBytes = 26;
        const int TickTailBytes = 8;
        const int TickMinBytes = 12;

        public static ReplayPlayer Parse(byte[] bytes)
        {
            try
            {
                using var ms = new MemoryStream(bytes);
                using var r = new BinaryReader(ms);
                if (r.ReadUInt32() != ReplayRecorder.Magic) throw new InvalidDataException("Not a TWR1 replay");
                var p = new ReplayPlayer();
                p.FormatVersion = r.ReadUInt16();
                if (p.FormatVersion != ReplayRecorder.FormatVersion)
                    throw new InvalidDataException($"Replay format v{p.FormatVersion}, this build reads v{ReplayRecorder.FormatVersion}");
                p.Config = new SimConfig
                {
                    TickRate = r.ReadInt32(), InputDelayTicks = r.ReadInt32(), MaxSlots = r.ReadInt32(), Seed = r.ReadUInt32(),
                    SilverPerSecond = r.ReadSingle(), StartingSilver = r.ReadInt32(), EventCapacity = r.ReadInt32(),
                    FactionA = r.ReadByte(), FactionB = r.ReadByte(), HeroPity0 = r.ReadSingle(), HeroPity1 = r.ReadSingle(),
                    Endless = r.ReadBoolean(),   // v16
                };
                p.Config.LoadoutA = ReadLoadout(r); p.Config.LoadoutB = ReadLoadout(r);
                float3 size = ReadF3(r);
                p.Init = new SimConfig.WorldInit { SizeMeters = size.xy, SpawnA = ReadF3(r), SpawnB = ReadF3(r), GoalZA = r.ReadSingle(), GoalZB = r.ReadSingle(), TestLevel = r.ReadBoolean() };   // v39
                p.MapId = r.ReadInt32();
                p.MapHash = r.ReadUInt64(); p.DataHash = r.ReadUInt64();
                int mapParamsLength = r.ReadInt32();
                long left = ms.Length - ms.Position;
                if (mapParamsLength < 0 || mapParamsLength > left)
                    throw new InvalidDataException($"Replay MapParams length {mapParamsLength}, only {left} bytes left");
                p.MapParams = r.ReadBytes(mapParamsLength);
                int ticks = r.ReadInt32();
                left = ms.Length - ms.Position;
                if (ticks < 0 || (long)TickMinBytes * ticks > left)
                    throw new InvalidDataException($"Replay tick count {ticks}, only {left} bytes left");
                p.Commands = new SimCommand[ticks][];
                p.Hashes = new ulong[ticks];
                for (int t = 0; t < ticks; t++)
                {
                    int n = r.ReadInt32();
                    left = ms.Length - ms.Position;
                    if (n < 0 || (long)CommandBytes * n + TickTailBytes > left)
                        throw new InvalidDataException($"Replay tick {t} command count {n}, only {left} bytes left");
                    var cs = new SimCommand[n];
                    for (int i = 0; i < n; i++)
                        cs[i] = new SimCommand { Tick = r.ReadUInt32(), Player = r.ReadByte(), Type = (CommandType)r.ReadByte(), A = r.ReadInt32(), B = r.ReadInt32(), Pos = ReadF3(r) };
                    p.Commands[t] = cs;
                    p.Hashes[t] = r.ReadUInt64();
                }
                // S8/S3.7: anything after the last tick's hash is not part of the format and hides a truncated or
                // appended file (a shorter tick count than the bytes on hand would otherwise just stop early).
                if (ms.Position != ms.Length)
                    throw new InvalidDataException($"Replay has {ms.Length - ms.Position} trailing byte(s) after the last tick");
                return p;
            }
            catch (InvalidDataException) { throw; }
            catch (System.Exception ex) when (ex is EndOfStreamException || ex is System.ArgumentException || ex is System.OverflowException)
            {
                throw new InvalidDataException("Replay file is truncated or corrupt", ex);
            }
        }

        /// <summary>Re-simulate and return the first tick whose hash differs, or -1 if the replay verifies.
        /// Throws InvalidDataException naming the first field by which the recorded header and the given world's
        /// Config disagree (a replay recorded against a different config always desyncs, usually at tick 0, which
        /// hid the real cause). mapHash/dataHash/mapId are compared against the recorded MapHash/DataHash/MapId
        /// only when both the recorded and the given value are non-zero (0 means "unknown" on either side; a mapId
        /// mismatch is checked and reported before Config, so a wrong map is named as a wrong map, not as a
        /// differing SizeMeters). MapParams has nothing in SimWorld to compare against (it only feeds the map
        /// generator that built the world before Verify ever sees it), so it is not checked here.</summary>
        public int Verify(SimWorld world, ulong mapHash = 0, ulong dataHash = 0, int mapId = 0)
        {
            // S5: a world that does not hash every tick leaves LastHash 0, so a good replay would read "desync at
            // tick 0"; a world that has already stepped cannot re-simulate the recording from its start.
            if (world.HashInterval != 1)
                throw new System.ArgumentException($"Verify needs a world that hashes every tick (HashInterval == 1), got {world.HashInterval}", nameof(world));
            if (world.Tick != 0)
                throw new System.ArgumentException($"Verify needs a fresh world at tick 0, got tick {world.Tick}", nameof(world));
            if (MapId != 0 && mapId != 0 && MapId != mapId)
                throw new InvalidDataException($"Replay recorded MapId {MapId}, given world built from MapId {mapId}");
            if (MapHash != 0 && mapHash != 0 && MapHash != mapHash)
                throw new InvalidDataException($"Replay recorded MapHash {MapHash}, given world built from MapHash {mapHash}");
            if (DataHash != 0 && dataHash != 0 && DataHash != dataHash)
                throw new InvalidDataException($"Replay recorded DataHash {DataHash}, given world built from DataHash {dataHash}");
            CheckConfig(world);
            for (int t = 0; t < TickCount; t++)
            {
                using var cmds = new NativeArray<SimCommand>(Commands[t], Allocator.Temp);
                world.Step(cmds);
                if (world.LastHash != Hashes[t]) return t;
            }
            return -1;
        }

        void CheckConfig(SimWorld world)
        {
            var c = world.Config;
            void Mismatch(string field) => throw new InvalidDataException($"Replay recorded with a different {field}");
            if (Config.TickRate != c.TickRate) Mismatch(nameof(SimConfig.TickRate));
            if (Config.InputDelayTicks != c.InputDelayTicks) Mismatch(nameof(SimConfig.InputDelayTicks));
            if (Config.MaxSlots != c.MaxSlots) Mismatch(nameof(SimConfig.MaxSlots));
            if (Config.Seed != c.Seed) Mismatch(nameof(SimConfig.Seed));
            if (Config.SilverPerSecond != c.SilverPerSecond) Mismatch(nameof(SimConfig.SilverPerSecond));
            if (Config.StartingSilver != c.StartingSilver) Mismatch(nameof(SimConfig.StartingSilver));
            if (Config.EventCapacity != c.EventCapacity) Mismatch(nameof(SimConfig.EventCapacity));
            if (Config.FactionA != c.FactionA) Mismatch(nameof(SimConfig.FactionA));
            if (Config.FactionB != c.FactionB) Mismatch(nameof(SimConfig.FactionB));
            if (Config.HeroPity0 != c.HeroPity0) Mismatch(nameof(SimConfig.HeroPity0));
            if (Config.HeroPity1 != c.HeroPity1) Mismatch(nameof(SimConfig.HeroPity1));
            if (Config.Endless != c.Endless) Mismatch(nameof(SimConfig.Endless));
            if (!LoadoutEquals(Config.LoadoutA, c.LoadoutA)) Mismatch(nameof(SimConfig.LoadoutA));
            if (!LoadoutEquals(Config.LoadoutB, c.LoadoutB)) Mismatch(nameof(SimConfig.LoadoutB));
            // Init is compared only when this replay actually recorded a world (SizeMeters non-zero): several
            // existing tests (DeterminismReplayTests, Tests/Match/DefinedUnitTests) record `default` Init and
            // verify against real worlds, and Tests/Match is outside this lane.
            if (!Init.SizeMeters.Equals(default(float2)))
            {
                if (!Init.SizeMeters.Equals(world.Init.SizeMeters)) Mismatch(nameof(SimConfig.WorldInit.SizeMeters));
                if (!Init.SpawnA.Equals(world.Init.SpawnA)) Mismatch(nameof(SimConfig.WorldInit.SpawnA));
                if (!Init.SpawnB.Equals(world.Init.SpawnB)) Mismatch(nameof(SimConfig.WorldInit.SpawnB));
                if (Init.GoalZA != world.Init.GoalZA) Mismatch(nameof(SimConfig.WorldInit.GoalZA));
                if (Init.GoalZB != world.Init.GoalZB) Mismatch(nameof(SimConfig.WorldInit.GoalZB));
                if (Init.TestLevel != world.Init.TestLevel) Mismatch(nameof(SimConfig.WorldInit.TestLevel));
            }
        }

        static bool LoadoutEquals(in Unity.Collections.FixedList32Bytes<byte> a, in Unity.Collections.FixedList32Bytes<byte> b)
        {
            if (a.Length != b.Length) return false;
            for (int i = 0; i < a.Length; i++) if (a[i] != b[i]) return false;
            return true;
        }

        static Unity.Collections.FixedList32Bytes<byte> ReadLoadout(BinaryReader r)
        {
            var l = new Unity.Collections.FixedList32Bytes<byte>();
            int n = r.ReadByte();
            if (n > l.Capacity)
                throw new InvalidDataException($"Replay loadout count {n} overruns FixedList32Bytes<byte>.Capacity {l.Capacity}");
            for (int i = 0; i < n; i++) l.Add(r.ReadByte());
            return l;
        }

        static float3 ReadF3(BinaryReader r) => new float3(r.ReadSingle(), r.ReadSingle(), r.ReadSingle());
    }
}
