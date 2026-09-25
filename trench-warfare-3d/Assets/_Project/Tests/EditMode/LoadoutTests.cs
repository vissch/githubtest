// Phase: A5c (implemented 2026-09-25) — the ten a player chose travel with the match.
//
// The owner chose a per-battle loadout, so a side's ten slots are no longer its faction's fixed ten. That makes the
// choice part of what both machines must agree on before the first tick, which is why it sits in SimConfig beside the
// factions and the seed rather than anywhere in the presentation: if one machine thinks slot 3 is a sniper and the
// other thinks it is a medic, the battle desyncs on the first deploy and the hash says so a hundred ticks too late.
//
// The subtler property here is the ORDER. The slot roster used to be filled from RosterEntry.ForArchetype inside the
// constructor, while UnitDefinitions.Apply runs last; a unit that exists only as a definition would have been
// deployable with a zeroed roster line - free, with no hit points. The slots read the catalogue now, and Apply refills
// them. AChosenUnitThatOnlyADefinitionKnowsArrivesWithItsRealNumbers is that test.
using System.IO;
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class LoadoutTests
    {
        static FixedList32Bytes<byte> Ten(params byte[] a)
        {
            var l = new FixedList32Bytes<byte>();
            foreach (var x in a) l.Add(x);
            return l;
        }

        /// <summary>A deliberately odd ten: nothing like either faction's default order.</summary>
        static FixedList32Bytes<byte> Odd() => Ten(
            InfantryArchetype.Sniper, InfantryArchetype.Medic, InfantryArchetype.Officer, InfantryArchetype.Rifle,
            VehicleArchetype.Redoubt, InfantryArchetype.Machinegunner, VehicleArchetype.Tusk, InfantryArchetype.Shield,
            VehicleArchetype.Kettle, InfantryArchetype.Repair);

        static SimConfig Config()
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = 100000;
            return cfg;
        }

        static void Run(MatchSim m, int ticks)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < ticks; t++) m.Step(none);
        }

        [Test]
        public void TheChosenTenFillTheSlots()
        {
            var cfg = Config();
            cfg.LoadoutA = Odd();
            using var m = MatchSim.CreateGreybox(cfg);
            var chosen = Odd();
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                Assert.AreEqual(chosen[s], m.World.Roster[s].Archetype, $"slot {s}");
        }

        /// <summary>Choosing nothing must be exactly what shipped, or this commit changed every battle.</summary>
        [Test]
        public void ChoosingNothingIsTheFactionsDefaultTen()
        {
            var cfg = Config();
            using var m = MatchSim.CreateGreybox(cfg);
            for (int p = 0; p < SimConfig.MaxPlayers; p++)
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    var want = FactionRoster.Slot(cfg.FactionOf(p), s);
                    Assert.AreEqual(want.Archetype, m.World.Roster[p * RosterEntry.SlotCount + s].Archetype, $"player {p} slot {s}");
                    Assert.AreEqual(want.Cost, m.World.Roster[p * RosterEntry.SlotCount + s].Cost, $"player {p} slot {s} cost");
                }
        }

        /// <summary>A loadout may name only the slots it changes; the rest stay the faction's.</summary>
        [Test]
        public void AShortLoadoutChangesOnlyTheSlotsItNames()
        {
            var cfg = Config();
            cfg.LoadoutB = Ten(InfantryArchetype.Sniper, InfantryArchetype.Sniper);
            using var m = MatchSim.CreateGreybox(cfg);
            int b = RosterEntry.SlotCount;
            Assert.AreEqual(InfantryArchetype.Sniper, m.World.Roster[b + 0].Archetype);
            Assert.AreEqual(InfantryArchetype.Sniper, m.World.Roster[b + 1].Archetype);
            for (int s = 2; s < RosterEntry.SlotCount; s++)
                Assert.AreEqual(FactionRoster.Slot(cfg.FactionOf(1), s).Archetype, m.World.Roster[b + s].Archetype, $"slot {s} untouched");
        }

        /// <summary>
        /// Two machines handed the same non-default loadout run the same battle. This is the property the whole thing
        /// exists for: a loadout that is agreed cannot desync, and one that is not would show here at once.
        /// </summary>
        [Test]
        public void TwoWorldsGivenTheSameChosenTenStayInStep()
        {
            var cfg = Config();
            cfg.LoadoutA = Odd();
            cfg.LoadoutB = Ten(VehicleArchetype.Breaker, InfantryArchetype.Assault, InfantryArchetype.Sniper);
            using var a = MatchSim.CreateGreybox(cfg);
            using var b = MatchSim.CreateGreybox(cfg);
            Assert.AreEqual(a.World.Hash(), b.World.Hash(), "they disagree before the first tick");
            for (int t = 0; t < 400; t++)
            {
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                a.Step(none); b.Step(none);
            }
            Assert.AreEqual(a.World.Hash(), b.World.Hash(), "they drifted apart over 400 ticks");
        }

        /// <summary>A different ten is a different battle, and the tick hash says so before anything has happened.</summary>
        [Test]
        public void ADifferentTenIsADifferentBattleAtTickZero()
        {
            var one = Config();
            var other = Config(); other.LoadoutA = Odd();
            using var a = MatchSim.CreateGreybox(one);
            using var b = MatchSim.CreateGreybox(other);
            Assert.AreNotEqual(a.World.Hash(), b.World.Hash());
        }

        /// <summary>
        /// The ordering fault this commit fixes: the slots are filled while the world is built, the definitions are
        /// applied afterwards, so a chosen unit that only a definition knows would have been deployable for nothing
        /// with no hit points. Spare is an id no faction fields.
        /// </summary>
        [Test]
        public void AChosenUnitThatOnlyADefinitionKnowsArrivesWithItsRealNumbers()
        {
            const byte Spare = 21;
            var cfg = Config();
            cfg.LoadoutA = Ten(Spare);
            using var m = MatchSim.CreateGreybox(cfg);

            Assert.AreEqual(0, m.World.Roster[0].Cost, "nothing defines it yet, so the slot is empty");
            Assert.AreEqual(0, m.World.SlotUnlocked[0], "and an empty slot is LOCKED: a cost of nothing passes the silver check");

            UnitDefinitions.Apply(m.World, new[] { new UnitDef
            {
                Archetype = Spare,
                Roster = new RosterEntry { Archetype = Spare, Cost = 175, Hp = 130f, Speed = 3.1f, CooldownTicks = 240 },
                Infantry = new InfantrySpec { Group = OrderGroup.Line },
                Weapon = new WeaponStats { Id = Spare, Damage = 12f, RangeMax = 48f, RoundsPerSecond = 1.5f, Accuracy = 0.55f },
            } });

            Assert.AreEqual(Spare, m.World.Roster[0].Archetype, "the chosen slot names it");
            Assert.AreEqual(175, m.World.Roster[0].Cost, "and it costs what the definition says, not nothing");
            Assert.AreEqual(130f, m.World.Roster[0].Hp, "and it can take what the definition says");
            Assert.AreEqual(1, m.World.SlotUnlocked[0], "and the slot opens, because a definition gave it real numbers");
        }


        /// <summary>
        /// A slot naming a unit nothing defines would otherwise hold a zeroed line, and Deploy checks the lock, the
        /// cooldown and the silver - a cost of nothing always passes the silver check. So it is locked, and would
        /// otherwise field a free man with no hit points.
        /// </summary>
        [Test]
        public void ASlotNamingNobodyCannotBeDeployed()
        {
            var cfg = Config();
            cfg.LoadoutA = Ten(InfantryArchetype.Rifle, 40);   // 40 is inside the tables and nothing occupies it
            using var m = MatchSim.CreateGreybox(cfg);

            Assert.AreEqual(1, m.World.SlotUnlocked[0], "slot 0 is a rifleman and open");
            Assert.AreEqual(0, m.World.SlotUnlocked[1], "slot 1 is nobody and shut");

            int before = m.World.AliveCount;
            var cmds = new NativeArray<SimCommand>(1, Allocator.Temp);
            cmds[0] = SimCommand.Deploy(m.World.Tick, 0, 1);
            m.Step(cmds);
            cmds.Dispose();
            Assert.AreEqual(before, m.World.AliveCount, "nothing came out of the shut slot");
            Assert.AreEqual(cfg.StartingSilver, m.World.Silver[0], "and it cost nothing because it never happened");
        }

        /// <summary>
        /// The ten travel in a FixedList32Bytes, which holds a fixed number of bytes. If the roster ever grows past
        /// that, a loadout would be silently truncated on the way into the config rather than failing.
        /// </summary>
        [Test]
        public void TheChosenTenFitInWhatCarriesThem()
        {
            var l = new FixedList32Bytes<byte>();
            Assert.LessOrEqual(RosterEntry.SlotCount, l.Capacity,
                $"RosterEntry.SlotCount is {RosterEntry.SlotCount} and a FixedList32Bytes holds {l.Capacity}: a bigger roster needs a bigger list");
        }
        // ---- the replay header ----------------------------------------------------------------------------------
        [Test]
        public void TheReplayHeaderCarriesTheChosenTen()
        {
            var cfg = Config();
            cfg.LoadoutA = Odd();
            cfg.LoadoutB = Ten(VehicleArchetype.Breaker, InfantryArchetype.Sniper);
            var back = ReplayPlayer.Parse(new ReplayRecorder(cfg, default, 1).Serialize());

            Assert.AreEqual(6, ReplayRecorder.FormatVersion, "a header that carries the loadout is v6");
            Assert.AreEqual(RosterEntry.SlotCount, back.Config.LoadoutA.Length);
            var chosen = Odd();
            for (int s = 0; s < RosterEntry.SlotCount; s++) Assert.AreEqual(chosen[s], back.Config.LoadoutA[s], $"slot {s}");
            Assert.AreEqual(2, back.Config.LoadoutB.Length);
            Assert.AreEqual(VehicleArchetype.Breaker, back.Config.LoadoutB[0]);
        }

        /// <summary>A replay that chose nothing still round-trips as choosing nothing, not as an empty ten.</summary>
        [Test]
        public void AReplayOfADefaultBattleCarriesNoChoice()
        {
            var back = ReplayPlayer.Parse(new ReplayRecorder(Config(), default, 1).Serialize());
            Assert.AreEqual(0, back.Config.LoadoutA.Length);
            Assert.AreEqual(0, back.Config.LoadoutB.Length);
        }

        /// <summary>
        /// A v5 replay has no loadout in its header, so reading one as v6 would take the map's bytes for a roster.
        /// It must be refused, not guessed at.
        /// </summary>
        [Test]
        public void AReplayFromBeforeTheChoiceIsRefused()
        {
            var bytes = new ReplayRecorder(Config(), default, 1).Serialize();
            bytes[4] = 5; bytes[5] = 0;   // the version, straight after the four magic bytes
            var e = Assert.Throws<InvalidDataException>(() => ReplayPlayer.Parse(bytes));
            StringAssert.Contains("v5", e.Message);
        }
    }
}
