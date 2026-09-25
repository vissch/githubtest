// Phase: A5c (implemented 2026-09-25) — a machine's kind is a field in the table, so a new machine may take any id.
//
// What this replaced: `IsWalker(a) => a >= Pincer && a <= Redoubt`. Every new walker had to be given an id inside
// that band or be silently treated as a tank by the gunnery, the modules, the animator and the renderer — and the
// band was full. These tests hold the two halves of the fix: the table says what the shipped machines are, and a
// machine defined at an id nowhere near the band is still a walker everywhere.
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class ChassisTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreateGreybox(cfg);
        }

        /// <summary>An id past the old walker band (6..11) and past every shipped machine.</summary>
        const byte FarWalker = 30;

        [Test]
        public void EveryShippedMachineDeclaresWhatItStandsOn()
        {
            using var m = NewMatch();
            foreach (byte a in new byte[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker })
                Assert.AreEqual(ChassisKind.Tracked, m.World.ChassisOf(a), $"archetype {a} runs on tracks");
            foreach (byte a in new byte[] { VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer,
                                            VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt })
                Assert.AreEqual(ChassisKind.Legged, m.World.ChassisOf(a), $"archetype {a} walks");
            foreach (byte a in new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Machinegunner, InfantryArchetype.Officer,
                                            InfantryArchetype.Medic, InfantryArchetype.Jetpack })
                Assert.AreEqual(ChassisKind.Foot, m.World.ChassisOf(a), $"archetype {a} is a man");
        }

        /// <summary>The table and the flag agree: nothing on legs or tracks is missing IsVehicle, and no man has it.</summary>
        [Test]
        public void ThePlaceAUnitStandsAndTheVehicleFlagNeverDisagree()
        {
            using var m = NewMatch();
            for (int a = 0; a < Archetypes.Count; a++)
            {
                var e = m.World.Units.Roster[a];
                if (e.Cost == 0 && e.Hp == 0f) continue;   // an id nothing defines
                Assert.AreEqual(e.IsVehicle, ChassisKind.IsArmoured(e.Chassis), $"archetype {a}: IsVehicle and chassis disagree");
            }
        }

        /// <summary>
        /// The point of the whole change: a walker defined at id 30 — nowhere near Pincer..Redoubt — is armoured, is a
        /// walker and is not a tank, everywhere that used to ask the id range.
        /// </summary>
        [Test]
        public void AWalkerDefinedFarOutsideTheOldBandIsStillAWalker()
        {
            using var m = NewMatch();
            Assert.AreEqual(ChassisKind.Foot, m.World.ChassisOf(FarWalker), "nothing defines it yet");

            UnitDefinitions.Apply(m.World, new[] { new UnitDef
            {
                Archetype = FarWalker,
                Roster = new RosterEntry { Archetype = FarWalker, Cost = 400, Hp = 2400f, Speed = 2.7f, CooldownTicks = 600, IsVehicle = true, Chassis = ChassisKind.Legged },
                Weapon = new WeaponStats { Id = FarWalker, Damage = 30f, RangeMax = 60f, RoundsPerSecond = 1f, Accuracy = 0.6f, PenetrationMm = 30f },
                Machine = new TankSpec { Hull = new ArmorProfile { FrontMm = 30f, SideMm = 18f, RearMm = 12f, TopMm = 8f }, Crew = 0, Unmanned = true },
                Drive = new VehicleProfile { TurnRateRad = 1.1f, HalfLength = 2.4f, HalfWidth = 1.6f, Legs = 6 },
            } });

            byte chassis = m.World.ChassisOf(FarWalker);
            Assert.AreEqual(ChassisKind.Legged, chassis);
            Assert.IsTrue(ChassisKind.IsWalker(chassis), "it walks");
            Assert.IsTrue(ChassisKind.IsArmoured(chassis), "the modules and the gunnery run for it");
            Assert.IsFalse(ChassisKind.IsTank(chassis), "it is not braced like a tank when it shoots");
        }

        /// <summary>A man is never armoured, whatever else a definition says about him.</summary>
        [Test]
        public void FootIsTheOnlyKindThatIsNotAMachine()
        {
            Assert.IsFalse(ChassisKind.IsArmoured(ChassisKind.Foot));
            Assert.IsTrue(ChassisKind.IsArmoured(ChassisKind.Tracked));
            Assert.IsTrue(ChassisKind.IsArmoured(ChassisKind.Legged));
            Assert.IsTrue(ChassisKind.IsArmoured(ChassisKind.Wheeled), "an armoured car has armour (phase 6b)");
        }
    }
}
