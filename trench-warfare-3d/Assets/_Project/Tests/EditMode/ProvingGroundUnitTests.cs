// Phase: the Proving Ground (2026-09-28) — the sixteen units the test level fields and no faction does: the asset
// playground's prototypes (Brute, Croaker, Hopper, Mercy, Frog) and docs/06's ideas as stand-ins on the sim's existing
// specs (Sentry, AT rifle, Death Battalion, the historical armour, the Sapper, the Flamethrower). Like DefinedUnitTests
// for the Skimmer and the Salvo: each is in the match's table with its numbers, in no roster or pool, may be named in a
// loadout, drives and fights on what it was given, and the same on two worlds. Plus SimConfig.Endless, the level's rule
// that a match never ends.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Presentation;

namespace TW.Tests
{
    public class ProvingGroundUnitTests
    {
        static byte[] New => UnitDefinitions.ProvingGround;

        static MatchSim NewMatch(bool endless = false)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Endless = endless;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static void RunUntil(MatchSim m, int maxTicks, System.Func<bool> done)
        {
            for (int t = 0; t < maxTicks && !done(); t++) Step(m);
        }

        static int Count(List<SimEvent> log, SimEventType type, int a)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && e.A == a) n++;
            return n;
        }

        static UnitDef Def(byte a)
        {
            foreach (var d in UnitDefinitions.All) if (d.Archetype == a) return d;
            Assert.Fail($"archetype {a} is not in UnitDefinitions.All"); return default;
        }

        /// <summary>Spawn one the way TankCapture.Spawn (and so the Unit Sandbox) does: its line from the match's table.</summary>
        static int Spawn(MatchSim m, byte team, byte archetype, float3 at, float speed = -1f)
        {
            var e = m.World.Units.Roster[archetype];
            Assert.Greater(e.Hp, 0f, $"archetype {archetype} has no line in the match's table");
            return m.World.Spawn(team, archetype, at, e.Hp, speed < 0f ? e.Speed : speed, ChassisKind.IsArmoured(m.World.ChassisOf(archetype)));
        }

        [Test]
        public void TheFileNamesSixteenAndTheListIsInIdOrder()
        {
            Assert.AreEqual(16, New.Length);
            for (int k = 1; k < New.Length; k++) Assert.Greater(New[k], New[k - 1], "in id order, so a reader can find one");
            foreach (byte a in New) Def(a);
            Assert.Less(New[New.Length - 1], Archetypes.Count);
        }

        [Test]
        public void EveryProvingGroundUnitIsInTheMatchTableWithItsNumbers()
        {
            using var m = NewMatch();
            var w = m.World;
            foreach (byte a in New)
            {
                var d = Def(a);
                var r = w.Units.Roster[a];
                Assert.AreEqual(a, r.Archetype, $"archetype {a}");
                Assert.AreEqual(d.Roster.Cost, r.Cost, $"archetype {a} cost"); Assert.AreEqual(d.Roster.Hp, r.Hp, $"archetype {a} hp");
                Assert.AreEqual(d.Roster.Speed, r.Speed, $"archetype {a} speed"); Assert.AreEqual(d.Roster.CooldownTicks, r.CooldownTicks, $"archetype {a} cooldown");
                Assert.AreEqual(d.Roster.IsVehicle, r.IsVehicle, $"archetype {a} vehicle flag"); Assert.AreEqual(d.Roster.Chassis, w.ChassisOf(a), $"archetype {a} chassis");
                Assert.AreEqual(a, m.Catalogue.Weapon[a].Id, $"archetype {a}: its weapon carries its own id");
                Assert.AreEqual(d.Infantry.Group, w.Units.Infantry[a].Group, $"archetype {a}: its order group");
                Assert.AreNotEqual(0, w.Units.Infantry[a].Group, $"archetype {a}: an order must reach it");
                if (r.IsVehicle)
                {
                    Assert.GreaterOrEqual(m.Catalogue.Tank[a].Crew, 2, $"archetype {a}: a one-man crew drives at 0.6");
                    Assert.Greater(m.Vehicles.Profiles[a].HalfLength, 0f, $"archetype {a}: a footprint");
                    Assert.Greater(m.Vehicles.Profiles[a].HalfWidth, 0f, $"archetype {a}: a footprint");
                }
            }
            // the numbers the level's cards will read, spot-checked against docs/06
            Assert.AreEqual(20f, m.Catalogue.Weapon[InfantryArchetype.AtRifle].PenetrationMm, "an anti-tank rifle");
            Assert.IsTrue(w.Units.Infantry[InfantryArchetype.AtRifle].HuntsArmour);
            Assert.AreEqual(8f, w.Units.Infantry[InfantryArchetype.Sentry].ShieldPlateMm, "a sentry's plate");
            Assert.AreEqual(0f, w.Units.Infantry[InfantryArchetype.Sentry].ShieldGuardRadius, "his own plate, nobody else's");
            Assert.IsTrue(w.Units.Infantry[InfantryArchetype.DeathBattalion].NeverPinned);
            Assert.AreEqual(2, w.Units.Infantry[InfantryArchetype.Sapper].MineCharges);
            Assert.IsTrue(m.Catalogue.Weapon[InfantryArchetype.Flamethrower].SetsBurning);
            Assert.AreEqual(FireMode.Cone, m.Catalogue.Weapon[InfantryArchetype.Flamethrower].Mode);
            Assert.AreEqual(12f, m.Catalogue.Weapon[InfantryArchetype.Flamethrower].RangeMax);
            Assert.AreEqual(25f, w.Units.Infantry[VehicleArchetype.Mercy].HealPerSecond, "an ambulance heals");
            Assert.AreEqual(0f, m.Catalogue.Weapon[VehicleArchetype.Mercy].RangeMax, "and shoots nothing");
            Assert.AreEqual(2, m.Vehicles.Profiles[VehicleArchetype.Croaker].Legs, "two legs");
            Assert.IsTrue(m.Vehicles.Profiles[VehicleArchetype.Hopper].Walker, "the gunship strides everything");
            Assert.AreEqual(0, m.Vehicles.Profiles[VehicleArchetype.Hopper].Legs, "and has no legs to lose");
            Assert.AreEqual(0f, m.Vehicles.Profiles[VehicleArchetype.Hopper].BogChance);
            Assert.AreEqual(30f, m.Catalogue.Tank[VehicleArchetype.A7V].Hull.FrontMm, "the A7V's nose");
            Assert.AreEqual(TankMount.Hull, m.Catalogue.Tank[VehicleArchetype.A7V].Gun0.Mount);
            Assert.AreEqual(TankSpec.Maw.Gun0.RangeMax, m.Catalogue.Tank[VehicleArchetype.MarkIV].Gun0.RangeMax, "the Mark IV is the Maw");
            Assert.AreEqual(0, m.Catalogue.Tank[VehicleArchetype.Whippet].GunCount, "the Whippet has machine guns only");
            Assert.IsTrue(m.Vehicles.Profiles[VehicleArchetype.Austin].Wheeled);
        }

        [Test]
        public void AVehicleFlagAndItsChassisNeverDisagree()
        {
            using var m = NewMatch();
            foreach (byte a in New)
            {
                var e = m.World.Units.Roster[a];
                Assert.AreEqual(e.IsVehicle, ChassisKind.IsArmoured(e.Chassis), $"archetype {a}: IsVehicle and chassis disagree");
                Assert.AreEqual(!e.IsVehicle, InfantryArchetype.IsInfantry(a), $"archetype {a}: IsInfantry and the roster line disagree");
            }
        }

        [Test]
        public void NoFactionFieldsThemAndTheOldSwitchesStayEmpty()
        {
            foreach (byte a in New)
            {
                Assert.AreEqual(0f, RosterEntry.ForArchetype(a).Hp, $"archetype {a} is a shipped unit");
                for (int f = 0; f < Factions.Count; f++)
                {
                    var faction = Factions.Of((byte)f);
                    Assert.IsFalse(FactionRoster.Fields(faction, a), $"{faction} fields archetype {a}");
                    for (int s = 0; s < RosterEntry.SlotCount; s++)
                        Assert.AreNotEqual(a, FactionRoster.Slot(faction, s).Archetype, $"{faction} slot {s} is archetype {a}");
                }
            }
            using var m = NewMatch();
            for (int s = 0; s < m.World.Roster.Length; s++)
                foreach (byte a in New) Assert.AreNotEqual(a, m.World.Roster[s].Archetype, $"the match's roster slot {s} is archetype {a}");
        }

        /// <summary>The level's launch screen puts any of them in the ten: the slot opens and a deploy is taken.</summary>
        [Test]
        public void ALoadoutMayNameThem()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var ten = new FixedList32Bytes<byte>();
            foreach (byte a in new[] { VehicleArchetype.Brute, VehicleArchetype.Croaker, VehicleArchetype.Hopper, VehicleArchetype.Mercy, InfantryArchetype.Frog,
                                       InfantryArchetype.Sentry, InfantryArchetype.AtRifle, InfantryArchetype.DeathBattalion, InfantryArchetype.Sapper, InfantryArchetype.Flamethrower }) ten.Add(a);
            cfg.LoadoutA = ten;
            using var m = MatchSim.CreatePlaytest(cfg);
            var w = m.World;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                Assert.AreEqual(ten[s], w.Roster[s].Archetype, $"slot {s}");
                Assert.AreEqual(1, w.SlotUnlocked[s], $"slot {s} is locked: the definition did not reach the roster");
            }
            for (int s = 0; s < RosterEntry.SlotCount; s++) Step(m, SimCommand.Deploy(w.Tick, 0, s));
            Assert.AreEqual(RosterEntry.SlotCount, w.AliveCount, "every deploy was taken");
            int machines = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) machines++;
            Assert.AreEqual(4, machines, "the four machines came in as machines");
        }

        [Test]
        public void EachMachineDrives()
        {
            foreach (byte a in New)
            {
                if (!Def(a).Roster.IsVehicle) continue;
                using var m = NewMatch();
                var start = new float3(30f, 0f, 200f);
                int s = Spawn(m, 1, a, start);
                Run(m, 120);
                Assert.Greater(math.distance(m.World.Position[s], start), 5f, $"archetype {a} does not drive off towards the enemy");
                Assert.IsTrue(m.World.IsAlive(s), $"archetype {a} died driving on an empty field");
            }
        }

        [Test]
        public void EachArmedUnitFightsAndTheUnarmedDoNot()
        {
            foreach (byte a in New)
            {
                var d = Def(a);
                bool armed = d.Weapon.RangeMax > 0f || d.Machine.GunCount > 0;
                using var m = NewMatch();
                float reach = d.Weapon.RangeMax > 0f ? d.Weapon.RangeMax : d.Machine.Gun0.RangeMax;
                float dist = armed ? math.min(30f, reach * 0.6f) : 30f;
                int s = Spawn(m, 1, a, new float3(30f, 0f, 200f), 0f);
                for (int k = 0; k < 4; k++) m.World.Spawn(0, 0, new float3(27f + k * 2f, 0f, 200f - dist), 1000f, 0f, false);
                var log = Run(m, 400);
                int shots = Count(log, SimEventType.Shot, s) + Count(log, SimEventType.VehicleFired, s);
                if (armed) Assert.Greater(shots, 0, $"archetype {a} never fired at men {dist:0} m off");
                else Assert.AreEqual(0, shots, $"archetype {a} is unarmed and fired");
            }
        }

        /// <summary>SimConfig.Endless: the level's matches never end. The lines still fall, the HQ objective is still
        /// captured and announced, and nobody wins.</summary>
        [Test]
        public void AnEndlessMatchNeverEnds()
        {
            using var m = NewMatch(endless: true);
            var w = m.World;
            int goal = m.Fields.GetGoal(GoalKey.Trench(2));
            float z = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[2].CellStart]).z;
            for (int k = 0; k < 5; k++)
            {
                int s = w.Spawn(0, 0, new float3(130f + k * 3f, 0f, z - 25f), 100f, 3f, false);
                w.GoalId[s] = goal; w.Flags[s] |= (uint)UnitFlags.Exposed;
            }
            RunUntil(m, 900, () => m.Fields.Trenches[2].OwnerTeam == 0);
            Assert.AreEqual(0, m.Fields.Trenches[2].OwnerTeam, "front trench captured");
            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 2 });
            RunUntil(m, 1500, () => m.Fields.Trenches[3].OwnerTeam == 0);
            Assert.AreEqual(0, m.Fields.Trenches[3].OwnerTeam, "reserve trench captured");
            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 3 });
            bool hqTaken = false;
            for (int t = 0; t < 1500 && !hqTaken; t++)
            {
                Step(m);
                var ev = w.Events.Events;
                for (int k = 0; k < ev.Length; k++) if (ev[k].Type == SimEventType.ObjectiveCaptured && ev[k].A == 5) hqTaken = true;
            }
            Assert.IsTrue(hqTaken, "the HQ objective still falls");
            Assert.AreEqual(-1, w.WinnerTeam, "and nobody wins");
            Run(m, 100);
            Assert.AreEqual(-1, w.WinnerTeam);
        }

        [Test]
        public void AnEndlessFlagRidesTheReplayHeader()
        {
            var cfg = SimConfig.Default; cfg.Endless = true;
            var back = ReplayPlayer.Parse(new ReplayRecorder(cfg, default, 1).Serialize());
            Assert.IsTrue(back.Config.Endless);
        }

        /// <summary>The whole set on the field in two worlds, tick for tick.</summary>
        [Test]
        public void ItIsTheSameOnTwoWorlds()
        {
            ulong[] Play()
            {
                using var m = NewMatch();
                m.World.HashInterval = 1;
                int k = 0;
                foreach (byte a in New) { Spawn(m, (byte)(k % 2), a, new float3(20f + (k % 8) * 12f, 0f, k % 2 == 0 ? 150f : 210f)); k++; }
                var hashes = new ulong[300];
                for (int t = 0; t < hashes.Length; t++) { Step(m); hashes[t] = m.World.LastHash; }
                return hashes;
            }
            var one = Play(); var two = Play();
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"the worlds part at tick {t}");
        }

        /// <summary>The Brute's model carries one gun, in the front of its hull, and is 8.8 m long and 5.3 m wide as the
        /// battle draws it: the sim fires that gun and drives that footprint (the A7V wears the same model).</summary>
        [Test]
        public void TheBruteFiresTheOneGunItsModelHasFromTheFootprintItsModelHas()
        {
            using var m = MatchSim.CreatePlaytest(SimConfig.Default);
            var spec = m.Catalogue.Tank[VehicleArchetype.Brute];
            Assert.AreEqual(1, spec.GunCount);
            Assert.AreEqual(TW.Sim.Combat.TankMount.Hull, spec.Gun0.Mount, "in the hull: it turns to bring it on");
            Assert.AreEqual(25f, spec.Gun0.ArcHalf * 180f / SimMath.Pi, 0.01f);
            Assert.AreEqual(0f, spec.Gun0.RestYaw);
            Assert.AreEqual(TW.Sim.Combat.TankMount.None, spec.Gun1.Mount, "no second gun");
            Assert.AreEqual(TW.Sim.Combat.TankSpec.Maw.Gun0.ApDamage, spec.Gun0.ApDamage, "the Maw's six-pounder");
            foreach (byte a in new[] { VehicleArchetype.Brute, VehicleArchetype.A7V })
            {
                var drive = m.Vehicles.Profiles[a];
                Assert.AreEqual(4.4f, drive.HalfLength, 1e-4f, $"archetype {a}");
                Assert.AreEqual(2.7f, drive.HalfWidth, 1e-4f, $"archetype {a}");
            }
        }
    }
}
