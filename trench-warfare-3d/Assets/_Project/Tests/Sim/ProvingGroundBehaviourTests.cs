// Phase: the Proving Ground (2026-09-28) — the three behaviours its data-only stand-ins needed from the sim: a machine
// with a healer's spec heals (the Mercy ambulance), a man whose small arms hunt armour fires armour-piercing at a plate
// they beat (the AT rifle), and a man whose spec says NeverPinned is never pinned (the Death Battalion). Each test would
// fail on the sim as it was: SupportSystem skipped every vehicle, TargetAcquisition and DirectFire read HuntsArmour only
// for a vehicle shooter, and nothing read NeverPinned.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class ProvingGroundBehaviourTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                each?.Invoke(m.World);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        /// <summary>One unit standing still where it is put, with its line from the match's table (hp may be overridden).</summary>
        static int Still(MatchSim m, byte team, byte archetype, float x, float z, float hp = -1f)
        {
            var e = m.World.Units.Roster[archetype];
            Assert.Greater(e.Hp, 0f, $"archetype {archetype} has no line in the match's table");
            return m.World.Spawn(team, archetype, new float3(x, 0f, z), hp < 0f ? e.Hp : hp, 0f, ChassisKind.IsArmoured(m.World.ChassisOf(archetype)));
        }

        /// <summary>Take every other unit of a team off the field, so a mark is the one the test put there.</summary>
        static void Clear(MatchSim m, byte team, params int[] keep)
        {
            for (int i = 0; i < m.World.HighWater; i++)
                if (System.Array.IndexOf(keep, i) < 0 && m.World.IsAlive(i) && m.World.Team[i] == team) m.World.Despawn(i, -1, default);
        }

        // ---------------------------------------------------------------------------------------------- the ambulance
        [Test]
        public void AnAmbulanceHealsTheMenBesideItAndNobodyElse()
        {
            // the patients are medics: nobody here carries a rifle, so nobody is shot before the assertion
            using var m = NewMatch();
            var w = m.World;
            int mercy = Still(m, 0, VehicleArchetype.Mercy, 60f, 30f);
            float reach = w.Units.Infantry[VehicleArchetype.Mercy].HealRadius + m.Vehicles.Profiles[VehicleArchetype.Mercy].HalfLength;
            int near = Still(m, 0, InfantryArchetype.Medic, 60f + reach - 1f, 30f);
            int far = Still(m, 0, InfantryArchetype.Medic, 60f + reach + 25f, 30f);
            int foe = Still(m, 1, InfantryArchetype.Medic, 60f - reach + 1f, 30f);
            int tank = Still(m, 0, VehicleArchetype.Mercy, 60f, 70f);   // a second ambulance, 40 m off: unarmed, so nobody is shot
            float full = w.MaxHp[near];
            w.Hp[near] = 20f; w.Hp[far] = 20f; w.Hp[foe] = 20f; w.Hp[tank] = 500f; w.Hp[mercy] = 500f;

            var log = Run(m, 40);   // 2 s at 25 a second
            Assert.That(w.Hp[near], Is.EqualTo(70f).Within(1.5f), "25 a second, one patient");
            Assert.Greater(Count(log, SimEventType.UnitHealed, mercy, near), 5, "the ambulance is the healer the event names");
            log.AddRange(Run(m, 60));
            Assert.AreEqual(full, w.Hp[near], "never past full");
            Assert.AreEqual(20f, w.Hp[far], "out of its reach");
            Assert.AreEqual(20f, w.Hp[foe], "not the other side");
            Assert.AreEqual(500f, w.Hp[tank], "and nothing for a machine");
            Assert.AreEqual(500f, w.Hp[mercy], "itself included");
            Assert.IsTrue(w.IsAlive(mercy));
        }

        [Test]
        public void AKnockedOutAmbulanceHealsNobodyAndOtherMachinesNeverDid()
        {
            using var m = NewMatch();
            var w = m.World;
            int mercy = Still(m, 0, VehicleArchetype.Mercy, 60f, 30f);
            int maw = Still(m, 0, VehicleArchetype.Maw, 90f, 30f);
            int a = Still(m, 0, InfantryArchetype.Medic, 64f, 30f);
            int b = Still(m, 0, InfantryArchetype.Medic, 97f, 30f);
            w.Hp[a] = 20f; w.Hp[b] = 20f;
            w.Flags[mercy] |= (uint)UnitFlags.KnockedOut;
            var log = Run(m, 60);
            Assert.AreEqual(20f, w.Hp[a], "a wreck patches nobody");
            Assert.AreEqual(20f, w.Hp[b], "a Maw is no ambulance");
            Assert.AreEqual(0, Count(log, SimEventType.UnitHealed, mercy));
            Assert.AreEqual(0, Count(log, SimEventType.UnitHealed, maw));
        }

        // ------------------------------------------------------------------------------------------------ the AT rifle
        /// <summary>His 20 mm takes a Tusk (16 mm at its thickest) as his mark from 25 m and holes it; a Redoubt's 38 mm
        /// front is no mark for him; and a rifleman beside him, who hunts nothing, looks at neither.</summary>
        [Test]
        public void HeHolesATuskFromRifleRangeAndLeavesARedoubtsFrontAlone()
        {
            using var m = NewMatch();
            var w = m.World;
            int rifle = Still(m, 1, InfantryArchetype.AtRifle, 40f, 150f, 1e6f);
            int plain = Still(m, 1, InfantryArchetype.Rifle, 43f, 150f, 1e6f);
            int tusk = Still(m, 0, VehicleArchetype.Tusk, 40f, 125f);
            w.Yaw[tusk] = SimMath.HalfPi;   // its side to him
            Clear(m, 0, tusk);
            Run(m, 6);
            Assert.AreEqual(tusk, w.TargetSlot[rifle], "the Tusk is the anti-tank rifle's mark");
            Assert.AreNotEqual(tusk, w.TargetSlot[plain], "and not a rifleman's: 25 m is no close assault");
            float before = w.Hp[tusk];
            var log = Run(m, 20 * 30);
            int shots = Count(log, SimEventType.Shot, rifle, tusk);
            bool out_ = !w.IsAlive(tusk) || (w.Flags[tusk] & (uint)UnitFlags.KnockedOut) != 0;
            Assert.Greater(shots, 0, "he fires at it");
            foreach (var e in log) if (e.Type == SimEventType.Shot && e.A == rifle && e.B == tusk) Assert.AreEqual(0f, e.Scalar, "a rifle shot, not a bundle of grenades (scalar 1)");
            Assert.IsTrue(shots > 2 || out_, $"one round every five seconds until it is out of the fight: {shots} shots, hp {w.Hp[tusk]} of {before}, flags 0x{w.Flags[tusk]:X}");
            Assert.IsTrue(w.Hp[tusk] < before || out_, "and they go through");

            using var m2 = NewMatch();
            int rifle2 = Still(m2, 1, InfantryArchetype.AtRifle, 40f, 150f, 1e6f);
            int redoubt = Still(m2, 0, VehicleArchetype.Redoubt, 40f, 125f);
            m2.World.Yaw[redoubt] = 0f;   // its front to him
            Clear(m2, 0, redoubt);
            Run(m2, 6);
            Assert.AreNotEqual(redoubt, m2.World.TargetSlot[rifle2], "38 mm of front is not a mark for 20 mm");
        }

        /// <summary>Beside a machine he cannot pierce he is a man with a bundle of grenades, like any other.</summary>
        [Test]
        public void BesideAPlateHeCannotBeatHeCloseAssaultsIt()
        {
            using var m = NewMatch();
            var w = m.World;
            int rifle = Still(m, 1, InfantryArchetype.AtRifle, 40f, 150f, 1e6f);
            int redoubt = Still(m, 0, VehicleArchetype.Redoubt, 40f, 150f - (CombatTables.CloseAssaultRange - 1f));
            w.Yaw[redoubt] = 0f;
            Clear(m, 0, redoubt);
            var log = Run(m, 100);
            int bundles = 0;
            foreach (var e in log) if (e.Type == SimEventType.Shot && e.A == rifle && e.B == redoubt && e.Scalar == 1f) bundles++;
            Assert.Greater(bundles, 0, "a close assault is a Shot with scalar 1");
        }

        // ------------------------------------------------------------------------------------------ the Death Battalion
        /// <summary>Two men under the same machine guns, thirty metres off, a hundred metres apart: the rifleman is pinned,
        /// the Death Battalion's never ends a tick at or above the threshold.</summary>
        [Test]
        public void ADeathBattalionRiflemanNeverStaysPinned()
        {
            using var m = NewMatch();
            var w = m.World;
            Clear(m, 0); Clear(m, 1);
            int elite = Still(m, 0, InfantryArchetype.DeathBattalion, 60f, 200f, 1e6f);
            int plain = Still(m, 0, InfantryArchetype.Rifle, 160f, 200f, 1e6f);
            for (int k = 0; k < 3; k++)
            {
                Still(m, 1, InfantryArchetype.Machinegunner, 56f + k * 4f, 230f, 1e6f);
                Still(m, 1, InfantryArchetype.Machinegunner, 156f + k * 4f, 230f, 1e6f);
            }
            float worstElite = 0f, worstPlain = 0f;
            Run(m, 400, world =>
            {
                worstElite = math.max(worstElite, world.Suppression[elite]);
                worstPlain = math.max(worstPlain, world.Suppression[plain]);
            });
            Assert.GreaterOrEqual(worstPlain, SuppressionRules.PinnedThreshold, "setup: three machine guns pin a rifleman");
            Assert.Greater(worstElite, SuppressionRules.ProneThreshold, "setup: the elite man is under the same fire");
            Assert.Less(worstElite, SuppressionRules.PinnedThreshold, "and never pinned by it");
            Assert.AreEqual(AuraSystem.UnpinTo, worstElite, 0.001f, "held where the officer's aura holds his men");
        }

        // -------------------------------------------------------------------------------------------------- determinism
        [Test]
        public void TheThreeAreTheSameOnTwoWorlds()
        {
            ulong[] Play()
            {
                using var m = NewMatch();
                var w = m.World;
                w.HashInterval = 1;
                int mercy = Still(m, 0, VehicleArchetype.Mercy, 60f, 120f);
                int hurt = Still(m, 0, InfantryArchetype.Medic, 64f, 120f); w.Hp[hurt] = 20f;
                Still(m, 1, InfantryArchetype.AtRifle, 40f, 150f, 5000f);
                int tusk = Still(m, 0, VehicleArchetype.Tusk, 40f, 125f); w.Yaw[tusk] = SimMath.HalfPi;
                Still(m, 0, InfantryArchetype.DeathBattalion, 200f, 200f, 5000f);
                for (int k = 0; k < 3; k++) Still(m, 1, InfantryArchetype.Machinegunner, 196f + k * 4f, 230f, 5000f);
                Assert.IsTrue(w.IsAlive(mercy));
                var hashes = new ulong[300];
                for (int t = 0; t < hashes.Length; t++) { Step(m); hashes[t] = w.LastHash; }
                return hashes;
            }
            var one = Play(); var two = Play();
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"the worlds part at tick {t}");
        }
    }
}
