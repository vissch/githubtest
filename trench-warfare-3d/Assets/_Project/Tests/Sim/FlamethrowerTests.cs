// Phase: A5 (2026-09-28) — the flamethrower in the sim: a hit by a weapon whose WeaponStats.SetsBurning is set lights
// the man it hits and the ground under him the tick of the shot, a miss lights the ground (and the man catches from
// it), a shield bearer's plate does not stop it, a rifle lights nothing, and it is the same on two worlds. The burning
// itself (what a man alight loses, where he runs) is BurningSystemTests'.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class FlamethrowerTests
    {
        static readonly float3 Open = new float3(150f, 0f, 240f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly List<SimEvent> Log = new List<SimEvent>();
            public List<SimEvent> Tick = new List<SimEvent>();
            public SimWorld W => M.World;
            public Rig(uint seed = 0xC0FFEE)
            {
                var cfg = SimConfig.Default; cfg.Seed = seed;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.HashInterval = 1;
            }
            /// <summary>A man who stands still where he is put, with the hit points given (his weapon is his archetype's).</summary>
            public int Man(byte archetype, float3 at, byte team, float hp)
            {
                Assert.Greater(W.Units.Roster[archetype].Hp, 0f, $"archetype {archetype} has no line in the match's table");
                return W.Spawn(team, archetype, at, hp, 0f, false);
            }
            public void Step()
            {
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                M.Step(none);
                Tick = new List<SimEvent>();
                var ev = W.Events.Events;
                for (int k = 0; k < ev.Length; k++) { Log.Add(ev[k]); Tick.Add(ev[k]); }
            }
            public bool RunUntil(int maxTicks, Func<bool> done)
            {
                for (int t = 0; t < maxTicks; t++) { Step(); if (done()) return true; }
                return false;
            }
            public static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
            {
                int n = 0;
                foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
                return n;
            }
            public bool Burning(int slot) => (W.Flags[slot] & (uint)UnitFlags.Burning) != 0;
            public void Dispose() => M.Dispose();
        }

        [Test]
        public void AFlameHitSetsTheTargetAlightAndTheGroundUnderHim()
        {
            using var r = new Rig();
            int flame = r.Man(InfantryArchetype.Flamethrower, Open, 0, 1e6f);
            int target = r.Man(InfantryArchetype.Medic, Open + new float3(0f, 0f, 6f), 1, 1000f);   // unarmed: nothing else happens
            Assert.IsTrue(r.RunUntil(200, () => Rig.Count(r.Tick, SimEventType.Hit, flame, target) > 0), "setup: the jet reaches a man 6 m off");
            Assert.IsTrue(r.Burning(target), "the man it hit is alight the tick of the shot");
            Assert.AreEqual(1, Rig.Count(r.Log, SimEventType.UnitAlight, target, 1), "UnitAlight (a = slot, b = 1)");
            Assert.Greater(r.M.Burning.AlightUntil[target], r.W.Tick);
            Assert.GreaterOrEqual(Rig.Count(r.Log, SimEventType.CellBurning), 1, "and the ground under him");
            bool under = false;
            for (int k = 0; k < r.M.Burning.Cells.Length; k++) under |= r.M.Burning.Cells[k].Cell == r.M.Burning.CellOf(r.W.Position[target]) && r.M.Burning.Cells[k].Player == 0;
            Assert.IsTrue(under, "his own nav cell, lit by the shooter's side");

            // he burns on top of what the jet does to him: more is lost than the hits account for
            float hp0 = r.W.Hp[target]; int mark = r.Log.Count;
            for (int t = 0; t < 60; t++) r.Step();
            float fromHits = 0f;
            for (int k = mark; k < r.Log.Count; k++) if (r.Log[k].Type == SimEventType.Hit && r.Log[k].B == target && r.Log[k].Scalar > 0f) fromHits += r.Log[k].Scalar;
            Assert.Greater(hp0 - r.W.Hp[target], fromHits + 0.5f * BurningSystem.BurnDps, "burning costs hit points too (12 a second)");
            Assert.IsFalse(r.Burning(flame), "the man with the flamethrower is not alight");
        }

        /// <summary>A round that misses lights the ground where it went; the man standing there catches from the ground,
        /// for the shorter time that gives. Seeds are tried until one's first shot is a miss: the rule, not the dice.</summary>
        [Test]
        public void AMissLightsTheGroundAndTheManCatchesFromIt()
        {
            for (uint seed = 1; seed <= 400; seed++)
            {
                using var r = new Rig(seed);
                int flame = r.Man(InfantryArchetype.Flamethrower, Open, 0, 1e6f);
                int target = r.Man(InfantryArchetype.Medic, Open + new float3(0f, 0f, 9f), 1, 1000f);
                if (!r.RunUntil(100, () => Rig.Count(r.Tick, SimEventType.Shot, flame, target) > 0)) continue;
                if (Rig.Count(r.Tick, SimEventType.Hit, flame, target) > 0) continue;   // a hit: try the next seed
                Assert.AreEqual(1, Rig.Count(r.Tick, SimEventType.CellBurning), $"seed {seed}: the miss lit one cell");
                SimEvent alight = default; bool found = false;
                foreach (var e in r.Tick) if (e.Type == SimEventType.UnitAlight && e.A == target && e.B == 1) { alight = e; found = true; }
                Assert.IsTrue(found, $"seed {seed}: he stands on burning ground and catches");
                Assert.LessOrEqual(alight.Scalar, BurningSystem.GroundCatchSeconds + 0.01f, "from the ground, not from the jet");
                return;
            }
            Assert.Fail("no seed in 400 gave a first shot that missed: the odds changed, pick the range again");
        }

        /// <summary>A shield bearer's 8 mm plate stops every round of a weapon with no penetration. Not this one.</summary>
        [Test]
        public void AShieldPlateDoesNotStopFire()
        {
            using var r = new Rig();
            int flame = r.Man(InfantryArchetype.Flamethrower, Open, 0, 1e6f);
            int bearer = r.Man(InfantryArchetype.Shield, Open + new float3(0f, 0f, 6f), 1, 1e5f);
            r.W.Yaw[bearer] = SimMath.Pi;   // facing the jet: the plate is between them
            Assert.IsTrue(r.RunUntil(200, () => Rig.Count(r.Log, SimEventType.Shot, flame, bearer) >= 8), "setup: eight bursts at him");
            Assert.AreEqual(0, Rig.Count(r.Log, SimEventType.ShieldBlocked, bearer, flame), "the plate stopped none of them");
            Assert.Greater(Rig.Count(r.Log, SimEventType.Hit, flame, bearer), 0, "they hit him");
            Assert.IsTrue(r.Burning(bearer), "and he is alight behind his plate");
        }

        [Test]
        public void ARifleHitLightsNobody()
        {
            using var r = new Rig();
            int rifle = r.Man(InfantryArchetype.Rifle, Open, 0, 1e6f);
            int target = r.Man(InfantryArchetype.Medic, Open + new float3(0f, 0f, 6f), 1, 1e5f);
            Assert.IsTrue(r.RunUntil(400, () => Rig.Count(r.Log, SimEventType.Hit, rifle, target) >= 3), "setup: the rifle hits him");
            Assert.AreEqual(0, Rig.Count(r.Log, SimEventType.UnitAlight));
            Assert.AreEqual(0, Rig.Count(r.Log, SimEventType.CellBurning));
            Assert.IsFalse(r.Burning(target));
            Assert.AreEqual(0, r.M.Burning.Cells.Length);
        }

        /// <summary>A man the jet kills outright is not set alight: his slot's next tenant must not inherit a fire.</summary>
        [Test]
        public void AManTheJetKillsIsDeadNotAlightAndHisSlotStartsClean()
        {
            using var r = new Rig();
            int flame = r.Man(InfantryArchetype.Flamethrower, Open, 0, 1e6f);
            int target = r.Man(InfantryArchetype.Medic, Open + new float3(0f, 0f, 6f), 1, 1f);
            Assert.IsTrue(r.RunUntil(200, () => !r.W.IsAlive(target)), "setup: one hit kills a man with one hit point");
            Assert.AreEqual(0, Rig.Count(r.Log, SimEventType.UnitAlight, target, 1), "he died of the hit");
            int next = r.Man(InfantryArchetype.Medic, Open + new float3(60f, 0f, 60f), 1, 100f);
            Assert.AreEqual(target, next, "setup: the slot is re-used");
            r.Step();
            Assert.IsFalse(r.Burning(next), "the next man is not alight");
        }

        [Test]
        public void ItIsTheSameOnTwoWorlds()
        {
            ulong[] Play()
            {
                using var r = new Rig();
                r.Man(InfantryArchetype.Flamethrower, Open, 0, 5000f);
                r.Man(InfantryArchetype.Flamethrower, Open + new float3(30f, 0f, 8f), 1, 5000f);
                for (int k = 0; k < 4; k++) r.Man(InfantryArchetype.Rifle, Open + new float3(-4f + k * 3f, 0f, 7f), 1, 300f);
                for (int k = 0; k < 4; k++) r.Man(InfantryArchetype.Shield, Open + new float3(26f + k * 3f, 0f, 0f), 0, 300f);
                var hashes = new ulong[300];
                for (int t = 0; t < hashes.Length; t++) { r.Step(); hashes[t] = r.W.LastHash; }
                Assert.Greater(Rig.Count(r.Log, SimEventType.UnitAlight, int.MinValue, 1), 0, "men were set alight inside the window");
                return hashes;
            }
            var one = Play(); var two = Play();
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"the worlds part at tick {t}");
        }
    }
}
