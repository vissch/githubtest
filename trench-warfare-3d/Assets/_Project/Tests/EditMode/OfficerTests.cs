// Phase: A3 (implemented 2026-09-25) — the officer: the men within his aura hit a fifth harder and are harder to
// suppress, nobody inside it stays pinned, a dead officer takes it with him, and it is the same on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class OfficerTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
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

        /// <summary>A man who stands still where he is put, in the open.</summary>
        static int Still(MatchSim m, byte team, byte archetype, float x, float z, float hp = 100f)
        {
            var at = new float3(x, 0f, z);
            int cell = m.Map.NavCellOf(at).x + m.Map.NavCellOf(at).y * m.Map.NavWidth;
            Assert.AreEqual(0, m.Map.NavLayers[cell] & (byte)NavLayer.Trench, $"setup: ({x},{z}) must be open ground");
            return m.World.Spawn(team, archetype, at, hp, 0f, false);
        }

        // A firing line of six riflemen 30 m from six unarmed, unkillable dummies (medics with a million hit points
        // never find a target). One line has an officer standing in it, the other has him 60 m away.
        static (int[] near, int[] far, int officer) TwoLines(MatchSim m, float z)
        {
            int officer = Still(m, 0, InfantryArchetype.Officer, 60f, z);
            var near = new int[6]; var far = new int[6];
            for (int k = 0; k < 6; k++)
            {
                near[k] = Still(m, 0, InfantryArchetype.Rifle, 54f + k * 2.4f, z + 1.5f);
                far[k] = Still(m, 0, InfantryArchetype.Rifle, 154f + k * 2.4f, z + 1.5f);
                Still(m, 1, InfantryArchetype.Medic, 54f + k * 2.4f, z + 30f, 1e6f);
                Still(m, 1, InfantryArchetype.Medic, 154f + k * 2.4f, z + 30f, 1e6f);
            }
            return (near, far, officer);
        }

        static float MeanHit(List<SimEvent> log, int[] shooters)
        {
            float sum = 0f; int n = 0;
            foreach (var e in log)
                if (e.Type == SimEventType.Hit && e.Scalar > 0f && System.Array.IndexOf(shooters, e.A) >= 0) { sum += e.Scalar; n++; }
            Assert.Greater(n, 15, "setup: the line must land some hits");
            return sum / n;
        }

        [Test]
        public void TheMenRoundAnOfficerHitAFifthHarder()
        {
            using var m = NewMatch();
            var (near, far, _) = TwoLines(m, 30f);
            var log = Run(m, 600);
            float ratio = MeanHit(log, near) / MeanHit(log, far);
            Assert.That(ratio, Is.EqualTo(1.2f).Within(0.06f), "mean damage per hit, officer's line over the other");
        }

        [Test]
        public void TheAuraIsWrittenEveryTickAndOnlyForHisOwnSideWithinReach()
        {
            using var m = NewMatch();
            var (near, far, officer) = TwoLines(m, 30f);
            int enemy = Still(m, 1, InfantryArchetype.Rifle, 62f, 33f);   // an enemy standing right beside him
            Step(m);
            var aura = m.Aura;
            foreach (var s in near) { Assert.AreEqual(1.2f, aura.DamageMul[s], $"near {s}"); Assert.AreEqual(0.5f, aura.SuppressionMul[s]); Assert.AreEqual(1, aura.Covered[s]); }
            foreach (var s in far) { Assert.AreEqual(1f, aura.DamageMul[s], $"far {s}"); Assert.AreEqual(1f, aura.SuppressionMul[s]); Assert.AreEqual(0, aura.Covered[s]); }
            Assert.AreEqual(1.2f, aura.DamageMul[officer], "he is inside his own ring");
            Assert.AreEqual(1f, aura.DamageMul[enemy], "the other side gets nothing from him");
        }

        [Test]
        public void NobodyInsideTheRingStaysPinned()
        {
            using var m = NewMatch();
            int officer = Still(m, 0, InfantryArchetype.Officer, 60f, 30f);
            int inside = Still(m, 0, InfantryArchetype.Rifle, 65f, 30f);
            int outside = Still(m, 0, InfantryArchetype.Rifle, 120f, 30f);
            for (int t = 0; t < 10; t++)
            {
                m.World.Suppression[inside] = 100f; m.World.Suppression[outside] = 100f;
                Step(m);
            }
            Assert.LessOrEqual(m.World.Suppression[inside], AuraSystem.UnpinTo);
            Assert.AreNotEqual((byte)Stance.Pinned, m.World.StanceOf[inside], "he keeps his feet");
            Assert.GreaterOrEqual(m.World.Suppression[outside], SuppressionRules.PinnedThreshold);
            Assert.AreEqual((byte)Stance.Pinned, m.World.StanceOf[outside], "sixty metres off, he is pinned like anyone");
            // a pinned officer covers nobody
            m.World.Suppression[officer] = 100f; m.World.Suppression[inside] = 100f;
            Step(m);
            Assert.AreEqual(0, m.Aura.Covered[inside]);
        }

        [Test]
        public void ADeadOfficerTakesHisAuraWithHim()
        {
            using var m = NewMatch();
            var (near, _, officer) = TwoLines(m, 30f);
            Step(m);
            Assert.AreEqual(1.2f, m.Aura.DamageMul[near[0]]);
            m.World.Despawn(officer);
            Step(m);
            Assert.AreEqual(1f, m.Aura.DamageMul[near[0]]);
            Assert.AreEqual(1f, m.Aura.SuppressionMul[near[0]]);
        }

        [Test]
        public void TheOfficerIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                TwoLines(m, 30f);
                for (int k = 0; k < 8; k++) Still(m, 1, InfantryArchetype.Rifle, 50f + k * 3f, 70f);   // an enemy line that shoots back
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
