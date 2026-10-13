// Phase: A5 (2026-09-24) — a burst is no longer a circle of equal harm.
// Owner: directional explosions, look AND damage. What is tested here is the damage half: the far side of a burst
// takes more than the side the shell came from, a trench gives LIMITED protection in the bay it lands in and more
// than that further along, a shell that goes off down in a trench barely reaches the open field, broken ground
// shadows what is behind it, and the cost of asking the ground that question is bounded.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class DirectionalBlastTests
    {
        const float Hp0 = 1000f;   // far more than a shell does, so every man lives and his damage can be read off

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }
            /// <summary>A man standing still where he is put, tough enough to survive anything here.</summary>
            public int Man(float3 at) => M.World.Spawn(0, 0, at, Hp0, 0f, false);
            public float Took(int slot) => Hp0 - M.World.Hp[slot];
            public void Fire(Impact im)
            {
                M.Blast.Queue(im);
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                M.Step(none);
            }
            public void Dispose() => M.Dispose();
        }

        static Impact Shell(float3 at, float radius, float damage, float3 dir = default) => new Impact
        {
            Pos = at, Radius = radius, Damage = damage, Suppression = 40f, Player = -1, Dir = dir,
        };

        [Test]
        public void TheFarSideOfABurstTakesMoreThanTheSideTheShellCameFrom()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            float3 flight = new float3(0f, 0f, 1f);          // travelling towards +Z
            int behind = r.Man(at - new float3(0f, 0f, 4f));  // the side it came from
            int ahead = r.Man(at + new float3(0f, 0f, 4f));   // the side it was going

            r.Fire(Shell(at, 8f, 200f, flight));

            float near = r.Took(behind), far = r.Took(ahead);
            Assert.Greater(far, near, "the fragments carry on: the far side takes more");
            Assert.AreEqual((1f + BlastRules.DirBias) / (1f - BlastRules.DirBias), far / near, 0.02f,
                "and by exactly the bias, since both men are the same distance out");
        }

        [Test]
        public void AShellWithNoFlightDirectionHarmsBothSidesAlike()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int a = r.Man(at - new float3(0f, 0f, 4f));
            int b = r.Man(at + new float3(0f, 0f, 4f));

            r.Fire(Shell(at, 8f, 200f));   // a cook-off, or a round coming straight down: Dir is zero

            Assert.AreEqual(r.Took(a), r.Took(b), 0.01f, "with no direction a burst is a circle again");
        }

        [Test]
        public void ATrenchAlwaysGivesLimitedProtectionEvenToTheBayTheShellLandsIn()
        {
            using var r = new Rig();
            var map = r.M.Map;
            // the z = 60 trench runs across the width; put three men in it and one out in the open beside it
            float3 inBay = new float3(150f, 0f, 62f);
            float3 alongTrench = new float3(150f + BlastRules.BayMetres + 4f, 0f, 62f);
            float3 otherTrench = new float3(150f, 0f, 142f);   // the z = 140 trench
            int bay = r.Man(inBay), along = r.Man(alongTrench), other = r.Man(otherTrench);
            foreach (int s in new[] { bay, along, other }) r.M.World.Flags[s] |= (uint)UnitFlags.InTrench;
            // [T8] the premise of the test: both men must stand in the SAME trench. A negative CellTrenchId means
            // "no trench here" (Blast.cs:242 branches on hitTrench >= 0), and the old line compared one cell with
            // itself, so it held even if neither man was in a trench at all.
            var cb = map.NavCellOf(inBay); var ca = map.NavCellOf(alongTrench);
            int idBay = map.CellTrenchId[map.NavIndex(cb.x, cb.y)];
            Assert.GreaterOrEqual(idBay, 0, "[T8] the bay man stands in a trench at all");
            Assert.AreEqual(idBay, map.CellTrenchId[map.NavIndex(ca.x, ca.y)],
                "[T8] the bay man and the man along the traverse are in the SAME trench, or this test proves nothing");

            // a big shell in the bay, wide enough to reach along the trench as well
            r.Fire(Shell(inBay, 24f, 200f));

            float took = r.Took(bay);
            Assert.Greater(took, 0f, "a shell in your own bay still hurts");
            Assert.Less(took, 200f * BlastRules.TrenchBayFactor + 1f,
                "but a trench ALWAYS gives limited protection, even for a direct hit in the bay");
            // [T8] the old ratio passed when `along` took nothing at all, which is the opposite of the claim
            Assert.Greater(r.Took(along), 0f, "[T8] the shell reached along the trench at all");
            Assert.Greater(r.Took(bay), r.Took(along),
                "[T8] further along the same trench a traverse takes more of it again");
            Assert.AreEqual(0f, r.Took(other), 0.01f, "and the next trench along is 80 m away: out of reach");
        }

        [Test]
        public void AShellDownInATrenchBarelyReachesTheMenInTheOpen()
        {
            using var r = new Rig();
            float3 inTrench = new float3(150f, 0f, 62f);
            int open = r.Man(new float3(150f, 0f, 70f));      // 8 m out in the field
            int openFar = r.Man(new float3(150f, 0f, 240f));  // well away, as a control

            r.Fire(Shell(inTrench, 16f, 200f));

            Assert.Greater(r.Took(open), 0f);
            // the parapet is between them: the field takes FieldShadow of what it would otherwise
            float bare = 200f * (1f - 0.75f * (8f / 16f));
            Assert.Less(r.Took(open), bare * (BlastRules.FieldShadow + 0.05f),
                "a shell under the parapet does not sweep the field");
            Assert.AreEqual(0f, r.Took(openFar), 0.01f);
        }

        [Test]
        public void TheCostOfAskingTheGroundWhatItShadowsIsBounded()
        {
            using var r = new Rig();
            // a wide shell in a trench, with far more men inside its radius than the cap allows rays for
            float3 at = new float3(150f, 0f, 62f);
            for (int i = 0; i < 140; i++) r.Man(new float3(80f + i, 0f, 66f));

            r.Fire(Shell(at, 90f, 50f));

            // [T8] LessOrEqual alone passed at zero rays, i.e. if the cap were dodged by casting none. 140 men stand
            // inside a 90 m burst and the cap is MaxRaycasts (Blast.cs:97,256), so the cap must be REACHED.
            Assert.AreEqual(BlastRules.MaxRaycasts, r.M.Blast.LastRaycasts,
                "[T8] at most MaxRaycasts terrain rays per impact, taken in slot order so the cap is the same " +
                "everywhere - and with 140 men in radius the cap is hit, not dodged by casting none");
        }

        [Test]
        public void FallingMasonryIsNotDirectionalAndABayIsNotARoof()
        {
            using var r = new Rig();
            float3 inBay = new float3(150f, 0f, 62f);
            int man = r.Man(inBay);
            r.M.World.Flags[man] |= (uint)UnitFlags.InTrench;

            var masonry = Shell(inBay, 3f, 120f, new float3(0f, 0f, 1f));
            masonry.Shape = (int)BlastShape.Masonry;
            r.Fire(masonry);

            float took = r.Took(man);
            Assert.AreEqual(120f * BlastRules.MasonryTrenchFactor, took, 2f,
                "a building coming down does not care which bay he is in, and its Dir is ignored");
        }

        [Test]
        public void NothingEverCutsABurstToNothing()
        {
            using var r = new Rig();
            float3 inBay = new float3(150f, 0f, 62f);
            int man = r.Man(inBay);
            r.M.World.Flags[man] |= (uint)UnitFlags.InTrench;
            r.M.World.StanceOf[man] = (byte)Stance.Prone;

            r.Fire(Shell(new float3(150f, 0f, 240f), 400f, 1000f));   // every reduction that exists, all at once

            Assert.Greater(r.Took(man), 0f, "MinThrough is a floor: a shell on top of a dugout still hurts");
        }

        [Test]
        public void ADirectionalBurstIsStillDeterministic()
        {
            ulong Run()
            {
                using var r = new Rig();
                for (int i = 0; i < 12; i++) r.Man(new float3(140f + i * 2f, 0f, 240f));
                r.Fire(Shell(new float3(150f, 0f, 240f), 12f, 90f, math.normalize(new float3(0.6f, 0f, 0.8f))));
                return r.M.World.Hash();
            }
            Assert.AreEqual(Run(), Run());
        }
    }
}
