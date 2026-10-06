// Phase: sim (2026-10-01, lane/sim/men-steady) — a man's step is not turned back on the one before, measured over the
// scripts' own match. They play a whole match through MatchLoopTests.Play with a ScriptedEnemy, so they are Match
// tests: the Sim tests' assembly sees neither (the test-modules split of 2026-10-02).
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class SteadyStepTests
    {
        /// <summary>A man walking to his post squeezes past his mates at theirs (2026-10-01). In the scripts' own match, seed
        /// 2, a man dropped into the fire trench at (24, 68) with his post 11 m along it, and the man at the post beside him
        /// held him off: the 2 m spacing's hard core under a metre shoved him back a step every few ticks (forward, forward,
        /// forward, back), and he was drawn turning about on each back-step (the gym's facing measure). Counted over the
        /// whole garrison, the first two minutes: a man's step reversed against the one before.</summary>
        [Test]
        public void AManWalkingToHisPostSqueezesPastHisMates()
        {
            int reversals = 0;
            var last = new Dictionary<long, float2>(); var lastStep = new Dictionary<long, float2>();
            uint seen = uint.MaxValue;
            MatchLoopTests.Play(MatchLoopTests.Policy.Script, 2, 2u, new ScriptedEnemy(), 8, m =>
            {
                var w = m.World;
                if (w.Tick == seen) return; seen = w.Tick;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0 || w.TrenchId[i] < 0) continue;
                    long key = ((long)i << 16) | w.Generation[i];
                    float2 p = w.Position[i].xz;
                    if (last.TryGetValue(key, out var q))
                    {
                        float2 step = p - q;
                        if (lastStep.TryGetValue(key, out var ls))
                        {
                            float a = math.length(step), b = math.length(ls);
                            if (a > 0.015f && b > 0.015f && math.dot(step, ls) < -0.5f * a * b) reversals++;
                        }
                        lastStep[key] = step;
                    }
                    last[key] = p;
                }
            });
            TestContext.WriteLine($"garrison men's steps reversed {reversals} times in two minutes");
            Assert.Less(reversals, 8, "a man on his way to his post is not shoved back and forth by the men at theirs");
        }

        /// <summary>A man closing on an enemy does not zig-zag where he stands (2026-10-01). EngageSystem asks whether the
        /// way to him is open ground; sampled a metre apart from where he stood, the answer by the corner of a wire or
        /// trench cell changed with a tenth of a metre, so he closed on him one tick and followed his goal's field the
        /// next, back and forth (the scripts' match, seed 2: one man's steps reversed 34 times in one place, 1.3 reversals
        /// a man-minute in the open). Now the way is walked cell by cell, and a man already closing keeps on while the
        /// next CloseKeep metres are open. Counted over the match: a man in the open whose step reversed the one before.</summary>
        [Test]
        public void AManClosingOnTheEnemyDoesNotZigZagWhereHeStands()
        {
            int reversals = 0; float open = 0f;
            var last = new Dictionary<long, float2>(); var lastStep = new Dictionary<long, float2>();
            uint seen = uint.MaxValue;
            MatchLoopTests.Play(MatchLoopTests.Policy.Script, 8, 2u, new ScriptedEnemy(), 8, m =>
            {
                var w = m.World;
                if (w.Tick == seen) return; seen = w.Tick;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)(UnitFlags.Vehicle | UnitFlags.InTrench)) != 0 || w.TrenchId[i] >= 0) continue;
                    open += w.Config.TickSeconds;
                    long key = ((long)i << 16) | w.Generation[i];
                    float2 p = w.Position[i].xz;
                    if (last.TryGetValue(key, out var q))
                    {
                        float2 step = p - q;
                        if (lastStep.TryGetValue(key, out var ls))
                        {
                            float a = math.length(step), b = math.length(ls);
                            if (a > 0.015f && b > 0.015f && math.dot(step, ls) < -0.5f * a * b) reversals++;
                        }
                        lastStep[key] = step;
                    }
                    last[key] = p;
                }
            });
            float perMinute = reversals / math.max(1f, open / 60f);
            TestContext.WriteLine($"men in the open: {open / 60f:F0} man-minutes, {reversals} steps reversed ({perMinute:F2} a man-minute)");
            Assert.Less(perMinute, 0.8f, "a man closing on an enemy does not close and stop closing on alternate ticks (1.3 a man-minute when the way was sampled a metre apart)");
        }
    }
}
