// Phase: perf pass (2026-11-25) - the two Burst jobs of AnimationController.Jobs.cs against a plain managed copy of the loops
// they replaced (kept here, written the way the controller had them): the Explosion latch over a scattered crowd, with a
// man killed this tick, a Hit between two shells and an aircraft's rounds, must leave bit-identical grime, blast latches
// and waves; Advance must leave bit-identical frames, rows, phases and blends. No sim is stepped, the rig is the one of
// BlastReactionTests and AnimAdvanceTests.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class ExplosionLatchJobTests
    {
        // Mono runs the managed loop with wider intermediates than Burst's strict 32-bit floats (seen: grime differs in the
        // last bit, 5e-10), so the floats are held to a tolerance far under anything drawn; ticks and rows stay exact.
        const float Tol = 1e-5f;
        static readonly float3 Centre = new float3(150f, 0f, 300f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            public SimWorld W => M.World;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;
            public NativeArray<ushort> Row => row;
            public NativeArray<float> Phase => phase;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        // the controller's Hash, as it was
        static float Hash(uint seed, uint tick) { uint h = seed ^ (tick * 2246822519u); h ^= h >> 13; h *= 3266489917u; h ^= h >> 16; return (h & 0xFFFFFF) / 16777216f; }

        /// <summary>The Explosion case of Latch as a managed loop over every slot, per event in order (the code the job replaced).</summary>
        static void ReferenceLatch(SimWorld w, AnimationController a, int count, uint tick, float tickSeconds,
                                   float[] grime, float[] radius, float[] dist, float3[] dir, uint[] waveAt, float[] waveR, float[] waveD)
        {
            var ev = w.Events.Events;
            for (int k = 0; k < ev.Length; k++)
            {
                var e = ev[k];
                if (e.Type == SimEventType.Hit) { if (e.B >= 0 && e.B < count) dir[e.B] = e.Dir; continue; }
                if (e.Type != SimEventType.Explosion) continue;
                float r = e.Scalar, reach = r + AnimationController.WaveReach, reach2 = reach * reach, inner = 0.85f * r, dirty = r + 6f;
                bool rounds = (int)math.round(e.Dir.y) == (int)TW.Sim.Combat.BlastShape.Strafe;
                float perTick = AnimationController.WaveSpeed * tickSeconds;
                for (int i = 0; i < count; i++)
                {
                    bool alive = (w.Flags[i] & (uint)UnitFlags.Alive) != 0;
                    if (!alive && (a.State[i].Dead || a.State[i].Generation != w.Generation[i])) continue;
                    float3 d = w.Position[i] - e.Pos; d.y = 0f; float dd = math.lengthsq(d);
                    if (dd >= reach2) continue;
                    float dst = math.sqrt(dd);
                    if (dst < dirty) { float close = 1f - dst / dirty; grime[i] = math.min(1f, grime[i] + 0.5f * close * close); }
                    if (!alive || dst < inner)
                    {
                        if (!rounds && (radius[i] <= 0f || dst < dist[i])) { radius[i] = r; dist[i] = dst; dir[i] = -math.normalizesafe(d, new float3(0, 0, 1)); }
                    }
                    else if (waveAt[i] == 0u || dst < waveD[i])
                    {
                        waveAt[i] = tick + 1u + (uint)((dst - inner) / perTick) + (Hash(a.State[i].Seed, tick + 71u) < 0.5f ? 0u : 1u);
                        waveR[i] = r; waveD[i] = dst;
                    }
                }
            }
        }

        static List<int> Crowd(Rig r, int n)
        {
            var men = new List<int>();
            uint s = 12345u;
            for (int i = 0; i < n; i++)
            {
                s = s * 1664525u + 1013904223u; float x = ((s >> 8) & 0xFFFF) / 65535f * 44f - 22f;
                s = s * 1664525u + 1013904223u; float z = ((s >> 8) & 0xFFFF) / 65535f * 44f - 22f;
                men.Add(r.W.Spawn((byte)(i & 1), 0, Centre + new float3(x, 0f, z), 100f, 3f, false));
            }
            return men;
        }

        [Test]
        public void ExplosionJobMatchesTheManagedLoop()
        {
            using var r = new Rig();
            var men = Crowd(r, 60);
            r.A.Tick(r.W); r.W.Events.Clear(); r.W.Tick++;   // every man gets his state (Fresh) before the shells land
            int count = r.W.HighWater;

            // two of them die this tick (their slots still count as in the blast), one is hit between the shells
            r.W.Despawn(men[3], men[4], new float3(0f, 0f, 1f), 0f);
            r.W.Despawn(men[11], men[4], new float3(0f, 0f, 1f), 0f);
            r.W.Events.Add(r.W.Tick, SimEventType.Explosion, 0, 0, Centre + new float3(3f, 0f, -2f), default, 8f);
            r.W.Events.Add(r.W.Tick, SimEventType.Hit, 0, men[7], Centre, new float3(0f, 0f, 5f), 1f);
            r.W.Events.Add(r.W.Tick, SimEventType.Explosion, 0, 0, Centre + new float3(-6f, 0f, 5f), default, 9.5f);
            r.W.Events.Add(r.W.Tick, SimEventType.Explosion, 0, 0, Centre + new float3(4f, 0f, 9f), new float3(0f, 6f, 0f), 5f);   // an aircraft's rounds (BlastShape.Strafe)
            r.W.Events.Add(r.W.Tick, SimEventType.Explosion, 0, 0, Centre + new float3(-12f, 0f, -9f), default, 8f);

            var grime = new float[count]; var radius = new float[count]; var dist = new float[count]; var dir = new float3[count];
            var waveAt = new uint[count]; var waveR = new float[count]; var waveD = new float[count];
            for (int i = 0; i < count; i++)
            {
                var l = r.A.LatchedBlast(i);
                grime[i] = r.A.Grime[i]; radius[i] = 0f; dist[i] = l.Dist; dir[i] = l.Dir; waveAt[i] = l.WaveAt; waveR[i] = l.WaveR; waveD[i] = l.WaveD;
            }
            uint tick = r.W.Tick; float tickSeconds = r.W.Config.TickSeconds;
            ReferenceLatch(r.W, r.A, count, tick, tickSeconds, grime, radius, dist, dir, waveAt, waveR, waveD);
            float fade = AnimationController.GrimeFade * tickSeconds;
            for (int i = 0; i < count; i++) if (grime[i] > 0f) grime[i] = math.max(0f, grime[i] - fade);   // the end of Latch wears it off

            r.A.Tick(r.W);

            int touched = 0;
            for (int i = 0; i < count; i++)
            {
                var l = r.A.LatchedBlast(i);
                Assert.AreEqual(grime[i], r.A.Grime[i], Tol, "grime of slot " + i);
                Assert.AreEqual(radius[i], l.Radius, Tol, "blast radius of slot " + i);
                Assert.AreEqual(dist[i], l.Dist, Tol, "blast distance of slot " + i);
                Assert.AreEqual(dir[i].x, l.Dir.x, Tol, "blast dir x of slot " + i); Assert.AreEqual(dir[i].y, l.Dir.y, Tol, "blast dir y of slot " + i); Assert.AreEqual(dir[i].z, l.Dir.z, Tol, "blast dir z of slot " + i);
                Assert.AreEqual(waveAt[i], l.WaveAt, "wave tick of slot " + i);
                Assert.AreEqual(waveR[i], l.WaveR, Tol, "wave radius of slot " + i);
                Assert.AreEqual(waveD[i], l.WaveD, Tol, "wave distance of slot " + i);
                if (radius[i] > 0f || waveAt[i] != 0u || grime[i] > 0f) touched++;
            }
            Assert.Greater(touched, 20, "the shells must reach a crowd, or the comparison proves nothing");
        }

        [Test]
        public void AdvanceJobMatchesTheManagedLoop()
        {
            using var r = new Rig();
            var men = Crowd(r, 40);
            r.A.Tick(r.W); r.W.Events.Clear(); r.W.Tick++;
            r.W.Events.Add(r.W.Tick, SimEventType.Explosion, 0, 0, Centre, default, 8f);   // some of them react, so the clips differ
            r.A.Tick(r.W); r.W.Events.Clear(); r.W.Tick++;
            r.A.Follow(men[5]);

            var info = Clips.Table;
            foreach (float dt in new[] { 0.016f, 0.033f, 0.1f, 0.7f })
            {
                var before = new AnimState[men.Count];
                for (int n = 0; n < men.Count; n++) before[n] = r.A.State[men[n]];
                r.A.Advance(dt);
                for (int n = 0; n < men.Count; n++)
                {
                    int i = men[n]; var s = before[n]; var c = info[(int)s.Clip];
                    if (s.Clip == Clip.None) continue;
                    s.Frame += dt * s.Rate;
                    if (c.Loop) { if (s.Frame >= c.Seconds) s.Frame -= c.Seconds * math.floor(s.Frame / c.Seconds); }
                    else if (s.Frame > c.Seconds) s.Frame = c.Seconds;
                    if (s.Blend > 0f) s.Blend -= dt;
                    float ph = c.Seconds > 0f ? s.Frame / c.Seconds : 0f;
                    var p = info[(int)s.PrevClip];
                    float pp = p.Seconds > 0f ? s.PrevFrame / p.Seconds : 0f;
                    var after = r.A.State[i];
                    Assert.AreEqual(s.Frame, after.Frame, Tol, "frame of slot " + i);
                    Assert.AreEqual(s.Blend, after.Blend, Tol, "blend left of slot " + i);
                    Assert.AreEqual((ushort)s.Clip, r.Row[i], "row of slot " + i);
                    Assert.AreEqual(c.Loop ? math.frac(ph) : math.min(ph, 1f), r.Phase[i], Tol, "phase of slot " + i);
                    Assert.AreEqual((ushort)s.PrevClip, r.A.PrevRow[i], "previous row of slot " + i);
                    Assert.AreEqual(p.Loop ? math.frac(pp) : math.min(pp, 1f), r.A.PrevPhase[i], Tol, "previous phase of slot " + i);
                    Assert.AreEqual(s.Blend > 0f && s.Fade > 0f ? math.saturate(s.Blend / s.Fade) : 0f, r.A.Blend[i], Tol, "blend of slot " + i);
                }
            }
            // the followed man's time in each clip is still kept (it lives outside the job)
            StringAssert.Contains(r.A.State[men[5]].Clip.ToString(), r.A.TraceText(), "the followed man's time-in-clip list lost his clip");
        }
    }
}
