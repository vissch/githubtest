// Phase: perf pass (2026-11-25, implemented) - part of AnimationController: the two hot loops that ran on the main
// thread as managed code, now Burst jobs run synchronously (.Run(), no worker threads, no scheduling latency):
//   AdvanceJob    the render-frame advance of every man Tick decided a clip for (was Advance's loop);
//   ExplosionJob  the Explosion case of Latch: for a run of explosion events, in event order, every slot's distance test,
//                 grime, blast latch and flinch wave (was a managed loop over every slot per shell).
// Both are line-for-line ports: the same float operations in the same order, only the containers changed (the clip table
// is copied into a NativeArray each call; the followed man's time-in-clip dictionary stays managed, outside the job).
// docs/reference/tasks.md lists the tests that guard them (AnimAdvanceTests, BlastReactionTests, ExplosionLatchJobTests).
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim;

namespace TW.Presentation
{
    public sealed partial class AnimationController
    {
        /// <summary>What a burst latched on one man this tick (the controller's private latches), for the tests.</summary>
        public struct BlastLatch { public float Radius, Dist; public float3 Dir; public uint WaveAt; public float WaveR, WaveD; }

        /// <summary>The blast latches of one slot as the last Tick left them. Tests read it; nothing in production does.</summary>
        public BlastLatch LatchedBlast(int slot) => new BlastLatch { Radius = blastRadius[slot], Dist = blastDist[slot], Dir = hitDir[slot], WaveAt = waveAt[slot], WaveR = waveR[slot], WaveD = waveD[slot] };

        // the clip table as a native copy for AdvanceJob: Clips.Table is a managed array whose rows Apply and Reset overwrite
        // at any time, so it is copied afresh on every Advance (about 200 small rows)
        NativeArray<ClipInfo> clipTable;

        void AllocateJobs() { clipTable = new NativeArray<ClipInfo>(Clips.Table.Length, Allocator.Persistent); }
        void DisposeJobs() { if (clipTable.IsCreated) clipTable.Dispose(); }

        /// <summary>One run of consecutive Explosion events, events[from..to), latched onto every slot below count in event order.
        /// A run, not the whole tick's list: a Hit or a MeleeBlow between two shells writes the same hitDir, so the order
        /// against those must stay as it was.</summary>
        void LatchExplosions(SimWorld w, NativeArray<SimEvent> events, int from, int to)
        {
            new ExplosionJob
            {
                Events = events, From = from, To = to, Count = count, Tick = tick, TickSeconds = tickSeconds,
                Strafe = (int)TW.Sim.Combat.BlastShape.Strafe,
                Flags = w.Flags, Position = w.Position, Generation = w.Generation, State = State,
                Grime = Grime, BlastRadius = blastRadius, BlastDist = blastDist, HitDir = hitDir, WaveAt = waveAt, WaveR = waveR, WaveD = waveD,
            }.Run();
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct ExplosionJob : IJob
        {
            [ReadOnly] public NativeArray<SimEvent> Events;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<ushort> Generation;
            [ReadOnly] public NativeArray<AnimState> State;
            public NativeArray<float> Grime, BlastRadius, BlastDist, WaveR, WaveD;
            public NativeArray<float3> HitDir;
            public NativeArray<uint> WaveAt;
            public int From, To, Count, Strafe;
            public uint Tick;
            public float TickSeconds;

            public void Execute()
            {
                for (int k = From; k < To; k++)
                {
                    var e = Events[k];
                    // Inside the ring the sim throws men in (0.85 of the radius), and for anyone it killed, the burst
                    // is this tick's. Past it the reaction runs out through the men at WaveSpeed, so a line flinches
                    // one man after the next instead of all on one tick, and it reaches WaveReach past the radius,
                    // where the sim stops. Everyone within six metres of the edge wears some of the earth it threw.
                    float r = e.Scalar, reach = r + WaveReach, reach2 = reach * reach, inner = 0.85f * r, dirty = r + 6f;
                    // an aircraft's rounds (BlastShape.Strafe, in dir.y) kill where men stand: they flinch from it but nobody
                    // is latched for a throw (its 5 m radius passed the 4 m throw test, so the sim's no-knock fix still threw
                    // them 1.8-7.5 m on screen: critic r6, 2026-09-27)
                    bool rounds = (int)math.round(e.Dir.y) == Strafe;
                    float perTick = WaveSpeed * TickSeconds;
                    for (int i = 0; i < Count; i++)
                    {
                        bool alive = (Flags[i] & (uint)UnitFlags.Alive) != 0;
                        if (!alive)
                        {
                            var st = State[i];
                            if (st.Dead || st.Generation != Generation[i]) continue;   // the men it killed this tick are still in it
                        }
                        float3 d = Position[i] - e.Pos; d.y = 0f; float dd = math.lengthsq(d);
                        if (dd >= reach2) continue;
                        float dist = math.sqrt(dd);
                        if (dist < dirty) { float close = 1f - dist / dirty; Grime[i] = math.min(1f, Grime[i] + 0.5f * close * close); }
                        if (!alive || dist < inner)
                        {
                            if (!rounds && (BlastRadius[i] <= 0f || dist < BlastDist[i]))
                            {
                                BlastRadius[i] = r; BlastDist[i] = dist;
                                // math.normalizesafe(d, (0, 0, 1)) with its rsqrt written out as math.rsqrt defines it (1 / sqrt), so no
                                // precision mode can turn it into an estimate
                                float len = math.dot(d, d);
                                HitDir[i] = -math.select(new float3(0f, 0f, 1f), d * (1f / math.sqrt(len)), len > math.FLT_MIN_NORMAL);
                            }
                        }
                        else if (WaveAt[i] == 0u || dist < WaveD[i])
                        {
                            WaveAt[i] = Tick + 1u + (uint)((dist - inner) / perTick) + (Hash(State[i].Seed, Tick + 71u) < 0.5f ? 0u : 1u);
                            WaveR[i] = r; WaveD[i] = dist;
                        }
                    }
                }
            }
        }

        /// <summary>Move every man Tick decided a clip for one render frame on. The followed man's time-in-clip (a managed
        /// dictionary) is added to here, outside the job.</summary>
        void RunAdvance(float dt)
        {
            if (activeCount <= 0) return;
            clipTable.CopyFrom(Clips.Table);
            if (FollowSlot >= 0)
                for (int at = 0; at < activeCount; at++)
                    if (active[at] == FollowSlot)
                    {
                        var c = State[FollowSlot].Clip;   // Advance never changes a man's clip
                        if (c != Clip.None) { timeIn.TryGetValue(c, out float t); timeIn[c] = t + dt; }
                        break;
                    }
            new AdvanceJob
            {
                Active = active, ActiveCount = activeCount, Table = clipTable, Dt = dt, Tick = tick, TickSeconds = tickSeconds,
                State = State, Row = Row, Phase = Phase, Yaw = Yaw, Lift = Lift, Hop = Hop, Lunge = Lunge,
                PrevRow = PrevRow, PrevPhase = PrevPhase, Blend = Blend,
            }.Run();
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct AdvanceJob : IJob
        {
            [ReadOnly] public NativeArray<int> Active;
            [ReadOnly] public NativeArray<ClipInfo> Table;
            public NativeArray<AnimState> State;
            public NativeArray<ushort> Row, PrevRow;
            public NativeArray<float> Phase, Yaw, Lift, Hop, PrevPhase, Blend;
            public NativeArray<float2> Lunge;
            public int ActiveCount;
            public uint Tick;
            public float Dt, TickSeconds;

            public void Execute()
            {
                float dt = Dt;
                for (int at = 0; at < ActiveCount; at++)
                {
                    int i = Active[at];
                    var s = State[i];
                    var info = Table[(int)s.Clip];
                    if (s.Clip == Clip.None) { Row[i] = (ushort)Clip.Idle; Phase[i] = 0f; Blend[i] = 0f; continue; }
                    s.Frame += dt * s.Rate;
                    if (info.Loop) { if (s.Frame >= info.Seconds) s.Frame -= info.Seconds * math.floor(s.Frame / info.Seconds); }
                    else if (s.Frame > info.Seconds) s.Frame = info.Seconds;
                    if (s.Blend > 0f) s.Blend -= dt;
                    // the feet come round to the body yaw at a turn's pace; a turn clip holds the old facing until it ends
                    if (s.Rung != Rung.Turn || info.Loop)
                    {
                        float d = s.BodyYaw - s.ShownYaw; while (d > math.PI) d -= 2f * math.PI; while (d < -math.PI) d += 2f * math.PI;
                        float step = math.min(math.abs(d), dt * (s.Clip == Clip.DiveAway ? 14f : s.Rung == Rung.Locomotion ? 7f : s.Rung == Rung.Fire ? 6f : 3f));
                        s.ShownYaw += math.sign(d) * step;
                        while (s.ShownYaw > math.PI) s.ShownYaw -= 2f * math.PI; while (s.ShownYaw < -math.PI) s.ShownYaw += 2f * math.PI;
                    }
                    bool climb = s.Rung == Rung.Trench && (s.Clip == Clip.ClimbOut || s.Clip == Clip.ClimbLadder || s.Clip == Clip.ClimbHold);
                    if (climb) s.ClimbTime += dt; else s.ClimbTime = 0f;
                    Lift[i] = climb ? math.saturate(s.ClimbTime / 0.8f) : 0f;   // the sim holds him on the trench floor for the vault: he is drawn rising up the wall
                    State[i] = s;
                    Row[i] = (ushort)s.Clip;
                    float ph = info.Seconds > 0f ? s.Frame / info.Seconds : 0f;
                    Phase[i] = info.Loop ? math.frac(ph) : math.min(ph, 1f);
                    Yaw[i] = s.ShownYaw;
                    // blown off his feet: a parabola over the first HopSeconds of his fall (Frame runs at Rate, so measure in clip seconds)
                    float hop = 0f;
                    if (s.HopHeight > 0f && (s.Clip == Clip.Trip || s.Clip == Clip.DiveAway || s.Clip == Clip.JumpDown) && s.ClipStart <= s.HopTick)
                    {
                        float u = math.saturate((s.Frame - s.HopFrame) / ((s.HopFor > 0f ? s.HopFor : HopSeconds) * math.max(0.1f, s.Rate)));
                        hop = s.HopHeight * 4f * u * (1f - u);
                        if (u >= 1f) { s.HopHeight = 0f; State[i] = s; }
                    }
                    if (s.LeapTick != 0)
                    {
                        float u = (Tick - s.LeapTick) * TickSeconds / s.LeapSeconds;
                        if (u < 1f) hop = math.max(hop, s.LeapHeight * 4f * u * (1f - u));
                        else { s.LeapTick = 0; State[i] = s; }
                    }
                    Hop[i] = hop;
                    // hand to hand: he steps into the blow he strikes and back out of it; struck, he is driven back and recovers
                    float2 lunge = default;
                    if (s.Clip == Clip.MeleeStab || s.Clip == Clip.MeleePunch || s.Clip == Clip.MeleeSmash)
                    {
                        float u = info.Seconds > 0f ? s.Frame / info.Seconds : 1f;
                        lunge = new float2(math.sin(s.ShownYaw), math.cos(s.ShownYaw)) * (LungeMetres * math.sin(math.PI * math.saturate((u - 0.08f) / 0.62f)));
                    }
                    if (s.RecoilTick != 0)
                    {
                        float since = (Tick - s.RecoilTick) * TickSeconds;
                        if (since < RecoilSeconds) { float k = 1f - since / RecoilSeconds; lunge += s.RecoilDir * (RecoilMetres * math.sin(math.PI * math.sqrt(1f - k))); }
                        else { s.RecoilTick = 0; State[i] = s; }
                    }
                    Lunge[i] = lunge;
                    var prev = Table[(int)s.PrevClip];
                    PrevRow[i] = (ushort)s.PrevClip;
                    float pp = prev.Seconds > 0f ? s.PrevFrame / prev.Seconds : 0f;
                    PrevPhase[i] = prev.Loop ? math.frac(pp) : math.min(pp, 1f);
                    Blend[i] = s.Blend > 0f && s.Fade > 0f ? math.saturate(s.Blend / s.Fade) : 0f;
                }
            }
        }
    }
}
