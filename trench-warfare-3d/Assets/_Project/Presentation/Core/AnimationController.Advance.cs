// Phase: B5 / docs/15 — the render-frame half of the character controller: one Burst job over the men Tick decided a
// clip for. The decision layer is AnimationController.cs; this file is only Advance. The clip table is a managed
// static array, so it is copied into a NativeArray once (and again whenever the bake's lengths land: Clips.Version).
// The followed man's time-in-clip tally is a managed dictionary and stays on the main thread.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Presentation
{
    public sealed partial class AnimationController
    {
        NativeArray<ClipInfo> clipTable;
        int clipTableVersion = -1;
        JobHandle advanceHandle;
        bool advanceScheduled;

        /// <summary>Start the render-frame advance over the slots Tick decided a clip for. Complete it with
        /// <see cref="CompleteAdvance"/> (SimHost hands the handle to the presenter's interpolate job as a dependency).</summary>
        public JobHandle ScheduleAdvance(float dt)
        {
            CompleteAdvance();
            if (!clipTable.IsCreated) clipTable = new NativeArray<ClipInfo>(Clips.Table.Length, Allocator.Persistent);
            if (clipTableVersion != Clips.Version) { NativeArray<ClipInfo>.Copy(Clips.Table, clipTable); clipTableVersion = Clips.Version; }
            // the trace's tally: a managed dictionary, so it is kept out of the job
            if (FollowSlot >= 0 && FollowSlot < count)
            {
                var f = State[FollowSlot];
                if (f.Clip != Clip.None) { timeIn.TryGetValue(f.Clip, out float t); timeIn[f.Clip] = t + dt; }
            }
            if (activeCount == 0) return default;
            var job = new AdvanceJob
            {
                Active = active, Table = clipTable, Dt = dt,
                State = State, Row = Row, Phase = Phase, Yaw = Yaw, Lift = Lift, Hop = Hop,
                PrevRow = PrevRow, PrevPhase = PrevPhase, Blend = Blend,
            };
            advanceHandle = job.Schedule(activeCount, 64);
            advanceScheduled = true;
            return advanceHandle;
        }

        /// <summary>Wait for the advance job. Safe to call when nothing is in flight.</summary>
        public void CompleteAdvance()
        {
            if (!advanceScheduled) return;
            advanceScheduled = false;
            advanceHandle.Complete();
        }

        /// <summary>Move every frame on, close the cross-fades, end the one-shots; then the stand-in row and phase per slot.</summary>
        public void Advance(float dt)
        {
            ScheduleAdvance(dt);
            CompleteAdvance();
        }

        void DisposeAdvance()
        {
            CompleteAdvance();
            if (clipTable.IsCreated) clipTable.Dispose();
        }

        // the job writes slot Active[k] only, and Tick puts each slot in the list at most once
        [BurstCompile]
        struct AdvanceJob : Unity.Jobs.IJobParallelFor
        {
            [ReadOnly] public NativeArray<int> Active;
            [ReadOnly] public NativeArray<ClipInfo> Table;
            public float Dt;
            [NativeDisableParallelForRestriction] public NativeArray<AnimState> State;
            [NativeDisableParallelForRestriction] public NativeArray<ushort> Row, PrevRow;
            [NativeDisableParallelForRestriction] public NativeArray<float> Phase, Yaw, Lift, Hop, PrevPhase, Blend;

            public void Execute(int k)
            {
                int i = Active[k];
                float dt = Dt;
                var s = State[i];
                var info = Table[(int)s.Clip];
                if (s.Clip == Clip.None) { Row[i] = (ushort)Clip.Idle; Phase[i] = 0f; Blend[i] = 0f; return; }
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
                State[i] = s;
                Row[i] = (ushort)s.Clip;
                float ph = info.Seconds > 0f ? s.Frame / info.Seconds : 0f;
                Phase[i] = info.Loop ? math.frac(ph) : math.min(ph, 1f);
                Yaw[i] = s.ShownYaw;
                bool climb = s.Rung == Rung.Trench && (s.Clip == Clip.ClimbOut || s.Clip == Clip.ClimbLadder || s.Clip == Clip.ClimbHold);
                Lift[i] = climb ? math.saturate(s.Frame / 0.8f) : 0f;   // the sim holds him on the trench floor for the vault: he is drawn rising up the wall
                // blown off his feet: a parabola over the first HopSeconds of his fall (Frame runs at Rate, so measure in clip seconds)
                float hop = 0f;
                if (s.HopHeight > 0f && (s.Clip == Clip.Trip || s.Clip == Clip.DiveAway) && s.ClipStart <= s.HopTick)
                {
                    float u = math.saturate((s.Frame - s.HopFrame) / (HopSeconds * math.max(0.1f, s.Rate)));
                    hop = s.HopHeight * 4f * u * (1f - u);
                    if (u >= 1f) { s.HopHeight = 0f; State[i] = s; }
                }
                Hop[i] = hop;
                var prev = Table[(int)s.PrevClip];
                PrevRow[i] = (ushort)s.PrevClip;
                float pp = prev.Seconds > 0f ? s.PrevFrame / prev.Seconds : 0f;
                PrevPhase[i] = prev.Loop ? math.frac(pp) : math.min(pp, 1f);
                Blend[i] = s.Blend > 0f && s.Fade > 0f ? math.saturate(s.Blend / s.Fade) : 0f;
            }
        }
    }
}
