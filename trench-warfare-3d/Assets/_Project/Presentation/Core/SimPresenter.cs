// Phase: P0 (implemented) — Pose stream producer, see docs/02-contracts.md
// Captures two consecutive tick states and interpolates them at display rate into UnitPose[].
// A slot that was (re)spawned since the last capture starts at its own position: slots are reused, and without this a
// fresh man is drawn sliding in from wherever the slot's last owner died.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim;

namespace TW.Presentation
{
    public sealed class SimPresenter : IDisposable
    {
        NativeArray<float3> prevPos, curPos;
        NativeArray<float> prevYaw, curYaw;
        NativeArray<byte> stance, archetype, team;
        NativeArray<uint> flags;
        NativeArray<int> target;
        NativeArray<ushort> generation;
        public NativeArray<UnitPose> Poses;
        public NativeArray<int> PoseSlot;   // slot index per pose (for picking / UI)
        public int PoseCount;
        int count;
        bool primed;

        public SimPresenter(int maxSlots)
        {
            prevPos = new NativeArray<float3>(maxSlots, Allocator.Persistent);
            curPos = new NativeArray<float3>(maxSlots, Allocator.Persistent);
            prevYaw = new NativeArray<float>(maxSlots, Allocator.Persistent);
            curYaw = new NativeArray<float>(maxSlots, Allocator.Persistent);
            stance = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            archetype = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            team = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            flags = new NativeArray<uint>(maxSlots, Allocator.Persistent);
            target = new NativeArray<int>(maxSlots, Allocator.Persistent);
            generation = new NativeArray<ushort>(maxSlots, Allocator.Persistent);
            Poses = new NativeArray<UnitPose>(maxSlots, Allocator.Persistent);
            PoseSlot = new NativeArray<int>(maxSlots, Allocator.Persistent);
        }

        /// <summary>Call once after every stepped tick.</summary>
        public void Capture(SimWorld w)
        {
            count = w.HighWater;
            if (primed)
            {
                NativeArray<float3>.Copy(curPos, prevPos, count);
                NativeArray<float>.Copy(curYaw, prevYaw, count);
            }
            NativeArray<float3>.Copy(w.Position, curPos, count);
            NativeArray<float>.Copy(w.Yaw, curYaw, count);
            NativeArray<byte>.Copy(w.StanceOf, stance, count);
            NativeArray<byte>.Copy(w.Archetype, archetype, count);
            NativeArray<byte>.Copy(w.Team, team, count);
            NativeArray<uint>.Copy(w.Flags, flags, count);
            NativeArray<int>.Copy(w.TargetSlot, target, count);
            for (int i = 0; i < count; i++)
            {
                if (generation[i] == w.Generation[i]) continue;
                generation[i] = w.Generation[i]; prevPos[i] = curPos[i]; prevYaw[i] = curYaw[i];
            }
            if (!primed)
            {
                NativeArray<float3>.Copy(curPos, prevPos, count);
                NativeArray<float>.Copy(curYaw, prevYaw, count);
                primed = true;
            }
        }

        /// <summary>Interpolate for rendering. alpha in [0,1] between the previous and current tick.</summary>
        public void Interpolate(float alpha, float animTime)
        {
            if (count == 0) { PoseCount = 0; return; }
            var job = new InterpolateJob
            {
                PrevPos = prevPos, CurPos = curPos, PrevYaw = prevYaw, CurYaw = curYaw, Stance = stance, Archetype = archetype,
                Team = team, Flags = flags, Target = target, Poses = Poses, Alpha = alpha, AnimTime = animTime,
            };
            job.Schedule(count, 128).Complete();
            // compaction (single-threaded, cheap)
            int n = 0;
            for (int i = 0; i < count; i++)
            {
                if ((flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                Poses[n] = Poses[i];
                PoseSlot[n] = i;
                n++;
            }
            PoseCount = n;
        }

        [BurstCompile]
        struct InterpolateJob : IJobParallelFor
        {
            [ReadOnly] public NativeArray<float3> PrevPos, CurPos;
            [ReadOnly] public NativeArray<float> PrevYaw, CurYaw;
            [ReadOnly] public NativeArray<byte> Stance, Archetype, Team;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<int> Target;
            public NativeArray<UnitPose> Poses;
            public float Alpha, AnimTime;

            public void Execute(int i)
            {
                float3 p = math.lerp(PrevPos[i], CurPos[i], Alpha);
                float a = PrevYaw[i], b = CurYaw[i];
                float d = b - a;
                if (d > math.PI) d -= 2f * math.PI; else if (d < -math.PI) d += 2f * math.PI;
                float yaw = a + d * Alpha;
                bool moving = math.distancesq(PrevPos[i].xz, CurPos[i].xz) > 1e-4f, firing = Target[i] >= 0;
                ushort row;
                switch ((TW.Sim.Stance)Stance[i])
                {
                    case TW.Sim.Stance.Sprint: row = (ushort)AnimRow.Sprint; break;
                    case TW.Sim.Stance.Crouch: row = (ushort)AnimRow.CrouchWalk; break;
                    case TW.Sim.Stance.Prone: row = (ushort)(!moving && firing ? AnimRow.FireProne : AnimRow.ProneCrawl); break;
                    case TW.Sim.Stance.FireStep: row = (ushort)AnimRow.FireFireStep; break;
                    case TW.Sim.Stance.Pinned: row = (ushort)AnimRow.PinnedLoop; break;
                    case TW.Sim.Stance.Vault: row = (ushort)AnimRow.Vault; break;
                    default: row = (ushort)(moving ? AnimRow.Walk : firing ? AnimRow.FireStanding : AnimRow.Idle); break;
                }
                Poses[i] = new UnitPose
                {
                    Pos = p, Yaw = (half)yaw, AnimRow = row, AnimT = (half)math.frac(AnimTime + i * 0.137f),
                    Archetype = Archetype[i], Flags = (byte)(Flags[i] & 0xFF), Lod = 0, Team = Team[i],
                };
            }
        }

        public void Dispose()
        {
            prevPos.Dispose(); curPos.Dispose(); prevYaw.Dispose(); curYaw.Dispose(); stance.Dispose(); archetype.Dispose();
            team.Dispose(); flags.Dispose(); target.Dispose(); generation.Dispose(); Poses.Dispose(); PoseSlot.Dispose();
        }
    }
}
