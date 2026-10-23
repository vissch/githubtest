// Phase: SHOW (act-officer-rally, 2026-10-23) — the read-only reader AnimationController.Rally.cs draws from.
// AuraSystem.Unpin (Sim/Combat/Aura.cs) caps a covered man's suppression at UnpinTo every tick and says nothing: this
// watches for the man it just held off the floor (Covered and at the cap) and the officer nearest him, so the
// presenter can draw the rally without the sim knowing it is being watched. No write to the world or to the sim.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;

namespace TW.Presentation
{
    public sealed class RallyWatch
    {
        readonly int maxSlots;
        NativeArray<byte> cappedNow, cappedLast;
        NativeArray<ushort> genAt;
        NativeArray<int> officerOf;

        /// <summary>How many men this watch has seen go from capped to not-capped-last-tick, this match (the probe log).</summary>
        public int Rallies { get; private set; }
        /// <summary>How many times AnimationController.Rally has played the officer's gesture, this match (the probe log).</summary>
        public int Gestures;

        public RallyWatch(int maxSlots)
        {
            this.maxSlots = maxSlots;
            cappedNow = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            cappedLast = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            genAt = new NativeArray<ushort>(maxSlots, Allocator.Persistent);
            officerOf = new NativeArray<int>(maxSlots, Allocator.Persistent);
            for (int i = 0; i < maxSlots; i++) officerOf[i] = -1;
        }

        public void Dispose()
        {
            if (cappedNow.IsCreated) cappedNow.Dispose();
            if (cappedLast.IsCreated) cappedLast.Dispose();
            if (genAt.IsCreated) genAt.Dispose();
            if (officerOf.IsCreated) officerOf.Dispose();
        }

        /// <summary>Held now, under an officer's aura at the cap Unpin just set him to.</summary>
        public bool Capped(int slot) => slot >= 0 && slot < maxSlots && cappedNow[slot] != 0;

        /// <summary>The rising edge: capped this tick, not capped last tick (the same man). Fires once per rally.</summary>
        public bool RalliedTick(int slot) => slot >= 0 && slot < maxSlots && cappedNow[slot] != 0 && cappedLast[slot] == 0;

        /// <summary>The nearest living officer of his own team within aura range, or -1.</summary>
        public int Officer(int slot) => slot >= 0 && slot < maxSlots ? officerOf[slot] : -1;

        public void Tick(SimWorld w)
        {
            int n = w.HighWater;
            var aura = w.GetSystem<AuraSystem>();
            for (int i = 0; i < n && i < maxSlots; i++)
            {
                cappedLast[i] = cappedNow[i];
                bool alive = (w.Flags[i] & (uint)UnitFlags.Alive) != 0;
                ushort gen = w.Generation[i];
                if (!alive || gen != genAt[i]) { genAt[i] = gen; cappedLast[i] = 0; }
                bool capped = alive && aura != null && aura.Covered[i] != 0 && w.Suppression[i] >= AuraSystem.UnpinTo - 0.01f;
                cappedNow[i] = capped ? (byte)1 : (byte)0;
                officerOf[i] = capped ? NearestOfficer(w, i) : -1;
                if (capped && cappedLast[i] == 0) Rallies++;
            }
            for (int i = n; i < maxSlots; i++) { cappedNow[i] = 0; cappedLast[i] = 0; officerOf[i] = -1; }
        }

        static int NearestOfficer(SimWorld w, int slot)
        {
            float3 at = w.Position[slot];
            byte team = w.Team[slot];
            int best = -1; float bestD2 = float.MaxValue;
            int n = w.HighWater;
            for (int j = 0; j < n; j++)
            {
                if ((w.Flags[j] & (uint)UnitFlags.Alive) == 0 || w.Team[j] != team) continue;
                var spec = w.Units.Infantry[w.Archetype[j]];
                if (!spec.AuraUnpin || spec.AuraRadius <= 0f) continue;
                float3 d = w.Position[j] - at; d.y = 0f;
                float d2 = math.lengthsq(d);
                if (d2 > spec.AuraRadius * spec.AuraRadius || d2 >= bestD2) continue;
                best = j; bestD2 = d2;
            }
            return best;
        }
    }
}
