// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — the choice behind NightLights' small machine pool
// (lights.machinePool, 0 by default: today). Kept apart from the Light components so a test can hold it:
//  - a request carries a key (the same thing asking again: a slot times four plus its kind), a priority (3 a
//    cook-off, 2 a burning machine, 1 the Maw's furnace), a peak and a life (0: for as long as it keeps asking);
//  - the same key keeps its slot; a new one takes a free slot, or the one with the lowest priority and then the
//    dimmest light, and only if that one ranks below it: a cook-off beats a fire, a fire beats a furnace, and a fire
//    keeps its light against another fire as bright as itself;
//  - a held light stays whole for HoldGrace seconds after it last asked (it is asked in one frame's LateUpdate and
//    lit in the next frame's Update), then fades out over HoldFade.
namespace TW.Presentation
{
    public sealed class MachineLightSlots
    {
        public const float HoldGrace = 0.1f, HoldFade = 0.3f;
        public readonly int Count;
        readonly int[] key, prio;
        readonly float[] peak, born, life, seen;

        public MachineLightSlots(int count)
        {
            Count = count < 0 ? 0 : count;
            key = new int[Count]; prio = new int[Count];
            peak = new float[Count]; born = new float[Count]; life = new float[Count]; seen = new float[Count];
            for (int i = 0; i < Count; i++) { key[i] = -1; seen[i] = float.NegativeInfinity; born[i] = float.NegativeInfinity; }
        }

        /// <summary>How bright slot i is now (0 when it is free).</summary>
        public float Level(int i, float now)
        {
            if (key[i] < 0) return 0f;
            if (life[i] > 0f)
            {
                float age = (now - born[i]) / life[i];
                return age >= 1f || age < 0f ? 0f : peak[i] * (1f - age) * (1f - age);
            }
            float gone = now - seen[i] - HoldGrace;
            return gone <= 0f ? peak[i] : gone >= HoldFade ? 0f : peak[i] * (1f - gone / HoldFade);
        }

        public int KeyOf(int i) => key[i];
        public int PriorityOf(int i) => prio[i];

        /// <summary>Frees the slots whose light has gone out.</summary>
        public void Sweep(float now)
        {
            for (int i = 0; i < Count; i++) if (key[i] >= 0 && Level(i, now) <= 0f && now > born[i]) key[i] = -1;
        }

        /// <summary>Asks for a light; returns the slot it has, or -1 when every slot holds something that ranks higher.</summary>
        public int Request(int k, int priority, float lightPeak, float seconds, float now)
        {
            if (Count == 0) return -1;
            int slot = -1;
            for (int i = 0; i < Count; i++) if (key[i] == k) { slot = i; break; }
            if (slot < 0)
            {
                float worstLevel = float.MaxValue; int worstPrio = int.MaxValue;
                for (int i = 0; i < Count; i++)
                {
                    float lv = Level(i, now);
                    if (key[i] < 0 || lv <= 0f) { slot = i; worstPrio = -1; break; }
                    if (prio[i] < worstPrio || (prio[i] == worstPrio && lv < worstLevel)) { worstPrio = prio[i]; worstLevel = lv; slot = i; }
                }
                if (slot >= 0 && worstPrio >= 0 && (worstPrio > priority || (worstPrio == priority && worstLevel >= lightPeak))) return -1;
                if (slot < 0) return -1;
                key[slot] = k; born[slot] = now;
            }
            else if (seconds > 0f) born[slot] = now;   // a flash asked again starts again
            prio[slot] = priority; peak[slot] = lightPeak; life[slot] = seconds; seen[slot] = now;
            return slot;
        }
    }
}
