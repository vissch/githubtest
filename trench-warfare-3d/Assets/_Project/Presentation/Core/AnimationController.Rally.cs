// Phase: SHOW (act-officer-rally, 2026-10-23) — part of AnimationController: draws the unpin RallyWatch reads
// (AuraSystem.Unpin, Sim/Combat/Aura.cs, caps a covered man's suppression and says nothing). The rallied man comes
// up from the cower to aim; the officer who held him off the floor plays a wave/shout gesture toward him. Beside
// AnimationController.Pin.cs: same pattern, a partial that reads sim state through GetSystem and never writes it.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;

namespace TW.Presentation
{
    public sealed partial class AnimationController
    {
        /// <summary>Before/after switch: the Play captures and the fails-on-old test turn this off to show the old code.</summary>
        public static bool DrawRally = true;
        /// <summary>Ticks an officer's rally covers a man for, so the gesture can follow a rally that landed a moment ago.</summary>
        const int RallyCoverTicks = 15;
        /// <summary>Ticks an officer waits between gestures, so he does not wave on every tick a man stays capped.</summary>
        const int RallyGestureCooldown = 90;

        RallyWatch rallyWatch;
        NativeArray<uint> rallyAt;      // tick a man was last seen on the rising edge (0: never)
        NativeArray<uint> lastGesture;  // tick an officer last played the rally gesture (0: never)

        /// <summary>The reader behind the draw, for a test or a probe to read Rallies / Gestures.</summary>
        public RallyWatch RallyWatch => rallyWatch;

        void RallyTick(SimWorld w)
        {
            if (rallyWatch == null)
            {
                rallyWatch = new RallyWatch(maxSlots);
                rallyAt = new NativeArray<uint>(maxSlots, Allocator.Persistent);
                lastGesture = new NativeArray<uint>(maxSlots, Allocator.Persistent);
            }
            rallyWatch.Tick(w);
            for (int i = 0; i < count; i++) if (rallyWatch.RalliedTick(i)) rallyAt[i] = tick;
        }

        /// <summary>True when the rally took the body this tick: the rallied man's get-up, or the officer's gesture.</summary>
        bool Rally(int i, ref AnimState s, SimWorld w)
        {
            if (rallyWatch == null) return false;
            if (RallyMan(i, ref s)) return true;
            return RallyOfficer(i, ref s, w);
        }

        bool RallyMan(int i, ref AnimState s)
        {
            if (!rallyWatch.RalliedTick(i)) return false;
            bool flat = s.Stance == (byte)Stance.Prone || s.Stance == (byte)Stance.Pinned;
            bool cowering = s.Clip == Clip.Shield || s.Clip == Clip.ProneIdle;
            if (!flat && !cowering) return false;
            s.WantStance = (byte)Stance.Crouch;
            s.Aimed = true;
            int officer = rallyWatch.Officer(i);
            if (officer >= 0) s.BodyYaw = s.AimYaw;   // faced the way he was looking; the stance ladder turns him to his target next
            Start(i, ref s, Clip.GetUp, Rung.Reaction, (i == FollowSlot ? "the officer has him up" : null));
            s.Stance = (byte)Stance.Crouch;
            return true;
        }

        bool RallyOfficer(int i, ref AnimState s, SimWorld w)
        {
            var spec = w.Units.Infantry[w.Archetype[i]];
            if (!spec.AuraUnpin || spec.AuraRadius <= 0f) return false;
            if (tick - lastGesture[i] < RallyGestureCooldown && lastGesture[i] != 0) return false;
            float sumX = 0f, sumZ = 0f; int n = 0;
            for (int j = 0; j < count; j++)
            {
                if (rallyWatch.Officer(j) != i || rallyAt[j] == 0 || tick - rallyAt[j] > RallyCoverTicks) continue;
                sumX += w.Position[j].x; sumZ += w.Position[j].z; n++;
            }
            if (n == 0) return false;
            float3 at = w.Position[i];
            float3 centroid = new float3(sumX / n, at.y, sumZ / n);
            float3 d = centroid - at; d.y = 0f;
            s.AimYaw = math.lengthsq(d) > 0.01f ? math.atan2(d.x, d.z) : s.AimYaw;
            lastGesture[i] = tick;
            rallyWatch.Gestures++;
            Start(i, ref s, Clip.Throw, Rung.Action, (i == FollowSlot ? "waves the men up" : null), 0.8f);
            return true;
        }

        void DisposeRally()
        {
            if (rallyAt.IsCreated) rallyAt.Dispose();
            if (lastGesture.IsCreated) lastGesture.Dispose();
            rallyWatch?.Dispose();
        }
    }
}
