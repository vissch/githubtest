// Phase: B1 (implemented) — the camera shake a shell burst gives the picture. Moved out of CombatFx.cs on
// 2026-09-25 so it can be found by name; SceneStatics resets it when Play ends (tests read its statics).
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    /// <summary>
    /// A shell landing in the picture shakes the camera: a hard thump for each one (Jolt, gone in about half a second)
    /// and, while shells keep coming, a slower rumble under it that builds with every burst and dies away over a few
    /// seconds after the last (Rumble), so a barrage is felt as a barrage. How hard a shell shakes falls off with its
    /// distance from the middle of the view, measured against how much ground the view shows, so it reads the same at
    /// every zoom; bigger shells shake harder. Each burst also shoves the picture AWAY from it, a push that springs back,
    /// and none of it arrives before the sound would: a shell 100 m off is felt a third of a second after its flash.
    /// Runs after TacticalCamera has placed the camera for the frame (from scratch every frame, so the shake never
    /// accumulates), on unscaled time; a lightning freeze holds it still.
    /// </summary>
    [DefaultExecutionOrder(10000)]
    public sealed class CameraShake : MonoBehaviour
    {
        /// <summary>0 turns the shake off (a player setting), 1 as designed.</summary>
        public static float Strength = 1f;
        static float jolt, rumble;
        static Vector3 lookPoint, lens;
        static float viewDistance = 70f;
        /// <summary>m/s: how fast a burst's thump travels to the lens.</summary>
        public const float SoundSpeed = 343f;
        // a burst's kick on its way to the lens: where it pushes (away from the burst, on the ground), when it lands, how hard
        struct Kick { public Vector2 Away; public float At, Size; }
        static readonly Kick[] kicks = new Kick[24];
        static int kickCount;
        static Vector2 shove, shoveVel;   // the push, in hundredths of the view distance along the ground: a critically damped spring

        /// <summary>Seconds a burst this many metres from the lens takes to be felt there.</summary>
        public static float Arrival(float metres) => Mathf.Max(0f, metres) / SoundSpeed;

        /// <summary>Kicks still on their way (tests, and the stats overlay).</summary>
        public static int Pending => kickCount;

        /// <summary>How far (m, on the ground) a place is from the middle of the picture. Effects use it to spend their
        /// per-frame budget on what the player is looking at rather than on whatever the sim listed first.</summary>
        public static float DistanceToLook(Vector3 at) => new Vector2(at.x - lookPoint.x, at.z - lookPoint.z).magnitude;

        /// <summary>A burst of this radius (m) at this place.</summary>
        public static void Add(Vector3 at, float radius)
        {
            float reach = 16f + viewDistance * 0.9f;   // about the ground the picture shows
            float d = new Vector2(at.x - lookPoint.x, at.z - lookPoint.z).magnitude;
            float near = 1f - Mathf.SmoothStep(0f, 1f, d / reach);
            float a = near * Mathf.Clamp(radius / 8f, 0.35f, 1.9f);
            if (a <= 0.001f) return;
            if (lens == Vector3.zero && Camera.main != null) lens = Camera.main.transform.position;   // before the first LateUpdate
            Vector2 away = new Vector2(lookPoint.x - at.x, lookPoint.z - at.z);
            away = away.sqrMagnitude > 0.25f ? away.normalized : Vector2.zero;   // a burst under the middle of the picture only thumps
            var kick = new Kick { Away = away, At = Time.unscaledTime + Arrival(Vector3.Distance(at, lens)), Size = a };
            if (kickCount < kicks.Length) kicks[kickCount++] = kick;
            else Land(kick);   // a full queue lands the extra now rather than losing it
        }

        static void Land(in Kick k)
        {
            jolt = Mathf.Min(1f, jolt + k.Size * 0.75f);
            rumble = Mathf.Min(0.75f, rumble + k.Size * 0.2f);
            shoveVel += k.Away * (k.Size * 24f);   // peaks near one unit (a hundredth of the view distance) for a heavy shell mid-picture
        }

        void LateUpdate()
        {
            using var perf = TW.Sim.PerfMarkers.FxShake.Auto();
            var t = transform;
            viewDistance = t.position.y / Mathf.Max(0.15f, -t.forward.y);
            lookPoint = t.position + t.forward * viewDistance;
            lens = t.position;
            if (Time.timeScale <= 0.001f) return;   // frozen by lightning: the picture holds still (the kicks wait)
            float clock = Time.unscaledTime, dt = Time.unscaledDeltaTime;
            for (int k = kickCount - 1; k >= 0; k--)
                if (kicks[k].At <= clock) { Land(kicks[k]); kicks[k] = kicks[--kickCount]; }
            // the push springs back, critically damped (about a fifth of a second out and back), and never runs away
            const float omega = 9f;
            shoveVel += (-omega * omega * shove - 2f * omega * shoveVel) * Mathf.Min(dt, 0.05f);
            shove += shoveVel * Mathf.Min(dt, 0.05f);
            if (shove.sqrMagnitude > 4f) shove = shove.normalized * 2f;
            bool shoving = shove.sqrMagnitude > 1e-6f || shoveVel.sqrMagnitude > 1e-4f;
            if (shoving)
            {
                // moved along the ground away from the burst, and tipped with it (the top of the picture goes first)
                var push = new Vector3(shove.x, 0f, shove.y) * Strength;
                t.position += push * (0.01f * viewDistance);
                t.rotation = Quaternion.AngleAxis(push.magnitude * 1.2f, Vector3.Cross(Vector3.up, push).normalized) * t.rotation;
            }
            if (jolt <= 0f && rumble <= 0f) return;
            float s = jolt * jolt * Strength, r = rumble * rumble * Strength;
            float fast = clock * 23f, slow = clock * 6.5f;
            // degrees: the thump is quick and sharp, the rumble slow and heavy; a little sideways shove on top, in proportion
            // to how far the ground is so it shows at every zoom
            float pitch = N(fast, 1.3f) * 2.6f * s + N(slow, 4.1f) * 2.0f * r;
            float yaw = N(fast, 2.9f) * 2.1f * s + N(slow, 6.7f) * 1.6f * r;
            float roll = N(fast, 5.3f) * 3.0f * s + N(slow, 8.2f) * 1.8f * r;
            t.position += (t.right * N(fast, 9.1f) + t.up * N(fast, 11.7f)) * (0.011f * viewDistance * s);
            t.rotation *= Quaternion.Euler(pitch, yaw, roll);
            jolt = Mathf.Max(0f, jolt - dt * 1.7f);
            rumble = Mathf.Max(0f, rumble - dt * 0.3f);
        }

        static float N(float x, float y) => (Mathf.PerlinNoise(x, y) - 0.5f) * 2f;

        /// <summary>Back to a camera that has felt nothing, looking at the origin. Strength is a player setting and
        /// stays. Runs when Play ends (SceneStatics.ResetSession): the look point used to survive into EditMode tests
        /// and drop every burst as "too far from the picture".</summary>
        public static void Reset()
        {
            jolt = 0f; rumble = 0f; kickCount = 0;
            lookPoint = Vector3.zero; lens = Vector3.zero; viewDistance = 70f;
            shove = Vector2.zero; shoveVel = Vector2.zero;
        }

        static CameraShake() => SceneStatics.Register(nameof(CameraShake), Reset);
    }
}
