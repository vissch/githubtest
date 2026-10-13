// Phase: anim-critique-camera (2026-10-13, lane/show/anim-critique) — a machine on wheels or tracks, driven
// Drives a VehicleRig whose tank3.json declares no walker, flyer or hopper but whose model has running gear
// (Wheel_* or Track_* parts): the Mercy, the Brute. Before this, `walk` only reached the leg and flight drives, so
// Mercy.trial.game-moves stood still for twelve seconds though it was told to drive (the critics, 2026-10-06).
//
// Built on FlyerDrive's shape, and for the same reason: only the Hull is moved, in the rig's own frame, so a part
// shot off still flies from where it was and the three LOD copies stay identical (nothing reads a world matrix).
// It runs after VehicleRig.Pose, so it ADDS to the pose Sag left (a machine that has lost a wheel keeps its lean).
// `Speed > 0` walks it round a circle of Radius, nose along the circle, so it drives without leaving the stage's
// frame; `InPlace` spins the wheels and bobs only. Knocked out, it coasts to a stop.
using UnityEngine;

namespace TW.Playground
{
    public sealed class GroundDrive : MonoBehaviour
    {
        public float Speed;                         // m/s along a circle of Radius (0: stands still)
        public float Radius = 20f;
        public bool InPlace;                        // drive on the spot: the wheels turn, the hull does not travel
        public float T { get; private set; }         // its own clock: only dt enters, so copies agree

        VehicleRig rig;
        VehicleRig.Part hull;
        VehicleRig.Part[] gear;                      // Wheel_* and Track_*, in the order VehicleRig built them
        float[] spin;                                // each one's angle about its own x (degrees)
        float angle, carried, kick;

        public GroundDrive Init(VehicleRig r)
        {
            rig = r; hull = r.Find("Hull");
            gear = System.Array.FindAll(r.Parts.ToArray(), p => p.Name.StartsWith("Wheel_") || p.Name.StartsWith("Track_"));
            spin = new float[gear.Length];
            return this;
        }

        /// <summary>One frame, after VehicleRig's pose.</summary>
        public void Drive(float dt)
        {
            if (rig == null || hull == null || hull.Loose) return;
            T += dt;
            float size = rig.Size;
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            // what it is actually doing now: it takes a moment to get going, and coasts to a stop when it is knocked out
            float want = dead ? 0f : Speed;
            float was = carried;
            carried = Mathf.MoveTowards(carried, want, (want > carried ? 3f : 2.5f) * dt);
            // where it has driven to: round a circle about the rig's middle, its nose along the circle. [A5] r must
            // not go to 0 just because it has coasted to a stop (carried <= 0.01): that snapped the hull back to its
            // start every time it stopped, instead of leaving it where it coasted to. Only InPlace (no travel at all)
            // keeps r at 0; the angle itself only advances while it is actually carried.
            float r = InPlace ? 0f : Radius / size;
            if (carried > 0.01f) angle += carried / Radius * dt;
            Vector3 at = r > 0f ? new Vector3(Mathf.Cos(angle) * r - r, 0f, Mathf.Sin(angle) * r) : Vector3.zero;
            float heading = r > 0f ? -angle * Mathf.Rad2Deg : 0f;
            // the nose lifts as it pulls away and dips as it stops, and the hull rocks 2 cm on its springs
            float accel = dt > 1e-5f ? (carried - was) / dt : 0f;
            kick = Mathf.Lerp(kick, Mathf.Clamp(-accel * 0.8f, -3.5f, 3.5f), 1f - Mathf.Exp(-dt * 5f));
            float bob = carried > 0.01f ? Mathf.Sin(T * 2.3f) * 0.02f / size : 0f;
            // the running gear turns with the ground it covers (a wheel of radius a, in the part's own frame)
            for (int i = 0; i < gear.Length; i++)
            {
                var p = gear[i];
                // [A6] a standing rig (carried 0, never spun) must not have its wheel pose overwritten every frame:
                // that replaced VehicleRig.Pose's Sag-left lean with RestRot even when nothing was driving at all.
                if (carried <= 0.01f && spin[i] == 0f) continue;
                float wheel = Mathf.Max(0.15f, p.Box.extents.y) * size;
                spin[i] = Mathf.Repeat(spin[i] + carried / wheel * Mathf.Rad2Deg * dt, 360f);
                if (!p.Loose) p.T.localRotation = p.RestRot * Quaternion.Euler(spin[i], 0f, 0f);
            }
            // added to the pose Sag left this frame, never replacing it
            hull.T.localPosition += at + new Vector3(0f, bob, 0f);
            hull.T.localRotation = Quaternion.Euler(kick, heading, 0f) * hull.T.localRotation;
        }
    }
}
