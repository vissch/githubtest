// Phase: Playground (2026-09-27, lane/show/playground) — a flying machine on VehicleRig
// Flies a VehicleRig whose tank3.json says "flyer" (Tools/mechsplit.py TW_KIND=flyer: Hull > Wing > Engine, Tail, Skids,
// Turret). The rig itself stays on the ground; only the Hull is lifted and moved, in the rig's own frame, so everything
// VehicleRig does still holds: a part shot off starts its flight from where it was in the air and falls to the ground
// (Tumble, the rig's frame), the fire and smoke ride the hull's sockets, and three copies at three LODs are posed
// identically (nothing is read from a world matrix).
//
// Hovering it bobs and sways; `fly <speed>` circles it, banked into the turn. Knocked out, it falls nose down and
// turning, and comes to rest on its skids where it hits.
using UnityEngine;

namespace TW.Playground
{
    public sealed class FlyerDrive : MonoBehaviour
    {
        public float Altitude = 14f;               // metres over the ground
        public float Speed;                        // m/s along a circle of Radius (0: hovers in place)
        public float Radius = 20f;
        public float T { get; private set; }        // its own clock: only dt enters, so copies agree

        VehicleRig rig;
        VehicleRig.Part hull;
        Vector3 rest;
        float angle, fallV, fallSpin, fallYaw, fallPitch, crashY = -1f;
        bool down;

        public FlyerDrive Init(VehicleRig r)
        {
            rig = r; hull = r.Find("Hull"); rest = hull.RestLocal;
            return this;
        }

        /// <summary>One frame, after VehicleRig's pose.</summary>
        public void Drive(float dt)
        {
            if (rig == null || hull == null || hull.Loose) return;
            T += dt;
            float size = rig.Size;
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            // where it is flying: round a circle about the rig's middle, its nose along the circle
            float r = Speed > 0.01f ? Radius / size : 0f;
            if (!dead) angle += (Speed > 0.01f ? Speed / Radius : 0f) * dt;
            Vector3 at = r > 0f ? new Vector3(Mathf.Cos(angle) * r - r, 0f, Mathf.Sin(angle) * r) : Vector3.zero;
            float heading = r > 0f ? -angle * Mathf.Rad2Deg : 0f;
            float bank = r > 0f ? Mathf.Clamp(Speed * Speed / Radius * 3f, 0f, 25f) : 0f;
            float bob = Mathf.Sin(T * 1.3f) * 0.35f + Mathf.Sin(T * 0.47f) * 0.2f;
            float sway = Mathf.Sin(T * 0.8f) * 3f;
            if (!dead)
            {
                float y = Altitude / size + bob / size;
                hull.T.localPosition = rest + at + new Vector3(0f, y, 0f);
                hull.T.localRotation = Quaternion.Euler(Mathf.Sin(T * 0.9f) * 2f, heading, -bank + sway);
                crashY = y; fallV = 0f;
                return;
            }
            // knocked out: it drops, nose down and turning, and stops on the ground
            if (!down)
            {
                fallV += 9.81f / size * dt;
                crashY = Mathf.Max(0f, crashY - fallV * dt);
                fallSpin = Mathf.Min(140f, fallSpin + 120f * dt);
                fallYaw += fallSpin * dt;
                fallPitch = Mathf.Min(24f, fallPitch + 30f * dt);
                if (crashY <= 0f) { down = true; fallPitch = 6f; if (rig.Fx != null) rig.Fx.Burst(hull.T.position, 2.5f * size); }
            }
            var p = hull.T.localPosition; p.y = rest.y + crashY;
            hull.T.localPosition = p;
            hull.T.localRotation = Quaternion.Euler(fallPitch, heading + fallYaw, down ? 4f : -bank);
        }
    }
}
