// Phase: Playground (2026-09-27, lane/show/playground) — a flying machine on VehicleRig
// Flies a VehicleRig whose tank3.json says "flyer" (Tools/mechsplit.py TW_KIND=flyer: Hull > Wing > Engine, Tail, Skids,
// Turret). The rig itself stays on the ground; only the Hull is lifted and moved, in the rig's own frame, so everything
// VehicleRig does still holds: a part shot off starts its flight from where it was in the air and falls to the ground
// (Tumble, the rig's frame), the fire and smoke ride the hull's sockets, and three copies at three LODs are posed
// identically (nothing is read from a world matrix).
//
// Hovering it bobs and sways; `fly <speed>` circles it, banked into the turn. Knocked out, it falls nose down and
// turning, and comes to rest on its skids where it hits. A hovercraft (tank3.json "hover") is the same drive held just
// off the ground: a small bob and roll, its fan spinning, and knocked out it settles flat onto its pods.
using UnityEngine;

namespace TW.Playground
{
    public sealed class FlyerDrive : MonoBehaviour
    {
        public float Altitude = 14f;               // metres over the ground
        public float Speed;                        // m/s along a circle of Radius (0: hovers in place)
        public float Radius = 20f;
        public float T { get; private set; }        // its own clock: only dt enters, so copies agree
        public bool Hover;                         // a hovercraft: rides Altitude (0.45 m) over the ground, settles when dead
        VehicleRig.Part fan; float fanAngle, fanSpeed, nextSpray;
        VehicleRig.Part[] pods;

        VehicleRig rig;
        VehicleRig.Part hull;
        Vector3 rest;
        float angle, fallV, fallSpin, fallYaw, fallPitch, crashRoll, crashY = -1f;
        bool down;

        public FlyerDrive Init(VehicleRig r)
        {
            rig = r; hull = r.Find("Hull"); rest = hull.RestLocal;
            Hover = r.Manifest.hover; fan = r.Find("Fan");
            pods = System.Array.FindAll(r.Parts.ToArray(), p => p.Name.StartsWith("Pod_"));
            if (Hover) { Altitude = 0.45f; Radius = 16f; }
            return this;
        }

        /// <summary>One frame, after VehicleRig's pose.</summary>
        public void Drive(float dt)
        {
            if (rig == null || hull == null || hull.Loose) return;
            T += dt;
            float size = rig.Size;
            // a flyer that has lost a wing or an engine cannot stay up: it goes down as if knocked out (it hovered level on
            // one engine, loop 2 r36)
            if (!Hover && rig.State < VehicleRig.Stage.KnockedOut)
                foreach (var part in rig.Parts)
                    if (part.Loose && (part.Name.StartsWith("Wing") || part.Name.StartsWith("Engine"))) { rig.KnockOut(); break; }
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            // where it is flying: round a circle about the rig's middle, its nose along the circle
            float r = Speed > 0.01f ? Radius / size : 0f;
            if (!dead) angle += (Speed > 0.01f ? Speed / Radius : 0f) * dt;
            Vector3 at = r > 0f ? new Vector3(Mathf.Cos(angle) * r - r, 0f, Mathf.Sin(angle) * r) : Vector3.zero;
            float heading = r > 0f ? -angle * Mathf.Rad2Deg : 0f;
            // banked as an aircraft turns (the angle of v^2 / r g), and more, so it reads: 7 degrees read as level (loop 2 r34)
            float bank = r > 0f && !Hover ? Mathf.Clamp(Mathf.Atan(Speed * Speed / (Radius * 9.81f)) * Mathf.Rad2Deg * 1.5f, 15f, 30f)
                       : r > 0f ? Mathf.Clamp(Speed * Speed / Radius * 3f, 0f, 8f) : 0f;
            float amp = Hover ? 0.15f : 1f;
            float bob = (Mathf.Sin(T * 1.3f) * 0.35f + Mathf.Sin(T * 0.47f) * 0.2f) * amp;
            float sway = Mathf.Sin(T * 0.8f) * 3f * (Hover ? 0.5f : 1f);
            // the fan runs while it lives and winds down when it dies (spins about the machine's length)
            fanSpeed = Mathf.MoveTowards(fanSpeed, dead ? 0f : 900f + 60f * Speed, (dead ? 250f : 600f) * dt);
            fanAngle = Mathf.Repeat(fanAngle + fanSpeed * dt, 360f);
            if (fan != null && !fan.Loose) fan.T.localRotation = Quaternion.Euler(0f, 0f, fanAngle);
            // a hovercraft blows the ground out from under its pods (loop 2 r32: "at 78 m it reads as a parked truck")
            if (Hover && !dead && rig.Fx != null && T >= nextSpray)
            {
                nextSpray = T + (Speed > 0.5f ? 0.12f : 0.25f);
                foreach (var pod in pods)
                {
                    if (pod.Loose) continue;
                    var c = pod.T.TransformPoint(pod.Box.center); var hc = hull.T.position;
                    var outward = new Vector3(c.x - hc.x, 0f, c.z - hc.z).normalized;
                    rig.Fx.Spray(new Vector3(c.x, rig.GroundY + 0.2f * size, c.z), outward, 1.6f * size, Speed > 0.5f ? 0.45f : 0.28f);
                }
            }
            if (!dead)
            {
                float y = Altitude / size + bob / size;
                hull.T.localPosition = rest + at + new Vector3(0f, y, 0f);
                hull.T.localRotation = Quaternion.Euler(Mathf.Sin(T * 0.9f) * 2f, heading, -bank + sway);
                crashY = y; fallV = 0f;
                return;
            }
            // knocked out: a hovercraft sinks onto its pods, a flyer drops nose down and turning and stops on the ground
            if (Hover)
            {
                crashY = Mathf.MoveTowards(crashY, 0f, 0.6f / size * dt);
                var q = hull.T.localPosition; q.y = rest.y + crashY; hull.T.localPosition = q;
                hull.T.localRotation = Quaternion.Euler(1.5f, heading, 2f);
                return;
            }
            if (!down)
            {
                fallV += 9.81f / size * dt;
                crashY = Mathf.Max(0f, crashY - fallV * dt);
                fallSpin = Mathf.Min(140f, fallSpin + 120f * dt);
                fallYaw += fallSpin * dt;
                fallPitch = Mathf.Min(24f, fallPitch + 30f * dt);
                // it noses in and digs in: level on the ground it read as parked, not crashed (loop 2 r34)
                if (crashY <= 0f)
                {
                    down = true; fallPitch = 16f; crashRoll = (rig.Seed & 1) == 0 ? 8f : -8f; crashY = -0.3f / size;
                    if (rig.Fx != null) rig.Fx.Burst(hull.T.position, 2.5f * size);
                }
            }
            var p = hull.T.localPosition; p.y = rest.y + crashY;
            hull.T.localPosition = p;
            hull.T.localRotation = Quaternion.Euler(fallPitch, heading + fallYaw, down ? crashRoll : -bank);
        }
    }
}
