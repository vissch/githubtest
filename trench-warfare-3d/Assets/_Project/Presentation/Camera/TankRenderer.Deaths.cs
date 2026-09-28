// Phase: deaths (2026-09-28, implemented) — part of TankRenderer: a machine's death made absurd (owner, 2026-09-28:
// slapstick; fx.deathAbsurd, DeathGags.Intensity, 0 = today's death exactly: nothing here runs). On top of Wreckify:
//  - the turret (or the cupola) leaps straight up at 17-21 m/s, turning end over end about the hull's side axis in
//    whole flips, and comes down with two bounces within 1.5 hull lengths (a cook-off's throw is taken over);
//  - the hull hops a metre and comes down with a bump; a walker holds its death pose a beat first, then drops;
//  - up to four of a machine's road wheels roll away 8-14 m along its length, spreading out, then topple flat.
// The flight is VehicleGags' (pure, tested); the dice are DebrisRng's, seeded by where it died, never UnityEngine.Random.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        sealed partial class View
        {
            /// <summary>The hull's hop: its launch speed (0: none), when it leaves, and the height it leaves from.</summary>
            public float HopSpeed, HopAt, HopBase;
        }

        sealed partial class Debris
        {
            /// <summary>The share of its fall speed it keeps on each of its next Bounces landings (then FlyDebris' 0.25).</summary>
            public float Bounce = 0.25f; public int Bounces;
            /// <summary>Seconds a wheel still rolls on its rim before it topples (0: it flies as any piece).</summary>
            public float Roll;
        }

        /// <summary>The absurd death, once, as the machine becomes a wreck (Wreckify, fx.deathAbsurd above 0).</summary>
        void DeathGag(View v, float now)
        {
            float a = DeathGags.Intensity;
            var rng = new DebrisRng(v.Pos, 0xDEADu + (uint)Mathf.Max(0, v.Slot));
            var lod = v.Model.Lods[0];
            var parts = lod.Parts;
            Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
            float hullLength = v.Model.HalfLength * 2f;

            // the turret: straight up, end over end, two bounces
            int top = -1;
            for (int i = 1; i < parts.Count && top < 0; i++) if (parts[i].Role == TankPartRole.Turret) top = i;
            for (int i = 1; i < parts.Count && top < 0; i++) if (parts[i].Role == TankPartRole.Cupola) top = i;
            if (top >= 0)
            {
                Debris d = null;
                if (v.Off[top]) { foreach (var p in v.Pieces) if (p.Part == top) d = p; }   // the cook-off threw it: take it over
                else d = Detach(v, top, v.World[top]);
                if (d != null)
                {
                    float turn = rng.Range(0f, 2f * Mathf.PI);
                    var leap = VehicleGags.TurretLeap(a, hullLength, new Vector3(Mathf.Sin(turn), 0f, Mathf.Cos(turn)), right, rng.Next(), rng.Next(), rng.Next());
                    d.Vel = leap.Vel; d.Spin = leap.Spin; d.Resting = false;
                    d.Bounce = VehicleGags.TurretBounce; d.Bounces = VehicleGags.TurretBounces;
                    d.Burn = Mathf.Max(d.Burn, v.Burn);
                }
            }

            // the wheels: a few roll away along its length, spreading out
            int rollers = 0;
            for (int i = 1; i < parts.Count && rollers < VehicleGags.MaxRollers; i++)
            {
                if (parts[i].Role != TankPartRole.Wheel || v.Off[i] || rng.Next() < 0.5f) continue;
                var d = Detach(v, i, v.World[i]);
                Vector3 at = (Vector3)v.World[i].GetColumn(3) - v.Pos;
                float side = Vector3.Dot(at, right) >= 0f ? 1f : -1f, ahead = Vector3.Dot(at, fwd) >= 0f ? 1f : -1f;
                Vector3 dir = (fwd * ahead + right * (side * rng.Range(0.2f, 0.5f))).normalized;
                float distance = VehicleGags.RollDistance(rng.Next());
                d.Vel = dir * VehicleGags.RollSpeed(distance);
                d.Roll = VehicleGags.RollSeconds(distance) + 0.5f;
                rollers++;
            }

            // the hull: a hop (a walker holds a beat first)
            v.HopSpeed = VehicleGags.HopSpeed(a);
            v.HopAt = now + (v.Model.LegCount > 0 ? VehicleGags.WalkerFreeze : 0f);
            v.HopBase = v.Heave.Value;
        }

        /// <summary>Once a frame, after the wrecks smoulder: the hulls still hopping.</summary>
        void HopsFrame(float now)
        {
            foreach (var v in wrecks)
            {
                if (v.HopSpeed <= 0f) continue;
                float t = now - v.HopAt;
                if (t < 0f) continue;
                if (t >= VehicleGags.HopSeconds(v.HopSpeed))
                {
                    v.Heave.Value = v.HopBase; v.HopSpeed = 0f;
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, v.Pos + Vector3.up * 0.3f, v.Model.HalfLength * 1.4f, 1.1f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.4f, grow: 0.8f, alpha: 0.6f);
                    CameraShake.Add(v.Pos, 3f);
                    continue;
                }
                v.Heave.Value = v.HopBase + VehicleGags.Hop(v.HopSpeed, t);
            }
        }

        /// <summary>What a landing piece keeps of its fall speed (FlyDebris): its own share for its next few bounces.</summary>
        static float BounceOf(Debris d)
        {
            if (d.Bounces <= 0) return 0.25f;
            d.Bounces--;
            return d.Bounce;
        }

        /// <summary>A wheel on its rim: it rolls along, slowing, turning about its axle; when it runs out it topples over.</summary>
        void RollWheel(Debris d, float dt)
        {
            var model = d.Owner.Model;
            var p = model.Lods[0].Parts[d.Part];
            Vector3 pos = d.World.GetColumn(3);
            Quaternion rot = d.World.rotation;
            Vector3 flat = new Vector3(d.Vel.x, 0f, d.Vel.z);
            float speed = flat.magnitude;
            d.Roll -= dt;
            if (speed < 0.5f || d.Roll <= 0f)
            {
                // out of roll: over onto its face, and FlyDebris takes it from here
                Vector3 dir = speed > 1e-3f ? flat / speed : Vector3.forward;
                d.Roll = 0f;
                d.Vel = dir * speed + Vector3.up * 1.2f;
                d.Spin = dir * 2.4f;
                return;
            }
            float slowed = Mathf.Max(0.01f, speed - VehicleGags.RollDecel * dt);
            flat *= slowed / speed;
            d.Vel = flat;
            pos += flat * dt;
            float radius = Mathf.Max(0.15f, model.WheelRadius);
            rot = Quaternion.AngleAxis(slowed / radius * Mathf.Rad2Deg * dt, Vector3.Cross(Vector3.up, flat / slowed)) * rot;
            Vector3 centre = pos + rot * p.Center;
            pos.y += Ground(centre.x, centre.z) + radius - centre.y;   // on its rim
            d.World = Matrix4x4.TRS(pos, rot, Vector3.one);
        }
    }
}
