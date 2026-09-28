// Phase: C4 (2026-09-28) — the Salvo's rockets, drawn where and when the sim flies them. TankGunnerySystem fires a rack
// (TankSpec.Rockets) as one RocketFired event per rocket on the tick the rack fires: where it comes down, the ticks until
// it leaves its tube and the ticks it flies. Its burst is queued for that landing tick, and CombatFx draws the burst from
// the Explosion event as it draws any shell. This file draws the rest: each rocket leaving its own tube, its arc and
// its smoke trail, timed on the sim's own clock (World.Tick + Alpha) so it reaches the ground on the frame the burst is
// shown, whatever the frame rate or the time scale.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        struct Rocket
        {
            public int Slot; public ushort Gen;    // the machine it leaves
            public int Tube;                       // which tube of the rack: its offset across the tube face
            public Vector3 From, To, Dir;          // From and Dir are taken at launch
            public float LaunchAt, LandAt;         // on the sim clock (ticks): when it leaves the tube, when it bursts
            public float Apex, NextPuff;
            public bool Flying;
        }

        readonly List<Rocket> rockets = new List<Rocket>(32);
        const float TubeSpacing = 0.42f;    // metres between tube centres on the Salvo's 4 x 4 face

        /// <summary>The sim clock the rockets are timed on, in ticks: a burst queued for tick L is shown from the first
        /// frame at which this reaches L + 1 (World.Tick counts the ticks already run).</summary>
        float SimClock => Host != null && Host.Local != null ? Host.Local.World.Tick + Host.Alpha : 0f;

        /// <summary>One rocket of a rack (SimEventType.RocketFired).</summary>
        void RocketFired(in SimEvent e)
        {
            if (!views.TryGetValue(e.A, out var v) || v.Dead) return;
            rockets.Add(new Rocket
            {
                Slot = v.Slot, Gen = v.Gen, Tube = e.B % 16, To = (Vector3)e.Pos,
                LaunchAt = e.Tick + e.Dir.x + 1f, LandAt = e.Tick + e.Dir.x + e.Dir.y + 1f,
            });
        }

        void RocketsFrame(float now)
        {
            if (rockets.Count == 0) return;
            bool fx = books != null && books.Ready;
            float clock = SimClock;
            for (int i = rockets.Count - 1; i >= 0; i--)
            {
                var r = rockets[i];
                if (!r.Flying)
                {
                    if (clock < r.LaunchAt) continue;
                    r.To.y = Ground(r.To.x, r.To.z);
                    // it leaves its own tube, from where the rack points now; if the machine has gone, from where it was
                    if (views.TryGetValue(r.Slot, out var v) && v.Gen == r.Gen && !v.Dead && v.Model.GunPart[0] >= 0)
                    {
                        Vector3 muzzle = MuzzleWorld(v, 0, out Vector3 dir);
                        var gun = v.World[v.Model.GunPart[0]];
                        Vector3 right = gun.MultiplyVector(Vector3.right).normalized, up = gun.MultiplyVector(Vector3.up).normalized;
                        int cx = r.Tube % 4, cy = (r.Tube / 4) % 4;
                        r.From = muzzle + right * ((cx - 1.5f) * TubeSpacing) + up * ((1.5f - cy) * TubeSpacing);
                        r.Dir = dir;
                        v.Recoil[0] = Mathf.Max(v.Recoil[0], 0.35f);
                    }
                    else { r.From = r.To + Vector3.up * 40f; r.Dir = Vector3.down; }
                    float ground = new Vector2(r.To.x - r.From.x, r.To.z - r.From.z).magnitude;
                    r.Apex = Mathf.Max(8f, ground * 0.22f);
                    r.Flying = true; r.NextPuff = now;
                    if (fx)
                    {
                        books.Add(FlipbookFx.Book.Flash, r.From + r.Dir * 0.3f, 1.6f, 0.08f, roll: Random.value * 6.28f, glow: SceneMood.Night ? 3f : 1.6f);
                        books.Add(FlipbookFx.Book.Smoke, r.From - r.Dir * 0.8f, 1.4f, 1.8f, velocity: -r.Dir * 3f + Vector3.up * 0.4f, grow: 1.4f, alpha: 0.6f);
                    }
                }
                float k = (clock - r.LaunchAt) / Mathf.Max(0.5f, r.LandAt - r.LaunchAt);
                if (k >= 1f) { rockets.RemoveAt(i); continue; }   // it is down: the sim's burst, drawn by CombatFx, is this frame's
                // on its arc: a straight line to the landing point, lifted by a parabola that peaks halfway
                Vector3 at = Vector3.Lerp(r.From, r.To, k) + Vector3.up * (4f * r.Apex * k * (1f - k));
                if (fx)
                {
                    books.Add(FlipbookFx.Book.Flash, at, 0.9f, 0.05f, roll: Random.value * 6.28f, glow: SceneMood.Night ? 3f : 2f);   // the motor
                    if (now >= r.NextPuff)
                    {
                        r.NextPuff = now + 0.035f;
                        books.Add(FlipbookFx.Book.Smoke, at, 0.8f, 2.2f, (i & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: Vector3.up * 0.3f, grow: 1.8f, alpha: 0.45f);
                    }
                }
                rockets[i] = r;
            }
        }
    }
}
