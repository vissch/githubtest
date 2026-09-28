// Phase: C4 (2026-09-28, presentation only) — the Salvo's rockets. The sim fires ONE indirect shell per reload
// (TankGunnerySystem, TankSpec Gun0 of UnitDefinitions.Salvo), whose Impact bursts at its landing point on the tick it is
// fired. The picture is a rack going off: a row of rockets leaves the tubes one after another over about a second, each
// from its own tube, each trailing smoke on its own arc, and they come down scattered round the sim's landing point
// (VehicleFired's Pos) with a burst each. Nothing here touches the sim: the damage is the sim's one burst, which happens
// when the rack STARTS firing, so the rockets land 1-3 s after it. Making the burst wait for them is a sim change (a
// delayed impact, like the abilities' ScheduledPayload) the owner has not asked for.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        struct Rocket
        {
            public int Slot; public ushort Gen;    // the machine it leaves, looked up when it launches
            public int Tube;                       // which tube of the rack: its offset across the tube face
            public Vector3 From, To, Dir;          // set at launch; To is chosen at the order
            public float Launch, Flight, Apex, NextPuff;
            public bool Flying;
        }

        readonly List<Rocket> rockets = new List<Rocket>(32);
        const float RocketSpeed = 140f;     // m/s along the ground: 150-380 m takes 1.1-2.7 s
        const float RackSeconds = 1.0f;     // the whole rack goes off in this long
        const float TubeSpacing = 0.42f;    // metres between tube centres on the Salvo's 4 x 4 face

        /// <summary>A shot from a machine whose Machines row has rockets: queue the rack. False if it has none.</summary>
        bool Salvo(View v, Vector3 landing, float now)
        {
            int row = v.Archetype < modelRow.Length ? modelRow[v.Archetype] : -1;
            int count = row >= 0 ? Machines[row].Rockets : 0;
            if (count <= 0) return false;
            float spread = Mathf.Max(3f, Machine(null, v.Archetype).Gun0.HeRadius);
            for (int i = 0; i < count; i++)
            {
                Vector2 off = Random.insideUnitCircle * spread;
                rockets.Add(new Rocket
                {
                    Slot = v.Slot, Gen = v.Gen, Tube = i % 16,
                    To = landing + new Vector3(off.x, 0f, off.y),
                    Launch = now + RackSeconds * i / count + Random.Range(0f, 0.03f),
                });
            }
            return true;
        }

        void RocketsFrame(float now)
        {
            if (rockets.Count == 0) return;
            bool fx = books != null && books.Ready;
            for (int i = rockets.Count - 1; i >= 0; i--)
            {
                var r = rockets[i];
                if (!r.Flying)
                {
                    if (now < r.Launch) continue;
                    // it leaves its own tube, from where the rack points now; a machine dead or gone since fires nothing more
                    if (!views.TryGetValue(r.Slot, out var v) || v.Gen != r.Gen || v.Dead || v.Model.GunPart[0] < 0) { rockets.RemoveAt(i); continue; }
                    Vector3 muzzle = MuzzleWorld(v, 0, out Vector3 dir);
                    var gun = v.World[v.Model.GunPart[0]];
                    Vector3 right = gun.MultiplyVector(Vector3.right).normalized, up = gun.MultiplyVector(Vector3.up).normalized;
                    int cx = r.Tube % 4, cy = r.Tube / 4;
                    r.From = muzzle + right * ((cx - 1.5f) * TubeSpacing) + up * ((1.5f - cy) * TubeSpacing);
                    r.To.y = Ground(r.To.x, r.To.z);
                    float ground = new Vector2(r.To.x - r.From.x, r.To.z - r.From.z).magnitude;
                    r.Flight = Mathf.Clamp(ground / RocketSpeed, 0.8f, 3.5f);
                    r.Apex = Mathf.Max(8f, ground * 0.22f);
                    r.Dir = dir; r.Flying = true; r.NextPuff = now;
                    v.Recoil[0] = Mathf.Max(v.Recoil[0], 0.35f);
                    if (fx)
                    {
                        books.Add(FlipbookFx.Book.Flash, r.From + dir * 0.3f, 1.6f, 0.08f, roll: Random.value * 6.28f, glow: SceneMood.Night ? 3f : 1.6f);
                        books.Add(FlipbookFx.Book.Smoke, r.From - dir * 0.8f, 1.4f, 1.8f, velocity: -dir * 3f + Vector3.up * 0.4f, grow: 1.4f, alpha: 0.6f);
                    }
                }
                float k = (now - r.Launch) / r.Flight;
                if (k >= 1f)
                {
                    if (fx)
                    {
                        books.Add(FlipbookFx.Book.Burst, r.To + Vector3.up * 0.4f, 4.2f, 0.9f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, glow: SceneMood.Night ? 3f : 1.5f);
                        books.Add(FlipbookFx.Book.Smoke, r.To + Vector3.up * 1.2f, 3.2f, 3.5f, velocity: Vector3.up * 0.8f, grow: 1.2f, alpha: 0.5f);
                    }
                    CameraShake.Add(r.To, 1.5f);
                    rockets.RemoveAt(i); continue;
                }
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
