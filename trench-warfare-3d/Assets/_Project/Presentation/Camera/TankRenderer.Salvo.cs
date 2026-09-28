// Phase: C4 (2026-09-28) — the Salvo's rockets, drawn where and when the sim flies them. TankGunnerySystem fires a rack
// (TankSpec.Rockets) as one RocketFired event per rocket on the tick the rack fires: where it comes down, the ticks until
// it leaves its tube and the ticks it flies. Its burst is queued for that landing tick, and CombatFx draws the burst from
// the Explosion event as it draws any shell. This file draws the rest, timed on the sim's own clock (World.Tick + Alpha)
// so each rocket reaches the ground on the frame its burst is shown, whatever the frame rate or the time scale:
//   the launch: a flash at its own tube's mouth (Socket_Tube## from Tools/mechsplit.py), and for the rack's first rocket
//     the back-blast: a cloud of smoke round the rack and dust thrown up off the ground under it;
//   the flight: a rocket body (a small mesh along its velocity, in the machine's own material), its motor's flame along
//     the flight, and a smoke trail laid by distance travelled, a puff every TrailStep metres, so it is one unbroken
//     ribbon at any speed or frame rate (puffs by time drew a dotted line: the critic's round, docs/22);
//   the landing: earth thrown up and dust, on top of the sim's own burst.
// The critic's round (2026-09-28) replaced a flash per frame (a line of sparks) and a 4 x 4 guess at the tubes.
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
            public int Tube;                       // which tube of the rack
            public bool First;                     // the rack's first rocket: it throws the back-blast
            public Vector3 From, To, Dir, Trail;   // From and Dir are taken at launch; Trail: where the last puff was laid
            public float LaunchAt, LandAt;         // on the sim clock (ticks): when it leaves the tube, when it bursts
            public float Apex;
            public bool Flying;
        }

        readonly List<Rocket> rockets = new List<Rocket>(32);
        const float TrailStep = 0.7f;       // metres between trail puffs: they overlap into a ribbon
        const float RocketLength = 1.5f, RocketRadius = 0.16f;
        const float LaunchKick = 0.25f;     // how far a rocket leaving rocks the rack (Recoil, 0..1)
        const float ApexShare = 0.22f;      // an arc's height as a share of the ground it covers (at least MinApex)
        const float MinApex = 8f;
        Mesh rocketMesh; Material rocketMat;

        /// <summary>The sim clock the rockets are timed on, in ticks: a burst queued for tick L is shown from the first
        /// frame at which this reaches L + 1 (World.Tick counts the ticks already run).</summary>
        float SimClock => Host != null && Host.Local != null ? Host.Local.World.Tick + Host.Alpha : 0f;

        /// <summary>One rocket of a rack (SimEventType.RocketFired).</summary>
        void RocketFired(in SimEvent e)
        {
            if (!views.TryGetValue(e.A, out var v) || v.Dead) return;
            rockets.Add(new Rocket
            {
                Slot = v.Slot, Gen = v.Gen, Tube = e.B, First = e.B == 0, To = (Vector3)e.Pos,
                LaunchAt = e.Tick + e.Dir.x + 1f, LandAt = e.Tick + e.Dir.x + e.Dir.y + 1f,
            });
        }

        /// <summary>Where rocket k leaves the rack: the mouth of its own tube, or the muzzle if the model has no tube sockets.</summary>
        Vector3 TubeMouth(View v, int k, out Vector3 dir)
        {
            Vector3 muzzle = MuzzleWorld(v, 0, out dir);
            if (!v.Model.IsRack) return muzzle;
            // the mouths hang off the Hull, as the rack stands at rest (Tools/mechsplit.py: deeper sockets do not survive
            // the import), so each is taken into the rack's own frame at rest and carried on the rack as it is posed now
            var s = v.Model.Tubes[k % v.Model.Tubes.Length];
            var parts = v.Model.Lods[0].Parts;
            int gun = v.Model.GunPart[0];
            Vector3 rest = Vector3.zero;
            for (int p = gun; p >= 0 && p != s.part; p = parts[p].Parent) rest += parts[p].Local;
            return v.World[gun].MultiplyPoint3x4(s.local - rest);
        }

        void BuildRocket()
        {
            // an eight-sided tube with a pointed nose, along +z, its tail at the origin
            var vs = new List<Vector3>(); var tris = new List<int>();
            const int sides = 8;
            for (int k = 0; k < sides; k++)
            {
                float a = k * Mathf.PI * 2f / sides;
                var ring = new Vector3(Mathf.Cos(a) * RocketRadius, Mathf.Sin(a) * RocketRadius, 0f);
                vs.Add(ring); vs.Add(ring + Vector3.forward * (RocketLength * 0.8f));
            }
            vs.Add(Vector3.forward * RocketLength); vs.Add(Vector3.zero);
            int nose = sides * 2, tail = nose + 1;
            for (int k = 0; k < sides; k++)
            {
                int a0 = k * 2, a1 = a0 + 1, b0 = (k + 1) % sides * 2, b1 = b0 + 1;
                tris.AddRange(new[] { a0, b0, a1, a1, b0, b1, a1, b1, nose, b0, a0, tail });
            }
            rocketMesh = new Mesh { name = "Salvo rocket", hideFlags = HideFlags.HideAndDontSave };
            rocketMesh.SetVertices(vs); rocketMesh.SetTriangles(tris, 0); rocketMesh.RecalculateNormals(); rocketMesh.RecalculateBounds();
            var shader = Shader.Find("TW/Tank (URP)");
            if (shader == null) return;
            var tex = new Texture2D(1, 1) { hideFlags = HideFlags.HideAndDontSave };
            tex.SetPixel(0, 0, new Color(0.30f, 0.29f, 0.24f)); tex.Apply();
            rocketMat = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, name = "Salvo rocket" };
            rocketMat.SetTexture("_BaseMap", tex);
            rocketMat.SetFloat("_OutlineWidth", 1.2f);
        }

        void RocketsFrame(float now)
        {
            if (rockets.Count == 0) return;
            if (rocketMesh == null) BuildRocket();
            bool fx = books != null && books.Ready;
            var cam = Camera.main;
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
                        r.From = TubeMouth(v, r.Tube, out r.Dir);
                        v.Recoil[0] = Mathf.Max(v.Recoil[0], LaunchKick);
                    }
                    else { r.From = r.To + Vector3.up * 40f; r.Dir = Vector3.down; }
                    float ground = new Vector2(r.To.x - r.From.x, r.To.z - r.From.z).magnitude;
                    r.Apex = Mathf.Max(MinApex, ground * ApexShare);
                    r.Flying = true; r.Trail = r.From;
                    if (fx) Launch(r, now);
                }
                float k = (clock - r.LaunchAt) / Mathf.Max(0.5f, r.LandAt - r.LaunchAt);
                if (k >= 1f)
                {
                    // it is down: the sim's burst, drawn by CombatFx, is this frame's; the earth it throws up is ours
                    if (fx)
                    {
                        books.Add(FlipbookFx.Book.Column, r.To, 3.4f, 1.4f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, alpha: 0.9f);
                        books.Add(FlipbookFx.Book.Wings, r.To, 5.5f, 0.9f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.5f, alpha: 0.7f);
                        books.Add(FlipbookFx.Book.Smoke, r.To + Vector3.up * 1.5f, 3.6f, 4f, velocity: Vector3.up * 0.9f, grow: 1.3f, alpha: 0.55f);
                    }
                    CameraShake.Add(r.To, 2f);
                    rockets.RemoveAt(i); continue;
                }
                // on its arc: a straight line to the landing point, lifted by a parabola that peaks halfway
                Vector3 at = Arc(r, k), ahead = Arc(r, Mathf.Min(1f, k + 0.01f)) - at;
                Vector3 vel = ahead.sqrMagnitude > 1e-6f ? ahead.normalized : r.Dir;
                if (rocketMesh != null && rocketMat != null)
                    Queue(rocketMesh, rocketMat, Matrix4x4.TRS(at - vel * RocketLength, Quaternion.LookRotation(vel), Vector3.one), 0f, Vector4.zero, new Vector4(1f, 1f, 1f, 0f));
                if (fx)
                {
                    Vector3 tail = at - vel * RocketLength;
                    float roll = cam != null ? FlipbookFx.ScreenRoll(cam, -vel) : 0f;
                    books.Add(FlipbookFx.Book.Muzzle, tail - vel * 0.6f, 1.1f, 0.05f, roll: roll, glow: SceneMood.Night ? 3f : 1.8f);   // the motor
                    // the trail: a puff every TrailStep metres of the way it came, however far that was this frame
                    Vector3 run = tail - r.Trail; float len = run.magnitude;
                    int puffs = Mathf.Min(40, Mathf.FloorToInt(len / TrailStep));
                    for (int p = 1; p <= puffs; p++)
                        books.Add(FlipbookFx.Book.Smoke, r.Trail + run * (p * TrailStep / len), 0.9f, 3.2f, (p & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: Vector3.up * 0.25f, grow: 1.9f, alpha: 0.42f);
                    if (puffs > 0) r.Trail += run * (puffs * TrailStep / len);
                }
                rockets[i] = r;
            }
        }

        static Vector3 Arc(in Rocket r, float k) => Vector3.Lerp(r.From, r.To, k) + Vector3.up * (4f * r.Apex * k * (1f - k));

        /// <summary>A rocket leaves its tube: a flash at the mouth; the rack's first throws the back-blast.</summary>
        void Launch(in Rocket r, float now)
        {
            books.Add(FlipbookFx.Book.Flash, r.From + r.Dir * 0.3f, 2.2f, 0.1f, roll: Random.value * 6.28f, glow: SceneMood.Night ? 3.5f : 2f);
            books.Add(FlipbookFx.Book.Smoke, r.From - r.Dir * 1.2f, 1.8f, 2.5f, velocity: -r.Dir * 4f + Vector3.up * 0.5f, grow: 1.6f, alpha: 0.6f);
            if (!r.First) return;
            CameraShake.Add(r.From, 3f);
            SceneHooks.Flash?.Invoke(r.From, new Color(1f, 0.7f, 0.4f), 30f, 12f, 0.25f);
            for (int k = 0; k < 7; k++)
            {
                Vector3 back = -r.Dir * (2f + k * 0.9f) + new Vector3(Random.Range(-1.5f, 1.5f), Random.Range(-0.5f, 1f), Random.Range(-1.5f, 1.5f));
                books.Add(FlipbookFx.Book.Smoke, r.From + back, 3.5f + k * 0.4f, 5f + k * 0.3f, (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: back.normalized * 1.5f + Vector3.up * 0.6f, grow: 1.4f, alpha: 0.6f, delay: k * 0.06f);
            }
            float g = Ground(r.From.x, r.From.z);
            books.Add(FlipbookFx.Book.Wings, new Vector3(r.From.x, g, r.From.z) - new Vector3(r.Dir.x, 0f, r.Dir.z) * 3f, 7f, 1.2f,
                FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.5f, alpha: 0.6f);
        }
    }
}
