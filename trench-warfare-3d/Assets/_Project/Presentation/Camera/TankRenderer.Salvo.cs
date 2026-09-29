// Phase: C4 (2026-09-28) — the Salvo's rockets, drawn where and when the sim flies them. TankGunnerySystem fires a rack
// (TankSpec.Rockets) as one RocketFired event per rocket on the tick the rack fires: where it comes down, the ticks until
// it leaves its tube and the ticks it flies. Its burst is queued for that landing tick, and CombatFx draws the burst from
// the Explosion event as it draws any shell. This file draws the rest, timed on the sim's own clock (World.Tick + Alpha)
// so each rocket reaches the ground on the frame its burst is shown, whatever the frame rate or the time scale:
//   the launch: a flash at its own tube's mouth (Socket_Tube## from Tools/mechsplit.py), and for the rack's first rocket
//     the back-blast: a cloud of smoke round the rack and dust thrown up off the ground under it;
//   the flight: a rocket body (a small mesh along its velocity, in the machine's own material), its motor's flame along
//     the flight, leaving along its tube (a cubic path whose first control point lies along the tube) and its smoke trail
//     as a ribbon of its own: points laid every RibbonStep metres, drawn as one camera-facing strip that widens and fades
//     over RibbonLife, all the rack's ribbons in one mesh. Critic round 4: the trail had been flipbook puffs every 0.7 m,
//     ~600 cards a rocket against FlipbookFx's shared 1,536, so one rack evicted every other smoke on the field;
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
            public Vector3 From, To, Dir;          // From and Dir are taken at launch
            public float LaunchAt, LandAt;         // on the sim clock (ticks): when it leaves the tube, when it bursts
            public float Apex;
            public bool Flying;
            public Ribbon Trail;                   // its smoke, which outlives it
        }

        /// <summary>A rocket's smoke: points along its way and the time each was laid (Time.time).</summary>
        sealed class Ribbon
        {
            public readonly List<Vector3> P = new List<Vector3>(64);
            public readonly List<float> T = new List<float>(64);
            public bool Live;                      // its rocket is still flying
        }

        readonly List<Ribbon> ribbons = new List<Ribbon>(32), ribbonPool = new List<Ribbon>(32);
        Mesh ribbonMesh; Material ribbonMat;
        readonly List<Vector3> rV = new List<Vector3>(2048); readonly List<Color> rC = new List<Color>(2048); readonly List<int> rI = new List<int>(6144);
        const float RibbonStep = 2f, RibbonLife = 4f;               // metres between points; seconds a point lasts
        // round 6: at 1.2 m growing 3 m, alpha 0.7 and a flat pale grey, sixteen overlapping ribbons were a searchlight bar at
        // night (RGB ~170 on ~31 ground) and a light shaft by day. Now thinner, fainter, broken up along the length and soft
        // across it, and at night the night smoke's own tint (FlipbookFx.NightTint), as the burst smoke is drawn
        const float RibbonWidth = 0.9f, RibbonGrow = 1f;            // metres across when laid; added over its life
        const float RibbonAlpha = 0.3f;
        const float RibbonBreak = 0.55f;    // how much of the alpha each point may lose to the break-up along the length
        const float RibbonNightValue = 0.16f;   // the night tint's value: the burst smoke's darkness
        static readonly Color RibbonDay = new Color(0.40f, 0.39f, 0.37f);   // darker than snow: pale grey vanished on the Winter field

        readonly List<Rocket> rockets = new List<Rocket>(32);
        const float RocketLength = 1.5f, RocketRadius = 0.16f;
        const float LaunchKick = 0.25f;     // what each rocket leaving adds to the rack's Recoil (0..1), which decays fast
        const float ApexShare = 0.22f;      // an arc's height as a share of the ground it covers (at least MinApex)
        const float MinApex = 8f;
        const float TubeLead = 0.35f;       // the path's first control point: along the tube, this share of the ground
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
                    // it leaves its own tube, from where the rack points now. A machine gone before this rocket's turn fires
                    // nothing more: the sim dropped the rocket (TankGunnerySystem, round 4), so the picture drops it too
                    if (!views.TryGetValue(r.Slot, out var v) || v.Gen != r.Gen || v.Dead || v.Model.GunPart[0] < 0) { rockets.RemoveAt(i); continue; }
                    r.From = TubeMouth(v, r.Tube, out r.Dir);
                    v.Recoil[0] = Mathf.Min(1f, v.Recoil[0] + LaunchKick);   // each rocket its own kick
                    RocketRock(v);                                          // and a sway of the hull (tank.shotRock)
                    float ground = new Vector2(r.To.x - r.From.x, r.To.z - r.From.z).magnitude;
                    r.Apex = Mathf.Max(MinApex, ground * ApexShare);
                    r.Flying = true;
                    r.Trail = ribbonPool.Count > 0 ? ribbonPool[ribbonPool.Count - 1] : new Ribbon();
                    if (ribbonPool.Count > 0) ribbonPool.RemoveAt(ribbonPool.Count - 1);
                    r.Trail.P.Clear(); r.Trail.T.Clear(); r.Trail.Live = true;
                    r.Trail.P.Add(r.From); r.Trail.T.Add(now);
                    ribbons.Add(r.Trail);
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
                    if (r.Trail != null) { r.Trail.P.Add(r.To); r.Trail.T.Add(now); r.Trail.Live = false; }
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
                }
                // the trail: a point every RibbonStep metres of the way it came (one ribbon mesh draws them all)
                if (r.Trail != null)
                {
                    Vector3 tailAt = at - vel * RocketLength;
                    if ((tailAt - r.Trail.P[r.Trail.P.Count - 1]).sqrMagnitude >= RibbonStep * RibbonStep) { r.Trail.P.Add(tailAt); r.Trail.T.Add(now); }
                }
                rockets[i] = r;
            }
        }

        /// <summary>A cubic path: out of the tube along it (the first control point lies along the tube), over, and down
        /// onto the landing point (the second lies above it).</summary>
        static Vector3 Arc(in Rocket r, float k)
        {
            float ground = new Vector2(r.To.x - r.From.x, r.To.z - r.From.z).magnitude;
            Vector3 c1 = r.From + r.Dir * (ground * TubeLead), c2 = r.To + Vector3.up * (r.Apex * 1.3f);
            float u = 1f - k;
            return u * u * u * r.From + 3f * u * u * k * c1 + 3f * u * k * k * c2 + k * k * k * r.To;
        }

        /// <summary>Every rack's smoke, in one mesh: each ribbon a camera-facing strip through its points, widening and
        /// fading as they age, dropped point by point as they pass RibbonLife.</summary>
        void RibbonsFrame(float now)
        {
            if (ribbons.Count == 0) return;
            if (ribbonMesh == null)
            {
                ribbonMesh = new Mesh { name = "Salvo trails", hideFlags = HideFlags.HideAndDontSave };
                ribbonMesh.MarkDynamic();
                // kept in the build by Resources/ShaderKeep/KeepParticlesUnlit.mat (ShaderInclusionTests)
                var shader = Shader.Find("Universal Render Pipeline/Particles/Unlit") ?? Shader.Find("Universal Render Pipeline/Unlit");
                ribbonMat = new Material(shader) { hideFlags = HideFlags.HideAndDontSave, name = "Salvo trails" };
                ribbonMat.SetFloat("_Surface", 1f); ribbonMat.SetFloat("_Blend", 0f); ribbonMat.SetFloat("_ZWrite", 0f);
                ribbonMat.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
                ribbonMat.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
                ribbonMat.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
                ribbonMat.SetOverrideTag("RenderType", "Transparent");
                ribbonMat.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
                if (ribbonMat.HasProperty("_BaseColor")) ribbonMat.SetColor("_BaseColor", Color.white);
                ribbonMat.SetFloat("_Cull", 0f);   // both faces: the strip's winding follows the view
            }
            var cam = Camera.main;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.up * 100f;
            Color tone = SceneMood.Night ? FlipbookFx.NightTint(RibbonNightValue) : RibbonDay;
            rV.Clear(); rC.Clear(); rI.Clear();
            for (int n = ribbons.Count - 1; n >= 0; n--)
            {
                var rb = ribbons[n];
                int old = 0;
                while (old < rb.T.Count && now - rb.T[old] > RibbonLife) old++;
                if (old > 0) { rb.P.RemoveRange(0, old); rb.T.RemoveRange(0, old); }
                if (rb.P.Count == 0 && !rb.Live) { ribbons.RemoveAt(n); ribbonPool.Add(rb); continue; }
                if (rb.P.Count < 2) continue;
                int first = rV.Count, seed = rb.GetHashCode();
                for (int k = 0; k < rb.P.Count; k++)
                {
                    Vector3 p = rb.P[k];
                    Vector3 along = k + 1 < rb.P.Count ? rb.P[k + 1] - p : p - rb.P[k - 1];
                    Vector3 side = Vector3.Cross(along, eye - p).normalized;
                    float age = Mathf.Clamp01((now - rb.T[k]) / RibbonLife);
                    float half = 0.5f * (RibbonWidth + RibbonGrow * age);
                    // puffy: each point's own share of the alpha (fixed per point, so it does not flicker), soft at the edges
                    float lump = 1f - RibbonBreak * Hash01(seed + k * 7919);
                    var c = tone; c.a = RibbonAlpha * lump * (1f - age) * (k == rb.P.Count - 1 && rb.Live ? 0f : 1f);
                    var edge = c; edge.a = 0f;
                    rV.Add(p - side * half); rV.Add(p); rV.Add(p + side * half); rC.Add(edge); rC.Add(c); rC.Add(edge);
                    if (k > 0)
                    {
                        int a = first + 3 * (k - 1), b = a + 3;
                        rI.Add(a); rI.Add(b); rI.Add(a + 1); rI.Add(a + 1); rI.Add(b); rI.Add(b + 1);
                        rI.Add(a + 1); rI.Add(b + 1); rI.Add(a + 2); rI.Add(a + 2); rI.Add(b + 1); rI.Add(b + 2);
                    }
                }
            }
            ribbonMesh.Clear();
            if (rI.Count == 0) return;
            ribbonMesh.SetVertices(rV); ribbonMesh.SetColors(rC); ribbonMesh.SetTriangles(rI, 0); ribbonMesh.RecalculateBounds();
            // through FrameBudget, as every draw is (FrameBudgetCoverageTests)
            var rp = new RenderParams(ribbonMat) { worldBounds = ribbonMesh.bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false };
            FrameBudget.Draw(rp, ribbonMesh, 0, Matrix4x4.identity);
        }

        static float Hash01(int n) { unchecked { uint h = (uint)n * 2654435761u; h ^= h >> 15; h *= 2246822519u; h ^= h >> 13; return (h & 0xFFFF) / 65535f; } }

        /// <summary>For the capture tools: how many trail ribbons and trail points are being drawn now.</summary>
        public (int ribbons, int points) TrailCount()
        {
            int pts = 0; foreach (var rb in ribbons) pts += rb.P.Count;
            return (ribbons.Count, pts);
        }

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
