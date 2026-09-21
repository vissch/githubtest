// Phase: B1 (implemented; greybox stand-in for C4 VFX and B5 ragdolls)
// Makes the fight readable with capsules only: every Shot event becomes a short-lived tracer, every Death leaves a
// flattened body in the team colour, and every trench or objective capture raises a banner. Instanced draws, no
// GameObjects per effect. Listens to SimHost.Events, so it sees exactly what the local sim produced.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class CombatFx : MonoBehaviour
    {
        public SimHost Host;
        public float TracerSeconds = 0.12f;
        public int MaxBodies = 600;

        struct Tracer { public Vector3 From, To; public float Born; public bool Hit; }
        struct Body { public Vector3 Pos; public float Yaw; public byte Team; }
        struct Burst { public Vector3 Pos; public float Radius, Born; }
        struct Marker { public Vector3 Pos; public float Radius, Until; public bool Mine; }
        struct Chunk { public Vector3 Pos, Vel; public float Born, Life, Size; public byte Kind; }   // 0 dirt, 1 splinter, 2 smoke

        readonly List<Tracer> tracers = new List<Tracer>(512);
        readonly List<Body> bodies = new List<Body>(600);
        readonly List<Burst> bursts = new List<Burst>(64);
        readonly List<Marker> markers = new List<Marker>(8);
        readonly List<Chunk> chunks = new List<Chunk>(768);
        Material dirtMat, woodMat, smokeMat;
        const int MaxChunks = 768;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        readonly Matrix4x4[] batchArray = new Matrix4x4[1023];
        Mesh cube, capsule, sphere;
        Material tracerMat, bodyMatA, bodyMatB, burstMat, markMine, markTheirs, aimMat;
        readonly Material[] gasMats = new Material[3];
        TestPanel panel;
        string banner; float bannerUntil;
        bool subscribed;

        void Start()
        {
            cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            capsule = Resources.GetBuiltinResource<Mesh>("Capsule.fbx");
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            var lit = Shader.Find("Universal Render Pipeline/Lit");
            if (unlit == null) unlit = Shader.Find("Unlit/Color");
            if (lit == null) lit = Shader.Find("Standard");
            tracerMat = new Material(unlit) { enableInstancing = true, color = new Color(1f, 0.9f, 0.45f) };
            bodyMatA = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.25f, 0.14f) };
            bodyMatB = new Material(lit) { enableInstancing = true, color = new Color(0.19f, 0.22f, 0.28f) };
            sphere = Resources.GetBuiltinResource<Mesh>("Sphere.fbx");
            dirtMat = new Material(lit) { enableInstancing = true, color = new Color(0.20f, 0.16f, 0.11f) };
            woodMat = new Material(lit) { enableInstancing = true, color = new Color(0.36f, 0.27f, 0.17f) };
            burstMat = Transparent(unlit, new Color(1f, 0.62f, 0.2f, 0.55f));
            markMine = Transparent(unlit, new Color(1f, 0.85f, 0.3f, 0.35f));
            markTheirs = Transparent(unlit, new Color(1f, 0.2f, 0.15f, 0.35f));
            aimMat = Transparent(unlit, new Color(1f, 1f, 1f, 0.22f));
            gasMats[0] = Transparent(unlit, new Color(0.78f, 0.85f, 0.25f, 0.18f));
            gasMats[1] = Transparent(unlit, new Color(0.78f, 0.85f, 0.25f, 0.34f));
            gasMats[2] = Transparent(unlit, new Color(0.80f, 0.86f, 0.22f, 0.52f));
            panel = GetComponent<TestPanel>();
        }

        /// <summary>URP Unlit set up for alpha blending from code (the shader GUI normally does this).</summary>
        static Material Transparent(Shader shader, Color color)
        {
            var m = new Material(shader) { enableInstancing = true, color = color };
            m.SetFloat("_Surface", 1f);
            m.SetFloat("_Blend", 0f);
            m.SetFloat("_ZWrite", 0f);
            m.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
            m.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
            m.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
            m.SetOverrideTag("RenderType", "Transparent");
            m.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
            if (m.HasProperty("_BaseColor")) m.SetColor("_BaseColor", color);
            return m;
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
        }

        void OnSimEvent(SimEvent e)
        {
            var w = Host.Local.World;
            var hf = Host.Local.Map.Height;
            switch (e.Type)
            {
                case SimEventType.Shot:
                {
                    if (tracers.Count >= 1500 || e.B < 0 || e.B >= w.Position.Length) break;
                    Vector3 from = (Vector3)e.Pos, to = (Vector3)w.Position[e.B];
                    from.y = hf.Sample(from.x, from.z) + 1.1f;
                    to.y = hf.Sample(to.x, to.z) + 0.9f;
                    tracers.Add(new Tracer { From = from, To = to, Born = Time.time });
                    break;
                }
                case SimEventType.Death:
                {
                    if (bodies.Count >= MaxBodies) bodies.RemoveAt(0);
                    Vector3 p = (Vector3)e.Pos;
                    p.y = hf.Sample(p.x, p.z) + 0.25f;
                    byte team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0;
                    bodies.Add(new Body { Pos = p, Yaw = Mathf.Atan2(e.Dir.x, e.Dir.z) * Mathf.Rad2Deg, Team = team });
                    break;
                }
                case SimEventType.Explosion:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = hf.Sample(p.x, p.z);
                    if (bursts.Count < 64) bursts.Add(new Burst { Pos = p, Radius = e.Scalar, Born = Time.time });
                    Throw(p, 14, 0, 9f, 0.22f); Throw(p + Vector3.up * 0.5f, 4, 2, 1.6f, 1.6f);
                    break;
                }
                case SimEventType.AbilityFired:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = hf.Sample(p.x, p.z) + 0.15f;
                    float radius = e.Scalar > 0f ? e.Scalar : 8f;
                    markers.Add(new Marker { Pos = p, Radius = radius, Until = Time.time + 10f, Mine = e.B == 0 });
                    string what = e.A == (int)OffMapAbilityId.ChlorineGas ? "gas" : "barrage";
                    Banner(e.B == 0 ? $"Your {what} is on its way" : $"INCOMING {what.ToUpper()}: fall back or keep below the rim", 3f);
                    break;
                }
                case SimEventType.PropChanged:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = hf.Sample(p.x, p.z) + 1.5f;
                    Throw(p, 18, 1, 7f, 0.16f);   // splinters where a tree broke, scrap where a wreck settled
                    break;
                }
                case SimEventType.TrenchCaptured:
                    Banner(e.B == 0 ? $"Trench {e.A} captured!" : $"Trench {e.A} lost!");
                    break;
                case SimEventType.MatchEnded:
                    Banner(e.A == 0 ? "VICTORY: enemy HQ taken" : "DEFEAT: your HQ has fallen", 3600f);
                    break;
            }
        }

        void Banner(string text, float seconds = 4f) { banner = text; bannerUntil = Time.time + seconds; }

        void Update()
        {
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));

            // tracers
            float now = Time.time;
            tracers.RemoveAll(t => now - t.Born > TracerSeconds);
            var rpT = new RenderParams(tracerMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            batch.Clear();
            for (int i = 0; i < tracers.Count; i++)
            {
                var t = tracers[i];
                Vector3 d = t.To - t.From;
                float len = d.magnitude;
                if (len < 0.1f) continue;
                // a streak that travels from muzzle to target over the tracer's life
                float k = Mathf.Clamp01((now - t.Born) / TracerSeconds);
                float streak = Mathf.Min(len, 14f);
                Vector3 mid = t.From + d.normalized * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
                batch.Add(Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(0.07f, 0.07f, streak)));
                if (batch.Count == 1023) Flush(cube, rpT);
            }
            if (batch.Count > 0) Flush(cube, rpT);

            // explosions: a fireball that swells to the blast radius and fades
            bursts.RemoveAll(b => now - b.Born > 0.45f);
            batch.Clear();
            for (int i = 0; i < bursts.Count; i++)
            {
                float k = (now - bursts[i].Born) / 0.45f;
                float r = Mathf.Lerp(1.5f, bursts[i].Radius, Mathf.Sqrt(k));
                batch.Add(Matrix4x4.TRS(bursts[i].Pos, Quaternion.identity, new Vector3(r * 2f, r * (1.2f - k), r * 2f)));
            }
            if (batch.Count > 0) Flush(sphere, new RenderParams(burstMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });

            DrawChunks(now, bounds);

            // target markers (both sides see where support fire was called) and the aiming circle
            markers.RemoveAll(m => now > m.Until);
            for (int pass = 0; pass < 2; pass++)
            {
                batch.Clear();
                for (int i = 0; i < markers.Count; i++)
                    if (markers[i].Mine == (pass == 0)) batch.Add(Matrix4x4.TRS(new Vector3(markers[i].Pos.x, Host.Local.Map.Height.Sample(markers[i].Pos.x, markers[i].Pos.z) + 0.4f, markers[i].Pos.z), Quaternion.identity, new Vector3(markers[i].Radius * 2f, 0.05f, markers[i].Radius * 2f)));   // on the ground where it was called, not at sea level
                if (batch.Count > 0) Flush(sphere, new RenderParams(pass == 0 ? markMine : markTheirs) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
            }
            if (panel != null && panel.Armed != OffMapAbilityId.None && panel.TryGroundPoint(out var aim) && OffMapAbilitySystem.TryGetStats((int)panel.Armed, out var aimStats))
            {
                float r = aimStats.Radius > 0f ? aimStats.Radius : 8f;
                aim.y = Host.Local.Map.Height.Sample(aim.x, aim.z) + 0.2f;
                batch.Clear();
                batch.Add(Matrix4x4.TRS(aim, Quaternion.identity, new Vector3(r * 2f, 0.05f, r * 2f)));
                Flush(sphere, new RenderParams(aimMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
            }

            // gas: one translucent block per 4 m field cell, three density bands
            var gas = Host.Local.Gas;
            if (gas != null && gas.Active)
            {
                var hfg = Host.Local.Map.Height;
                float cs = TW.Sim.Terrain.MapData.FieldCellSize;
                for (int band = 0; band < 3; band++)
                {
                    float lo = band == 0 ? 1f : band == 1 ? 6f : 18f, hi = band == 0 ? 6f : band == 1 ? 18f : float.MaxValue;
                    var rpG = new RenderParams(gasMats[band]) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                    batch.Clear();
                    for (int z = 0; z < gas.Length; z++)
                    for (int x = 0; x < gas.Width; x++)
                    {
                        float c = gas.Gas[z * gas.Width + x];
                        if (c < lo || c >= hi) continue;
                        float wx = (x + 0.5f) * cs, wz = (z + 0.5f) * cs;
                        batch.Add(Matrix4x4.TRS(new Vector3(wx, hfg.Sample(wx, wz) + 1.1f, wz), Quaternion.identity, new Vector3(cs, 2.4f, cs)));
                        if (batch.Count == 1023) Flush(cube, rpG);
                    }
                    if (batch.Count > 0) Flush(cube, rpG);
                }
            }

            // bodies
            for (int team = 0; team < 2; team++)
            {
                var rp = new RenderParams(team == 0 ? bodyMatA : bodyMatB) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                batch.Clear();
                for (int i = 0; i < bodies.Count; i++)
                {
                    var b = bodies[i];
                    if (b.Team != team) continue;
                    batch.Add(Matrix4x4.TRS(b.Pos, Quaternion.Euler(90f, b.Yaw, 0f), new Vector3(0.55f, 0.85f, 0.45f)));
                    if (batch.Count == 1023) Flush(capsule, rp);
                }
                if (batch.Count > 0) Flush(capsule, rp);
            }
        }

        /// <summary>Throw debris: dirt and splinters fly and fall, smoke rises, swells and thins.</summary>
        void Throw(Vector3 at, int count, byte kind, float speed, float size)
        {
            for (int k = 0; k < count && chunks.Count < MaxChunks; k++)
            {
                Vector3 dir = UnityEngine.Random.onUnitSphere; dir.y = Mathf.Abs(dir.y) * (kind == 2 ? 0.4f : 1.4f) + 0.2f;
                chunks.Add(new Chunk { Pos = at, Vel = dir.normalized * speed * UnityEngine.Random.Range(0.5f, 1.2f), Born = Time.time, Life = kind == 2 ? UnityEngine.Random.Range(3.5f, 6f) : UnityEngine.Random.Range(0.9f, 1.7f),
                    Size = size * UnityEngine.Random.Range(0.6f, 1.5f), Kind = kind });
            }
        }

        void DrawChunks(float now, Bounds bounds)
        {
            if (smokeMat == null) smokeMat = Transparent(Shader.Find("Universal Render Pipeline/Unlit"), new Color(0.16f, 0.15f, 0.14f, 0.30f));
            chunks.RemoveAll(c => now - c.Born > c.Life);
            float dt = Time.deltaTime;
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind == 2) { c.Vel = Vector3.Lerp(c.Vel, new Vector3(0f, 1.2f, -0.8f), dt * 1.5f); }   // drifts up and down wind
                else c.Vel += Vector3.down * 9.8f * dt;
                c.Pos += c.Vel * dt;
                chunks[i] = c;
            }
            for (byte kind = 0; kind < 3; kind++)
            {
                batch.Clear();
                var rp = new RenderParams(kind == 0 ? dirtMat : kind == 1 ? woodMat : smokeMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < chunks.Count; i++)
                {
                    var c = chunks[i];
                    if (c.Kind != kind) continue;
                    float k = (now - c.Born) / c.Life;
                    float s = kind == 2 ? c.Size * (1f + 2.5f * k) * (1f - k * k * 0.6f) : c.Size;
                    var rot = kind == 2 ? Quaternion.identity : Quaternion.Euler(c.Born * 997f + now * 300f, c.Born * 613f, now * 200f);
                    batch.Add(Matrix4x4.TRS(c.Pos, rot, kind == 1 ? new Vector3(s * 0.4f, s * 0.4f, s * 3f) : new Vector3(s, s, s)));
                    if (batch.Count == 1023) Flush(kind == 2 ? sphere : cube, rp);
                }
                if (batch.Count > 0) Flush(kind == 2 ? sphere : cube, rp);
            }
        }

        void Flush(Mesh mesh, RenderParams rp)
        {
            batch.CopyTo(batchArray);
            Graphics.RenderMeshInstanced(rp, mesh, 0, batchArray, batch.Count);
            batch.Clear();
        }

        void OnGUI()
        {
            if (banner == null || Time.time > bannerUntil) return;
            var style = new GUIStyle(GUI.skin.label) { fontSize = 30, fontStyle = FontStyle.Bold, alignment = TextAnchor.MiddleCenter };
            var rect = new Rect(0, Screen.height * 0.12f, Screen.width, 50);
            style.normal.textColor = Color.black; GUI.Label(new Rect(rect.x + 2, rect.y + 2, rect.width, rect.height), banner, style);
            style.normal.textColor = new Color(1f, 0.92f, 0.6f); GUI.Label(rect, banner, style);
        }
    }
}
