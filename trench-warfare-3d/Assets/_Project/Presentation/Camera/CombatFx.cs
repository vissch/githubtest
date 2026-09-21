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

        struct Tracer { public Vector3 From, To; public float Born; public bool Hit; public byte Team; }
        struct Body { public Vector3 Pos; public float Yaw; public byte Team; }
        struct Burst { public Vector3 Pos; public float Radius, Born; public int Variant; }
        struct Flash { public Vector3 Pos, Direction; public float Born; }
        struct Marker { public Vector3 Pos; public float Radius, Until; public bool Mine; }
        struct Chunk { public Vector3 Pos, Vel; public float Born, Life, Size; public byte Kind; }   // 0 dirt, 1 splinter, 2 smoke

        readonly List<Tracer> tracers = new List<Tracer>(512);
        readonly List<Body> bodies = new List<Body>(600);
        readonly List<Burst> bursts = new List<Burst>(64);
        readonly List<Flash> flashes = new List<Flash>(256);
        readonly List<Marker> markers = new List<Marker>(8);
        readonly List<Chunk> chunks = new List<Chunk>(768);
        Material dirtMat, woodMat, smokeMat;
        Material smokeThin, smokeFaint;
        const int MaxChunks = 768;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        readonly Matrix4x4[] batchArray = new Matrix4x4[1023];
        Mesh cube, capsule, sphere, plume, puff, flashMesh;
        Material flashMat;
        Material tracerNightA, tracerNightB, tracerCore;
        bool nightTinted;

        /// <summary>An unlit material that adds its colour to what is behind it (glow halos).</summary>
        static Material Additive(Shader unlit, Color color)
        {
            var m = new Material(unlit) { enableInstancing = true, color = color };
            m.SetFloat("_Surface", 1f); m.SetFloat("_Blend", 2f);
            m.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One); m.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.One);
            m.SetInt("_ZWrite", 0); m.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
            m.SetOverrideTag("RenderType", "Transparent"); m.renderQueue = 3100;
            return m;
        }
        Material tracerMat, bodyMatA, bodyMatB, burstMat, markMine, markTheirs, aimMat;
        readonly Material[] gasMats = new Material[3];
        TestPanel panel;
        TW.Presentation.Units.VATRenderer units;
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
            tracerMat = new Material(unlit) { enableInstancing = true, color = new Color(0.95f, 0.84f, 0.57f) };
            // night (SceneMood): each side's fire is its own colour, over-bright so the bloom takes it
            tracerNightA = Additive(unlit, new Color(0.06f, 0.36f, 0.12f));   // the halo round the streak: its side's colour
            tracerNightB = Additive(unlit, new Color(0.50f, 0.07f, 0.05f));
            tracerCore = new Material(unlit) { enableInstancing = true, color = new Color(3.0f, 2.7f, 2.3f) };   // the streak itself: white-hot
            bodyMatA = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.25f, 0.14f) };
            bodyMatB = new Material(lit) { enableInstancing = true, color = new Color(0.19f, 0.22f, 0.28f) };
            sphere = Resources.GetBuiltinResource<Mesh>("Sphere.fbx");
            dirtMat = Painted(new Color(0.32f, 0.285f, 0.24f), 0.7f);
            woodMat = Painted(new Color(0.43f, 0.35f, 0.25f), 0.6f);
            burstMat = Painted(new Color(0.53f, 0.46f, 0.35f), 1.1f);
            smokeMat = Transparent(unlit, new Color(0.34f, 0.32f, 0.29f, 0.36f));
            smokeThin = Transparent(unlit, new Color(0.34f, 0.32f, 0.29f, 0.20f));
            smokeFaint = Transparent(unlit, new Color(0.34f, 0.32f, 0.29f, 0.07f));
            flashMat = new Material(unlit) { enableInstancing = true, color = new Color(1f, 0.91f, 0.65f) };
            plume = BuildPlume(); puff = BuildPuff(); flashMesh = BuildFlash();
            markMine = Transparent(unlit, new Color(1f, 0.85f, 0.3f, 0.35f));
            markTheirs = Transparent(unlit, new Color(1f, 0.2f, 0.15f, 0.35f));
            aimMat = Transparent(unlit, new Color(1f, 1f, 1f, 0.22f));
            gasMats[0] = Transparent(unlit, new Color(0.78f, 0.85f, 0.25f, 0.18f));
            gasMats[1] = Transparent(unlit, new Color(0.78f, 0.85f, 0.25f, 0.34f));
            gasMats[2] = Transparent(unlit, new Color(0.80f, 0.86f, 0.22f, 0.52f));
            panel = GetComponent<TestPanel>();
            units = FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
        }

        static Material Painted(Color color, float outline)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color); mat.SetFloat("_OutlineWidth", outline);
            return mat;
        }

        static Mesh BuildPlume()
        {
            // A shaped, double-sided ink silhouette: more like a painted splash than a cluster of solid cones.
            // Two rings preserve a dark connected root and pale irregular tips. All radii are asset data.
            float[] radii = { .72f, .62f, .65f, 1.08f, .49f, 1.32f, .83f, 1.49f, .48f, 1.13f, .62f, .67f,
                .72f, .72f, .72f, .72f, .72f, .72f, .72f, .72f, .72f, .72f, .72f, .72f };
            var vertices = new List<Vector3> { new Vector3(0f, 0.55f, 0f) };
            var colors = new List<Color> { new Color(.43f, .43f, .43f, 1f) };
            var normals = new List<Vector3> { Vector3.forward };
            var outline = new List<Vector3> { Vector3.up };
            var triangles = new List<int>();
            for (int ring = 0; ring < 2; ring++)
            for (int i = 0; i < radii.Length; i++)
            {
                float a = i * Mathf.PI * 2f / radii.Length;
                float y = Mathf.Sin(a), r = radii[i];
                var edge = new Vector3(Mathf.Cos(a) * r, Mathf.Max(0f, .55f + y * (y > 0f ? r * 1.7f : .58f)), 0f);
                var v = Vector3.Lerp(new Vector3(0f, .55f, 0f), edge, ring == 0 ? .52f : 1f);
                vertices.Add(v); normals.Add(Vector3.forward);
                outline.Add((edge - new Vector3(0f, .55f, 0f)).normalized);
                float pigment = ring == 0 ? .62f : Mathf.Lerp(.57f, 1.04f, Mathf.Clamp01(v.y / 2.5f));
                colors.Add(new Color(pigment, pigment, pigment, 1f));
            }
            for (int i = 0; i < 24; i++)
            {
                int j = (i + 1) % 24;
                triangles.Add(0); triangles.Add(i + 1); triangles.Add(j + 1);
                triangles.Add(i + 1); triangles.Add(i + 25); triangles.Add(j + 1);
                triangles.Add(j + 1); triangles.Add(i + 25); triangles.Add(j + 25);
            }
            int frontCount = triangles.Count;
            for (int i = 0; i < frontCount; i += 3) { triangles.Add(triangles[i]); triangles.Add(triangles[i + 2]); triangles.Add(triangles[i + 1]); }
            var mesh = new Mesh { name = "Painted dirt splash", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetNormals(normals); mesh.SetColors(colors); mesh.SetUVs(3, outline);
            mesh.SetTriangles(triangles, 0); mesh.RecalculateBounds(); return mesh;
        }

        static Mesh BuildPuff()
        {
            var vertices = new List<Vector3>(); var triangles = new List<int>();
            const int sides = 8, rings = 5;
            for (int ring = 0; ring <= rings; ring++)
            for (int side = 0; side <= sides; side++)
            {
                float a = side * Mathf.PI * 2f / sides, b = ring * Mathf.PI / rings;
                vertices.Add(new Vector3(Mathf.Cos(a) * Mathf.Sin(b), Mathf.Cos(b), Mathf.Sin(a) * Mathf.Sin(b)) * 0.5f);
                if (ring == rings || side == sides) continue;
                int i = ring * (sides + 1) + side;
                triangles.Add(i); triangles.Add(i + 1); triangles.Add(i + sides + 1);
                triangles.Add(i + 1); triangles.Add(i + sides + 2); triangles.Add(i + sides + 1);
            }
            var mesh = new Mesh { name = "Low poly smoke puff", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetTriangles(triangles, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds(); return mesh;
        }

        static Mesh BuildFlash()
        {
            var vertices = new List<Vector3> { new Vector3(0f, 0f, 0.55f) }; var triangles = new List<int>();
            for (int i = 0; i < 8; i++)
            {
                float a = i * Mathf.PI * 0.25f, r = (i & 1) == 0 ? 0.5f : 0.17f;
                vertices.Add(new Vector3(Mathf.Cos(a) * r, Mathf.Sin(a) * r, 0f));
            }
            for (int i = 0; i < 8; i++)
            {
                int a = i + 1, b = (i + 1) % 8 + 1;
                triangles.Add(0); triangles.Add(a); triangles.Add(b);
                triangles.Add(0); triangles.Add(b); triangles.Add(a);
            }
            var mesh = new Mesh { name = "Muzzle ink star", vertices = vertices.ToArray(), triangles = triangles.ToArray(), hideFlags = HideFlags.HideAndDontSave };
            mesh.RecalculateNormals(); return mesh;
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
            foreach (var mat in new[] { tracerNightA, tracerNightB, tracerCore, tracerMat, bodyMatA, bodyMatB, burstMat, markMine, markTheirs, aimMat, dirtMat, woodMat, smokeMat, smokeThin, smokeFaint, flashMat }) if (mat != null) Destroy(mat);
            foreach (var mat in gasMats) if (mat != null) Destroy(mat);
            if (plume != null) Destroy(plume); if (puff != null) Destroy(puff); if (flashMesh != null) Destroy(flashMesh);
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
                    float scale = 1f;
                    var cam = Camera.main;
                    if (units != null)
                    {
                        float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
                        scale = units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
                    }
                    var stance = e.A >= 0 && e.A < w.HighWater ? (Stance)w.StanceOf[e.A] : Stance.Standing;
                    float shoulder = stance == Stance.Prone || stance == Stance.Pinned ? 0.35f : stance == Stance.Crouch ? 0.85f : 1.1f;
                    from.y = RenderGround.Sample(Host.Local.Map, from.x, from.z) + shoulder * scale;
                    to.y = RenderGround.Sample(Host.Local.Map, to.x, to.z) + 0.9f * scale;
                    tracers.Add(new Tracer { From = from, To = to, Born = Time.time, Team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0 });
                    Vector3 direction = (to - from).normalized;
                    if (flashes.Count < 256 && e.Scalar < 0.5f) flashes.Add(new Flash { Pos = from + direction * (0.65f * scale), Direction = direction, Born = Time.time });
                    // a rifle leaves a little smoke at the muzzle: one small puff that drifts forward and thins out. Capped well
                    // under the chunk budget so a big firefight never starves the shell bursts of theirs.
                    if (e.Scalar < 0.5f && chunks.Count < 420)
                        chunks.Add(new Chunk { Pos = from + direction * (0.9f * scale), Vel = direction * 1.4f + Vector3.up * 0.35f, Born = Time.time, Life = UnityEngine.Random.Range(1.1f, 1.9f), Size = 0.16f * scale * UnityEngine.Random.Range(0.8f, 1.3f), Kind = 2 });
                    break;
                }
                case SimEventType.Death:
                {
                    if (bodies.Count >= MaxBodies) bodies.RemoveAt(0);
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.25f;
                    byte team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0;
                    bodies.Add(new Body { Pos = p, Yaw = Mathf.Atan2(e.Dir.x, e.Dir.z) * Mathf.Rad2Deg, Team = team });
                    break;
                }
                case SimEventType.Explosion:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
                    if (bursts.Count < 64) bursts.Add(new Burst { Pos = p, Radius = e.Scalar, Born = Time.time, Variant = (Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 3 });
                    Throw(p, 14, 0, 9f, 0.22f); Throw(p + Vector3.up * 0.5f, 4, 2, 1.6f, 1.6f);
                    break;
                }
                case SimEventType.AbilityFired:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.15f;
                    float radius = e.Scalar > 0f ? e.Scalar : 8f;
                    markers.Add(new Marker { Pos = p, Radius = radius, Until = Time.time + 10f, Mine = e.B == 0 });
                    string what = e.A == (int)OffMapAbilityId.ChlorineGas ? "gas" : "barrage";
                    Banner(e.B == 0 ? $"Your {what} is on its way" : $"INCOMING {what.ToUpper()}: fall back or keep below the rim", 3f);
                    break;
                }
                case SimEventType.PropChanged:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 1.5f;
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
            bool night = SceneMood.Night;
            if (night && !nightTinted)
            {
                // under the moon smoke is a cold dark blue, and the flash is over-bright so the bloom spreads it
                nightTinted = true;
                smokeMat.color = new Color(0.17f, 0.20f, 0.27f, 0.36f); smokeThin.color = new Color(0.17f, 0.20f, 0.27f, 0.20f); smokeFaint.color = new Color(0.17f, 0.20f, 0.27f, 0.07f);
                flashMat.color = new Color(3.2f, 2.5f, 1.3f);
            }
            // night: three layers a tracer. side 0 / 1 = a wide additive halo in the side's colour, side 2 = the white-hot streak.
            for (int side = 0; side < (night ? 3 : 1); side++)
            {
            var rpT = new RenderParams(night ? (side == 0 ? tracerNightA : side == 1 ? tracerNightB : tracerCore) : tracerMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            batch.Clear();
            for (int i = 0; i < tracers.Count; i++)
            {
                var t = tracers[i];
                if (night && side < 2 && (t.Team & 1) != side) continue;
                Vector3 d = t.To - t.From;
                float len = d.magnitude;
                if (len < 0.1f) continue;
                // a streak that travels from muzzle to target over the tracer's life
                float k = Mathf.Clamp01((now - t.Born) / TracerSeconds);
                float streak = Mathf.Min(len, night ? 10f : 6f);
                Vector3 mid = t.From + d.normalized * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
                float thick = !night ? 0.045f : side == 2 ? 0.075f : 0.24f;
                batch.Add(Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(thick, thick, side == 2 ? streak * 0.8f : streak * 1.15f)));
                if (batch.Count == 1023) Flush(cube, rpT);
            }
            if (batch.Count > 0) Flush(cube, rpT);
            }

            flashes.RemoveAll(f => now - f.Born > 0.065f);
            batch.Clear();
            for (int i = 0; i < flashes.Count; i++)
            {
                var f = flashes[i]; float s = Mathf.Lerp(0.80f, 0.22f, (now - f.Born) / 0.065f);
                batch.Add(Matrix4x4.TRS(f.Pos, f.Direction.sqrMagnitude > 0.01f ? Quaternion.LookRotation(f.Direction) : Quaternion.identity, new Vector3(s, s, s * 1.6f)));
            }
            if (batch.Count > 0) Flush(flashMesh, new RenderParams(flashMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });

            // Earth rises sharply, then collapses. The silhouette faces the view but stays vertical and rooted.
            var view = Camera.main;
            Vector3 facing = view != null ? -view.transform.forward : Vector3.forward; facing.y = 0f;
            var splashRotation = facing.sqrMagnitude > 0.001f ? Quaternion.LookRotation(facing) : Quaternion.identity;
            bursts.RemoveAll(b => now - b.Born > 0.8f);
            batch.Clear();
            for (int i = 0; i < bursts.Count; i++)
            {
                float k = Mathf.Clamp01((now - bursts[i].Born) / 0.8f);
                float r = Mathf.Clamp(bursts[i].Radius * 0.42f, 0.8f, 2.6f);
                float rise = Mathf.Sin(Mathf.Pow(k, 0.55f) * Mathf.PI);
                int variant = bursts[i].Variant;
                float flip = (variant & 1) == 0 ? 1f : -1f;
                batch.Add(Matrix4x4.TRS(bursts[i].Pos + Vector3.down * k * 0.2f, splashRotation * Quaternion.Euler(0f, 0f, variant * 4f - 6f), new Vector3(flip * r * (0.7f + k * 0.4f), r * Mathf.Max(0.02f, rise) * (0.88f + variant * 0.08f), r)));
            }
            if (batch.Count > 0) Flush(plume, new RenderParams(burstMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
            batch.Clear();
            for (int i = 0; i < bursts.Count; i++)
            {
                float k = Mathf.Clamp01((now - bursts[i].Born) / 0.8f);
                float r = Mathf.Clamp(bursts[i].Radius * 0.42f, 0.8f, 2.6f);
                float h = Mathf.Sin(k * Mathf.PI) * r * 0.65f;
                batch.Add(Matrix4x4.TRS(bursts[i].Pos + Vector3.up * h * 0.30f, Quaternion.identity, new Vector3(r * (1.25f + k * 0.5f), Mathf.Max(0.01f, h), r * (1.25f + k * 0.5f))));
            }
            if (batch.Count > 0) Flush(puff, new RenderParams(smokeMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });

            DrawChunks(now, bounds);

            // target markers (both sides see where support fire was called) and the aiming circle
            markers.RemoveAll(m => now > m.Until);
            for (int pass = 0; pass < 2; pass++)
            {
                batch.Clear();
                for (int i = 0; i < markers.Count; i++)
                    if (markers[i].Mine == (pass == 0)) batch.Add(Matrix4x4.TRS(new Vector3(markers[i].Pos.x, RenderGround.Sample(Host.Local.Map, markers[i].Pos.x, markers[i].Pos.z) + 0.4f, markers[i].Pos.z), Quaternion.identity, new Vector3(markers[i].Radius * 2f, 0.05f, markers[i].Radius * 2f)));   // on the ground where it was called, not at sea level
                if (batch.Count > 0) Flush(sphere, new RenderParams(pass == 0 ? markMine : markTheirs) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
            }
            if (panel != null && panel.Armed != OffMapAbilityId.None && panel.TryGroundPoint(out var aim) && OffMapAbilitySystem.TryGetStats((int)panel.Armed, out var aimStats))
            {
                float r = aimStats.Radius > 0f ? aimStats.Radius : 8f;
                aim.y = RenderGround.Sample(Host.Local.Map, aim.x, aim.z) + 0.2f;
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
                        batch.Add(Matrix4x4.TRS(new Vector3(wx, RenderGround.Sample(Host.Local.Map, wx, wz) + 1.1f, wz), Quaternion.identity, new Vector3(cs, 2.4f, cs)));
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
            for (int pass = 0; pass < 5; pass++)
            {
                byte kind = (byte)Mathf.Min(pass, 2);
                batch.Clear();
                var rp = new RenderParams(pass == 0 ? dirtMat : pass == 1 ? woodMat : pass == 2 ? smokeMat : pass == 3 ? smokeThin : smokeFaint) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < chunks.Count; i++)
                {
                    var c = chunks[i];
                    if (c.Kind != kind) continue;
                    float k = (now - c.Born) / c.Life;
                    if (kind == 2 && Mathf.Min(2, Mathf.FloorToInt(k * 3f)) != pass - 2) continue;
                    float s = kind == 2 ? c.Size * (1f + 2f * k) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.55f, 1f, k))) : c.Size;
                    var rot = kind == 2 ? Quaternion.identity : Quaternion.Euler(c.Born * 997f + now * 300f, c.Born * 613f, now * 200f);
                    batch.Add(Matrix4x4.TRS(c.Pos, rot, kind == 1 ? new Vector3(s * 0.4f, s * 0.4f, s * 3f) : new Vector3(s, s, s)));
                    if (batch.Count == 1023) Flush(kind == 2 ? puff : cube, rp);
                }
                if (batch.Count > 0) Flush(kind == 2 ? puff : cube, rp);
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
