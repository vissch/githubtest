// Phase: B1 (implemented; C4 VFX: the drawn bursts, hits and flares live in FlipbookFx; B5 ragdolls still stand-ins)
// Makes the fight readable: every Shot event becomes a short-lived tracer with a muzzle flare and a spurt where it
// lands, every Hit a spike and a puff on the man, every Explosion a drawn burst with its column and wings, every Death
// leaves a body, and every trench or objective capture raises a banner. Instanced draws, no GameObjects per effect.
// Listens to SimHost.Events, so it sees exactly what the local sim produced.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    /// <summary>
    /// A shell landing near what you are looking at jolts the camera. Runs after TacticalCamera has placed the camera
    /// for the frame (which it does from scratch every frame, so the jolt never accumulates) and decays in half a second.
    /// </summary>
    [DefaultExecutionOrder(10000)]
    public sealed class CameraShake : MonoBehaviour
    {
        static float trauma;
        static Vector3 lookPoint;
        /// <summary>A burst of this radius at this place; how hard it shakes falls off with its distance from the view's centre.</summary>
        public static void Add(Vector3 at, float radius)
        {
            float d = Vector3.Distance(at, lookPoint);
            trauma = Mathf.Min(0.8f, trauma + Mathf.Clamp01(Mathf.Max(radius, 3f) / (d * 0.45f + 5f)) * 0.55f);
        }

        void LateUpdate()
        {
            var t = transform;
            lookPoint = t.position + t.forward * (t.position.y / Mathf.Max(0.15f, -t.forward.y));
            if (trauma <= 0f) return;
            float s = trauma * trauma, c = Time.time * 28f;
            t.position += (t.right * (Mathf.PerlinNoise(c, 1.3f) - 0.5f) + t.up * (Mathf.PerlinNoise(2.7f, c) - 0.5f)) * (0.9f * s);
            t.rotation *= Quaternion.Euler((Mathf.PerlinNoise(c, 5.1f) - 0.5f) * 1.2f * s, (Mathf.PerlinNoise(7.9f, c) - 0.5f) * 1.2f * s, (Mathf.PerlinNoise(c, 9.4f) - 0.5f) * 1.6f * s);
            trauma = Mathf.Max(0f, trauma - Time.deltaTime * 1.7f);
        }
    }

    public sealed class CombatFx : MonoBehaviour
    {
        public SimHost Host;
        public float TracerSeconds = 0.12f;
        public int MaxBodies = 600;

        struct Tracer { public Vector3 From, To; public float Born; public bool Hit; public byte Team; }
        struct Body { public Vector3 Pos; public Quaternion Rot; public byte Team, Variant; }
        struct Burst { public Vector3 Pos; public float Radius, Born; public int Variant; }
        struct Flash { public Vector3 Pos, Direction; public float Born; }
        struct Marker { public Vector3 Pos; public float Radius, Until; public bool Mine; }
        struct Chunk { public Vector3 Pos, Vel; public float Born, Life, Size; public byte Kind; }   // 0 dirt, 1 splinter, 2 smoke, 3 spark (night), 4 water, 5 brass, 6 helmet, 7 vapour
        // ---- what only a close camera sees (SceneHooks.CloseUp): nothing below is made or drawn at the standard view
        struct Rest { public Matrix4x4 At; public float Until; public byte Kind; }      // things come to rest: 0 brass, 1 helmet, 2 clod
        struct Mark { public Matrix4x4 At; public float Born, Life; public byte Kind; }  // pressed into the mud: 0 boot print, 1 track rut
        struct Trail { public Vector3 Last; public bool Left; public float Seen; }
        readonly List<Rest> rests = new List<Rest>(256);
        readonly List<Mark> marks = new List<Mark>(512);
        readonly Dictionary<int, Trail> trails = new Dictionary<int, Trail>(128);
        readonly List<Vector4> hotCraters = new List<Vector4>(16);   // xyz, w = cools at
        readonly List<int> trailSweep = new List<int>(64);
        const int MaxRests = 320, MaxMarks = 520;
        const float CloseReach = 42f;
        readonly Mesh[] fallen = new Mesh[8];
        readonly Material[] markMats = new Material[6];
        Material fallenMat, brassMat, helmetMat, vapourMat;
        Mesh markQuad;
        float nextPrint, nextBreath, nextExhaust; int breathCursor;
        static readonly int WetId = Shader.PropertyToID("_TWWet");
        static readonly int WindGlobalId = Shader.PropertyToID("_TWWind");
        struct Bird { public Vector3 Pos, Vel; public float Born, Phase; }
        readonly List<Bird> birds = new List<Bird>();
        const int MaxBirds = 80; const float BirdLife = 8f;
        float lastFlock = -10f, nextKick, nextSmoke; int impactsThisFrame, kickCursor;
        Material waterMat, birdMat;

        readonly List<Tracer> tracers = new List<Tracer>(512);
        readonly List<Body> bodies = new List<Body>(600);
        readonly List<Burst> bursts = new List<Burst>(64);
        readonly List<Flash> flashes = new List<Flash>(256);
        readonly List<Marker> markers = new List<Marker>(8);
        readonly List<Chunk> chunks = new List<Chunk>(768);
        readonly List<Matrix4x4> gasCards = new List<Matrix4x4>(2048);
        Material dirtMat, woodMat, smokeMat;
        Material smokeThin, smokeFaint;
        const int MaxChunks = 768;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        readonly Matrix4x4[] batchArray = new Matrix4x4[1023];
        Mesh cube, capsule, sphere, plume, puff, flashMesh;
        Material flashMat;
        Material tracerNightA, tracerNightB, tracerCore, sparkMat;
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
        FlipbookFx books;   // the drawn bursts, dust, hits and flares; without its textures the older painted meshes stand in
        int hitsThisFrame;
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
            sparkMat = Additive(unlit, new Color(3.4f, 1.7f, 0.5f));
            waterMat = new Material(unlit) { enableInstancing = true, color = new Color(0.62f, 0.70f, 0.82f) };
            birdMat = new Material(unlit) { enableInstancing = true, color = new Color(0.05f, 0.05f, 0.07f) };
            SceneHooks.Sparks = (at, count) => Throw(at, count, 3, 2.5f, 0.04f);
            var lens = Camera.main;
            if (lens != null && lens.GetComponent<CameraShake>() == null) lens.gameObject.AddComponent<CameraShake>();
            tracerCore = new Material(unlit) { enableInstancing = true, color = new Color(3.0f, 2.7f, 2.3f) };   // the streak itself: white-hot
            bodyMatA = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.25f, 0.14f) };
            bodyMatB = new Material(lit) { enableInstancing = true, color = new Color(0.19f, 0.22f, 0.28f) };
            sphere = Resources.GetBuiltinResource<Mesh>("Sphere.fbx");
            dirtMat = Painted(new Color(0.27f, 0.235f, 0.20f), 0.7f);
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
            books = new FlipbookFx();
            if (!books.Ready) Debug.LogWarning("CombatFx: the flipbook textures (Resources/VFX) or TW/Flipbook are missing; drawing the painted stand-ins.");
            // the fallen lie as they fell: four deaths a side, in their side's cloth
            for (int k = 0; k < 8; k++)
                fallen[k] = TW.Presentation.Units.ProceduralSoldier.BuildFallen(k & 3, k < 4 ? new Color(0.47f, 0.40f, 0.24f) : new Color(0.34f, 0.38f, 0.40f));
            fallenMat = Painted(Color.white, 1.2f);
            brassMat = Painted(new Color(0.80f, 0.60f, 0.24f), 0f); brassMat.SetColor("_Emission", new Color(0.20f, 0.14f, 0.04f));
            helmetMat = Painted(new Color(0.25f, 0.28f, 0.23f), 0.9f);
            vapourMat = Transparent(unlit, new Color(0.74f, 0.80f, 0.90f, 0.13f));
            var markShader = Shader.Find("TW/GroundMark (URP)");
            if (markShader != null)
                for (int k = 0; k < 6; k++)
                {
                    markMats[k] = new Material(markShader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
                    markMats[k].SetFloat("_Shape", k / 3); markMats[k].SetFloat("_Alpha", (k % 3) == 0 ? 0.88f : (k % 3) == 1 ? 0.58f : 0.26f);
                }
            markQuad = new Mesh { name = "Ground mark", hideFlags = HideFlags.HideAndDontSave };
            markQuad.SetVertices(new List<Vector3> { new Vector3(-.5f, 0f, -.5f), new Vector3(-.5f, 0f, .5f), new Vector3(.5f, 0f, .5f), new Vector3(.5f, 0f, -.5f) });
            markQuad.SetUVs(0, new List<Vector2> { new Vector2(0f, 0f), new Vector2(0f, 1f), new Vector2(1f, 1f), new Vector2(1f, 0f) });
            markQuad.SetNormals(new List<Vector3> { Vector3.up, Vector3.up, Vector3.up, Vector3.up });
            markQuad.SetTriangles(new[] { 0, 1, 2, 0, 2, 3 }, 0); markQuad.RecalculateBounds();
        }

        /// <summary>The ground point the view looks at, and whether a place is near enough to it for the small things.</summary>
        static Vector3 LookPoint(Camera cam) => cam.transform.position + cam.transform.forward * (cam.transform.position.y / Mathf.Max(0.15f, -cam.transform.forward.y));
        static bool Near(Vector3 p, float reach)
        {
            if (SceneHooks.CloseUp <= 0f) return false;
            var cam = Camera.main; if (cam == null) return false;
            Vector3 eye = cam.transform.position; float dx = p.x - eye.x, dz = p.z - eye.z;
            return dx * dx + dz * dz < reach * reach;
        }

        /// <summary>The ground's tilt at a point, so a print or a body lies on the slope and not in the air above it.</summary>
        Quaternion Lie(float x, float z, float yawDegrees, float span = 0.3f)
        {
            var map = Host.Local.Map;
            float sx = RenderGround.Sample(map, x + span, z) - RenderGround.Sample(map, x - span, z), sz = RenderGround.Sample(map, x, z + span) - RenderGround.Sample(map, x, z - span);
            return Quaternion.FromToRotation(Vector3.up, new Vector3(-sx, 2f * span, -sz).normalized) * Quaternion.Euler(0f, yawDegrees, 0f);
        }

        void AddRest(Matrix4x4 at, float seconds, byte kind)
        {
            if (rests.Count >= MaxRests) rests.RemoveAt(0);
            rests.Add(new Rest { At = at, Until = Time.time + seconds, Kind = kind });
        }

        void AddMark(float x, float z, float yawDegrees, Vector2 size, float life, byte kind)
        {
            if (marks.Count >= MaxMarks) marks.RemoveAt(0);
            Vector3 at = new Vector3(x, RenderGround.Sample(Host.Local.Map, x, z) + 0.025f, z);
            marks.Add(new Mark { At = Matrix4x4.TRS(at, Lie(x, z, yawDegrees, 0.2f), new Vector3(size.x, 1f, size.y)), Born = Time.time, Life = life, Kind = kind });
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
            SceneHooks.Sparks = null;
            books?.Dispose();
            foreach (var mat in new[] { waterMat, birdMat, sparkMat, tracerNightA, tracerNightB, tracerCore, tracerMat, bodyMatA, bodyMatB, burstMat, markMine, markTheirs, aimMat, dirtMat, woodMat, smokeMat, smokeThin, smokeFaint, flashMat }) if (mat != null) Destroy(mat);
            foreach (var mat in gasMats) if (mat != null) Destroy(mat);
            foreach (var mat in markMats) if (mat != null) Destroy(mat);
            foreach (var mat in new[] { fallenMat, brassMat, helmetMat, vapourMat }) if (mat != null) Destroy(mat);
            foreach (var mesh in fallen) if (mesh != null) Destroy(mesh);
            if (markQuad != null) Destroy(markQuad);
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
                    bool drawn = books != null && books.Ready;
                    if (e.Scalar < 0.5f && drawn)
                    {
                        // the flare at the muzzle lies along the shot; over-bright at night so the bloom takes it
                        float flare = (1.05f + UnityEngine.Random.value * 0.4f) * scale;
                        books.Add(FlipbookFx.Book.Muzzle, from + direction * (0.55f * scale), flare, 0.11f, UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            roll: FlipbookFx.ScreenRoll(cam, direction) + Mathf.PI, glow: SceneMood.Night ? 3.2f : 1.6f);
                    }
                    else if (flashes.Count < 256 && e.Scalar < 0.5f) flashes.Add(new Flash { Pos = from + direction * (0.65f * scale), Direction = direction, Born = Time.time });
                    // a rifle leaves a little smoke at the muzzle: one small puff that drifts forward and thins out. Capped well
                    // under the chunk budget so a big firefight never starves the shell bursts of theirs.
                    // the round that misses lands somewhere: a spurt of dirt beside the man shot at, a splash and a ring if he
                    // stands in water, now and then a ricochet spark at night. A few a frame at most, whatever the firefight.
                    if (e.Scalar < 0.5f && impactsThisFrame < 5 && chunks.Count < 520)
                    {
                        impactsThisFrame++;
                        float angle = UnityEngine.Random.value * 6.2832f, off = UnityEngine.Random.Range(0.35f, 1.7f);
                        Vector3 hit = new Vector3(to.x + Mathf.Cos(angle) * off, 0f, to.z + Mathf.Sin(angle) * off);
                        hit.y = RenderGround.Sample(Host.Local.Map, hit.x, hit.z);
                        bool mirror = UnityEngine.Random.value < 0.5f;
                        if (SceneHooks.IsWater != null && SceneHooks.IsWater(hit.x, hit.z))
                        {
                            SceneHooks.AddRing?.Invoke(hit.x, hit.z, 0.8f);
                            Throw(hit + Vector3.up * 0.4f, 3, 4, 3.4f, 0.05f);
                            // a round in the water stands up a little white column
                            if (drawn) books.Add(FlipbookFx.Book.Splash, hit, 0.55f * scale, 0.55f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0), alpha: 0.9f);
                        }
                        else
                        {
                            Throw(hit, drawn ? 2 : 3, 0, 3.0f, 0.06f);
                            if (SceneMood.Night && UnityEngine.Random.value < 0.22f) Throw(hit, 1, 3, 7f, 0.03f);
                            // and in the mud a spurt of dust that leans away from the shooter
                            if (drawn) books.Add(FlipbookFx.Book.Spurt, hit, (0.8f + UnityEngine.Random.value * 0.4f) * scale, 0.4f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (Vector3.Dot(direction, cam != null ? cam.transform.right : Vector3.right) < 0f ? FlipbookFx.Kind.Mirror : 0), grow: 0.3f, alpha: 0.85f, pop: 0.3f);
                        }
                    }
                    if (e.Scalar < 0.5f && chunks.Count < 420)
                        chunks.Add(new Chunk { Pos = from + direction * (0.9f * scale), Vel = direction * 1.4f + Vector3.up * 0.35f, Born = Time.time, Life = UnityEngine.Random.Range(1.1f, 1.9f), Size = 0.16f * scale * UnityEngine.Random.Range(0.8f, 1.3f), Kind = 2 });
                    if (e.Scalar < 0.5f && chunks.Count < 600 && Near(from, 34f))
                    {
                        // up close every shot throws its case out to the right, and the barrel keeps a thread of smoke
                        Vector3 right = Vector3.Cross(Vector3.up, direction).normalized;
                        chunks.Add(new Chunk { Pos = from + direction * 0.25f + right * 0.08f, Vel = right * UnityEngine.Random.Range(1.5f, 2.5f) + Vector3.up * UnityEngine.Random.Range(1.6f, 2.4f) - direction * UnityEngine.Random.Range(0.1f, 0.6f),
                            Born = Time.time, Life = 3f, Size = 1f, Kind = 5 });
                        chunks.Add(new Chunk { Pos = from + direction * 0.7f, Vel = direction * 0.25f + Vector3.up * 0.5f, Born = Time.time, Life = UnityEngine.Random.Range(1.8f, 2.6f), Size = 0.06f, Kind = 7 });
                    }
                    break;
                }
                case SimEventType.Hit:
                {
                    // a man struck: a spike of light where the round lands and a small cloud off his coat, at chest height for
                    // his stance. A ricochet (negative damage) is only the spike. A few a frame at most, whatever the fight.
                    if (books == null || !books.Ready || hitsThisFrame >= 8 || e.B < 0 || e.B >= w.HighWater) break;
                    hitsThisFrame++;
                    var cam = Camera.main;
                    float scale = 1f;
                    if (units != null)
                    {
                        float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
                        scale = units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
                    }
                    var stance = (Stance)w.StanceOf[e.B];
                    bool vehicle = (w.Flags[e.B] & (uint)UnitFlags.Vehicle) != 0;
                    float chest = vehicle ? 1.4f : stance == Stance.Prone || stance == Stance.Pinned ? 0.3f : stance == Stance.Crouch ? 0.75f : 1.05f;
                    Vector3 p = (Vector3)w.Position[e.B];
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + chest * scale;
                    Vector3 toward = new Vector3(e.Dir.x, 0f, e.Dir.z); if (toward.sqrMagnitude < 0.01f) toward = Vector3.forward;
                    p -= toward.normalized * (0.18f * scale);   // on the side the round came from
                    p += new Vector3(UnityEngine.Random.Range(-0.12f, 0.12f), UnityEngine.Random.Range(-0.15f, 0.15f), UnityEngine.Random.Range(-0.12f, 0.12f)) * scale;
                    if (vehicle) books.Add(FlipbookFx.Book.Star, p, 0.9f * scale * UnityEngine.Random.Range(0.8f, 1.2f), 0.07f, roll: UnityEngine.Random.value * 6.2832f, glow: SceneMood.Night ? 3f : 1.8f);
                    else books.Add(FlipbookFx.Book.Flash, p, 1.2f * scale, 0.07f, roll: UnityEngine.Random.value * 6.2832f, glow: SceneMood.Night ? 2.2f : 1.4f, pop: 0.5f);
                    if (e.Scalar > 0f)
                        books.Add(FlipbookFx.Book.Puff, p, (vehicle ? 1.2f : 1.2f) * scale, 0.7f, UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: toward.normalized * 0.6f + Vector3.up * 0.5f, grow: 0.7f, roll: UnityEngine.Random.Range(-0.5f, 0.5f), alpha: 0.85f, pop: 0.4f);
                    if (vehicle && SceneMood.Night) Throw(p, 4, 3, 5f, 0.03f);   // sparks off armour
                    break;
                }
                case SimEventType.Death:
                {
                    if (bodies.Count >= MaxBodies) bodies.RemoveAt(0);
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.02f;
                    byte team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0;
                    float fellYaw = Mathf.Atan2(e.Dir.x, e.Dir.z) * Mathf.Rad2Deg;
                    int death = (Mathf.FloorToInt(p.x * 13f) ^ Mathf.FloorToInt(p.z * 29f)) & 3;
                    // he goes down as the figure he was (VATRenderer plays the death and holds it); without it, a still box figure
                    if (units != null && units.Ready)
                    {
                        // the controller chose the death for his stance, gait and the side the shot came from; it is drawn where he fell
                        var anim = Host != null ? Host.Animation : null;
                        bool controlled = anim != null && Host.UseAnimationController && e.A >= 0 && e.A < w.HighWater;
                        Clip deathClip = controlled ? anim.State[e.A].Clip : Clip.None;
                        float yaw = controlled ? anim.State[e.A].ShownYaw : e.A >= 0 && e.A < w.HighWater ? w.Yaw[e.A] : fellYaw * Mathf.Deg2Rad;
                        units.AddFallen(new Vector3(p.x, p.y - 0.02f, p.z), yaw, team, death, deathClip, e.A >= 0 && e.A < w.HighWater ? w.Archetype[e.A] : 0);
                    }
                    else bodies.Add(new Body { Pos = p, Rot = Lie(p.x, p.z, fellYaw, 0.6f), Team = team, Variant = (byte)death });
                    // his helmet comes off as he goes down and rolls a step away
                    if (!(units != null && units.Ready) && Near(p, 60f) && chunks.Count < 700)   // the animated figure keeps his helmet on
                        chunks.Add(new Chunk { Pos = p + Vector3.up * 1.2f, Vel = Quaternion.Euler(0f, fellYaw + UnityEngine.Random.Range(-70f, 70f), 0f) * Vector3.forward * UnityEngine.Random.Range(1.2f, 2.4f) + Vector3.up * 1.6f,
                            Born = Time.time, Life = 4f, Size = 1f, Kind = 6 });
                    break;
                }
                case SimEventType.Explosion:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
                    bool wet = SceneHooks.IsWater != null && SceneHooks.IsWater(p.x, p.z);
                    bool drawn = books != null && books.Ready;
                    if (drawn)
                    {
                        // the drawn burst: its own light for an instant, the earth (or water) stood up in a column, the low
                        // burst running out either side, and the boiling cloud that rises off it and thins
                        float r = Mathf.Clamp(e.Scalar, 2f, 9f);
                        bool mirror = ((Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 1) == 0;
                        var ground = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
                        Vector4 wind = Shader.GetGlobalVector(WindGlobalId); Vector3 drift = new Vector3(wind.x, 0f, wind.y) * 3.5f + Vector3.up * 0.55f;   // _TWWind is the breeze at 0.034 per m/s (Atmosphere)
                        books.Add(FlipbookFx.Book.Flash, p + Vector3.up * (r * 0.3f), r * 2.2f, 0.14f, roll: UnityEngine.Random.value * 6.2832f, glow: SceneMood.Night ? 5f : 2.5f, pop: 0.5f);
                        books.Add(wet ? FlipbookFx.Book.Splash : FlipbookFx.Book.Column, p, r * (wet ? 0.8f : 1.4f), wet ? 1.2f : 1.5f, ground | (mirror ? FlipbookFx.Kind.Mirror : 0), grow: 0.2f, alpha: wet ? 0.85f : 1f, pop: 0.15f);
                        // the two wings are not a mirror pair: the second is born a little later and a little smaller
                        books.Add(FlipbookFx.Book.Wings, p, r * 1.7f, 0.95f, ground, grow: 0.4f, alpha: wet ? 0.6f : 0.9f, pop: 0.2f);
                        books.Add(FlipbookFx.Book.Wings, p + Vector3.up * 0.1f, r * 1.45f, 1.1f, ground | FlipbookFx.Kind.Mirror, grow: 0.5f, alpha: wet ? 0.5f : 0.8f, pop: 0.1f);
                        if (!wet)
                        {
                            books.Add(FlipbookFx.Book.Burst, p + Vector3.up * (r * 0.55f), r * 1.9f, 1.8f, FlipbookFx.Kind.Upright | (mirror ? 0 : FlipbookFx.Kind.Mirror),
                                velocity: Vector3.up * (r * 0.3f) + drift, grow: 0.5f, roll: UnityEngine.Random.Range(-0.15f, 0.15f), glow: SceneMood.Night ? 2.6f : 1.6f, pop: 0.3f);
                            // what a burst leaves: dark smoke that climbs, spreads and drifts off down wind for seconds
                            for (int k = 0; k < 5; k++)
                            {
                                Vector3 off = new Vector3(UnityEngine.Random.Range(-0.5f, 0.5f), 0.3f + k * 0.18f, UnityEngine.Random.Range(-0.5f, 0.5f)) * r;
                                books.Add(FlipbookFx.Book.Smoke, p + off, r * UnityEngine.Random.Range(0.8f, 1.2f), UnityEngine.Random.Range(4f, 6.5f), (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                    velocity: drift * UnityEngine.Random.Range(1.4f, 2.2f) + Vector3.up * 0.4f, grow: 1.8f, roll: UnityEngine.Random.Range(-0.6f, 0.6f), alpha: 0.65f, pop: 0.3f, delay: 0.5f + k * 0.15f);
                            }
                        }
                    }
                    else if (bursts.Count < 64) bursts.Add(new Burst { Pos = p, Radius = e.Scalar, Born = Time.time, Variant = (Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 3 });
                    if (wet) Throw(p + Vector3.up * 0.4f, drawn ? 14 : 26, 4, 12f, 0.13f);   // a shell in the water throws a white column, not earth
                    else
                    {
                        Throw(p, drawn ? 8 : 14, 0, 9f, 0.22f);
                        // a fresh hole: clods lie thrown round its rim, and the hot earth steams in the rain (seen from close by)
                        float rim = Mathf.Clamp(e.Scalar * 0.55f, 1.2f, 4.5f);
                        for (int k = 0; k < 8; k++)
                        {
                            float a = (k + UnityEngine.Random.value) * 0.785f, d = rim * UnityEngine.Random.Range(0.75f, 1.5f), s = UnityEngine.Random.Range(0.10f, 0.26f);
                            float cx = p.x + Mathf.Cos(a) * d, cz = p.z + Mathf.Sin(a) * d;
                            AddRest(Matrix4x4.TRS(new Vector3(cx, RenderGround.Sample(Host.Local.Map, cx, cz) + s * 0.25f, cz), Quaternion.Euler(a * 97f, a * 311f, a * 53f), new Vector3(s * 1.3f, s * 0.7f, s)), 140f, 2);
                        }
                        if (hotCraters.Count >= 16) hotCraters.RemoveAt(0);
                        hotCraters.Add(new Vector4(p.x, p.y, p.z, Time.time + 22f));
                    }
                    Throw(p + Vector3.up * 0.5f, 4, 2, 1.6f, 1.6f);
                    Startle(p);
                    CameraShake.Add(p, e.Scalar);
                    if (SceneMood.Night) Throw(p + Vector3.up * 0.3f, 14, 3, 15f, 0.05f);   // burning fragments arc out of the burst and die on the way down
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
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, p, 2.2f, 0.7f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.8f, grow: 0.6f, alpha: 0.7f);
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
                float thick = (!night ? 0.045f : side == 2 ? 0.075f : 0.24f) * Mathf.Lerp(1f, 0.30f, SceneHooks.CloseUp);   // sized for the standard view; among the men a round is a thin line
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
            books?.Draw(now, bounds);
            hitsThisFrame = 0;

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

            // gas: drawn clouds that boil slowly over each 4 m field cell (the old translucent blocks stand in without the books)
            var gas = Host.Local.Gas;
            if (gas != null && gas.Active && books != null && books.Ready)
            {
                float cs = TW.Sim.Terrain.MapData.FieldCellSize;
                gasCards.Clear();
                for (int z = 0; z < gas.Length; z++)
                for (int x = 0; x < gas.Width; x++)
                {
                    float c = gas.Gas[z * gas.Width + x];
                    if (c < 0.8f) continue;
                    uint h = (uint)(x * 73856093 ^ z * 19349663);
                    float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f, h3 = ((h >> 20) & 1023) / 1023f;
                    float thick = Mathf.Clamp01(c / 14f);
                    float wx = (x + 0.5f) * cs + (h1 - 0.5f) * 2.4f, wz = (z + 0.5f) * cs + (h2 - 0.5f) * 2.4f;
                    float ground = RenderGround.Sample(Host.Local.Map, wx, wz);
                    float slow = now * 0.22f + h3 * 6.2832f;
                    // the cloud boils: the book's solid frames back and forth, a slow roll, a slow rise and fall
                    float frame = 1.2f + 2.6f * (0.5f + 0.5f * Mathf.Sin(slow));
                    float width = (4.6f + 2.2f * thick) * (0.9f + 0.2f * h2);
                    var kind = FlipbookFx.Kind.Upright | (((h >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                    gasCards.Add(FlipbookFx.Pack(new Vector3(wx, ground + width * 0.32f + 0.3f * Mathf.Sin(slow * 0.7f), wz), width, width, frame, 1f, 1f, (h1 - 0.5f) * 0.6f + now * 0.03f, kind, 0.22f + 0.5f * thick));
                    if (c > 6f)   // a dense cell holds a second, smaller cloud a little off and higher
                        gasCards.Add(FlipbookFx.Pack(new Vector3(wx + (h3 - 0.5f) * 3f, ground + width * 0.55f, wz + (h1 - 0.5f) * 3f), width * 0.7f, width * 0.7f, 4.0f - frame * 0.5f, 1f, 1f, (h2 - 0.5f) * 0.6f - now * 0.02f, kind ^ FlipbookFx.Kind.Mirror, 0.2f + 0.4f * thick));
                }
                books.DrawPacked(FlipbookFx.Book.Gas, gasCards, bounds);
            }
            else if (gas != null && gas.Active)
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

            // the fallen: a still figure in one of four deaths, lying on the slope where he fell
            float grow = 1f;
            if (units != null && view != null) grow = units.UnitScale * Mathf.Clamp((view.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f) / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
            var rpF = new RenderParams(fallenMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = true };
            for (int kind = 0; kind < 8; kind++)
            {
                batch.Clear();
                for (int i = 0; i < bodies.Count; i++)
                {
                    var b = bodies[i];
                    if ((b.Team & 1) * 4 + b.Variant != kind) continue;
                    batch.Add(Matrix4x4.TRS(b.Pos, b.Rot, new Vector3(grow, grow, grow)));
                    if (batch.Count == 1023) Flush(fallen[kind], rpF);
                }
                if (batch.Count > 0) Flush(fallen[kind], rpF);
            }
            DrawClose(now, bounds);
        }

        /// <summary>What has come to rest (brass, helmets, clods) and what is pressed into the mud (boot prints, track ruts): close camera only.</summary>
        void DrawClose(float now, Bounds bounds)
        {
            rests.RemoveAll(r => now > r.Until);
            marks.RemoveAll(m => now - m.Born > m.Life);
            if (SceneHooks.CloseUp <= 0f) return;
            var cam = Camera.main; if (cam == null) return;
            Vector3 eye = cam.transform.position;
            for (int kind = 0; kind < 3; kind++)
            {
                batch.Clear();
                var rp = new RenderParams(kind == 0 ? brassMat : kind == 1 ? helmetMat : dirtMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = true };
                for (int i = 0; i < rests.Count; i++)
                {
                    var r = rests[i]; if (r.Kind != kind) continue;
                    float dx = r.At.m03 - eye.x, dz = r.At.m23 - eye.z; if (dx * dx + dz * dz > CloseReach * CloseReach) continue;
                    batch.Add(r.At);
                    if (batch.Count == 1023) Flush(kind == 1 ? sphere : cube, rp);
                }
                if (batch.Count > 0) Flush(kind == 1 ? sphere : cube, rp);
            }
            if (markMats[0] == null) return;
            for (int pass = 0; pass < 6; pass++)
            {
                batch.Clear();
                var rp = new RenderParams(markMats[pass]) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < marks.Count; i++)
                {
                    var m = marks[i]; if (m.Kind != pass / 3) continue;
                    float k = (now - m.Born) / m.Life;
                    if ((k < 0.5f ? 0 : k < 0.8f ? 1 : 2) != pass % 3) continue;
                    float dx = m.At.m03 - eye.x, dz = m.At.m23 - eye.z; if (dx * dx + dz * dz > CloseReach * CloseReach) continue;
                    batch.Add(m.At);
                    if (batch.Count == 1023) Flush(markQuad, rp);
                }
                if (batch.Count > 0) Flush(markQuad, rp);
            }
        }

        /// <summary>Boot prints behind walking men, ruts and flung mud behind tanks, breath in the cold, steam off fresh craters.</summary>
        void CloseLife(float now, Camera cam)
        {
            if (SceneHooks.CloseUp <= 0f) { if (trails.Count > 0) trails.Clear(); return; }
            var w = Host.Local.World; var map = Host.Local.Map;
            Vector3 eye = cam.transform.position;
            float rain = Shader.GetGlobalVector(WetId).z;
            if (now >= nextPrint)
            {
                nextPrint = now + 0.1f;
                for (int i = 0; i < w.HighWater; i++)
                {
                    uint flags = w.Flags[i];
                    if ((flags & (uint)UnitFlags.Alive) == 0) continue;
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z; if (dx * dx + dz * dz > CloseReach * CloseReach) continue;
                    bool tank = (flags & (uint)UnitFlags.Vehicle) != 0;
                    Vector3 here = new Vector3(p.x, 0f, p.z);
                    if (!trails.TryGetValue(i, out var trail)) { trails[i] = new Trail { Last = here, Seen = now }; continue; }
                    trail.Seen = now;
                    Vector3 step = here - trail.Last; float far = step.magnitude, stride = tank ? 0.85f : 0.72f;
                    if (far > 6f) { trail.Last = here; trails[i] = trail; continue; }   // the slot was reused by another man
                    if (far >= stride)
                    {
                        Vector3 dir = step / far, side = new Vector3(dir.z, 0f, -dir.x);
                        float yawDeg = Mathf.Atan2(dir.x, dir.z) * Mathf.Rad2Deg;
                        bool dryFooting = !tank && (flags & (uint)UnitFlags.InTrench) != 0;   // duckboards take no print
                        for (float d = stride; d <= far && d < stride * 4.5f; d += stride)
                        {
                            Vector3 at = trail.Last + dir * d;
                            if (dryFooting || (SceneHooks.IsWater != null && SceneHooks.IsWater(at.x, at.z))) continue;
                            if (tank)
                            {
                                AddMark(at.x + side.x * 0.78f, at.z + side.z * 0.78f, yawDeg, new Vector2(0.50f, 0.92f), 70f, 1);
                                AddMark(at.x - side.x * 0.78f, at.z - side.z * 0.78f, yawDeg, new Vector2(0.50f, 0.92f), 70f, 1);
                            }
                            else
                            {
                                float foot = trail.Left ? -0.11f : 0.11f; trail.Left = !trail.Left;
                                AddMark(at.x + side.x * foot, at.z + side.z * foot, yawDeg + (trail.Left ? 7f : -7f), new Vector2(0.15f, 0.34f), 45f, 0);
                            }
                        }
                        if (tank && chunks.Count < 560)
                        {
                            // the tracks fling what they lift
                            Vector3 rear = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + 0.3f, p.z) - dir * 1.9f;
                            Throw(rear + side * 0.78f, 1, 0, 2.6f, 0.07f); Throw(rear - side * 0.78f, 1, 0, 2.6f, 0.07f);
                        }
                        trail.Last = here;
                    }
                    trails[i] = trail;
                }
                if (trails.Count > 96)
                {
                    trailSweep.Clear();
                    foreach (var kv in trails) if (now - kv.Value.Seen > 1.5f) trailSweep.Add(kv.Key);
                    for (int k = 0; k < trailSweep.Count; k++) trails.Remove(trailSweep[k]);
                }
            }
            if (now >= nextExhaust)
            {
                nextExhaust = now + 0.3f;
                for (int i = 0; i < w.HighWater && chunks.Count < 560; i++)
                {
                    if ((w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) != ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) continue;
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                    float yaw = w.Yaw[i];
                    Vector3 back = new Vector3(-Mathf.Sin(yaw), 0f, -Mathf.Cos(yaw));
                    chunks.Add(new Chunk { Pos = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + 1.5f, p.z) + back * 1.7f, Vel = back * 0.8f + Vector3.up * 0.9f, Born = now, Life = UnityEngine.Random.Range(1.6f, 2.4f), Size = 0.2f, Kind = 2 });
                }
                // rain on the hot earth of a fresh hole
                hotCraters.RemoveAll(h => now > h.w);
                if (rain > 0.05f)
                    for (int k = 0; k < hotCraters.Count && chunks.Count < 560; k++)
                    {
                        Vector3 at = hotCraters[k]; float dx = at.x - eye.x, dz = at.z - eye.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                        float heat = (hotCraters[k].w - now) / 22f;
                        if (UnityEngine.Random.value > heat) continue;
                        Vector2 r = UnityEngine.Random.insideUnitCircle * 1.2f;
                        chunks.Add(new Chunk { Pos = at + new Vector3(r.x, 0.1f, r.y), Vel = new Vector3(0f, 0.7f, 0f), Born = now, Life = UnityEngine.Random.Range(1.8f, 3f), Size = 0.22f, Kind = 7 });
                    }
            }
            if (SceneMood.Night && now >= nextBreath && w.HighWater > 0)
            {
                // a cold night: the men nearest the view breathe out a little cloud, one man at a time
                nextBreath = now + 0.35f;
                for (int n = 0; n < w.HighWater && n < 400; n++)
                {
                    int i = (breathCursor + n) % w.HighWater;
                    uint flags = w.Flags[i];
                    if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z; if (dx * dx + dz * dz > 20f * 20f) continue;
                    var stance = (Stance)w.StanceOf[i];
                    float head = stance == Stance.Prone || stance == Stance.Pinned ? 0.35f : stance == Stance.Crouch ? 1.05f : 1.58f;
                    float yaw = w.Yaw[i]; Vector3 ahead = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
                    if (chunks.Count < 600)
                        chunks.Add(new Chunk { Pos = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + head, p.z) + ahead * 0.16f, Vel = ahead * 0.45f + Vector3.up * 0.1f, Born = now, Life = UnityEngine.Random.Range(0.9f, 1.4f), Size = 0.05f, Kind = 7 });
                    breathCursor = i + 1; break;
                }
            }
        }

        /// <summary>Throw debris: dirt and splinters fly and fall, smoke rises, swells and thins.</summary>
        /// <summary>A shell burst puts up the crows from the nearest standing timber: they climb away from the blast and are gone.</summary>
        void Startle(Vector3 burst)
        {
            if (Time.time - lastFlock < 0.8f || birds.Count > MaxBirds - 10) return;
            var props = Host.Local.Map.Props;
            int best = -1; float bestSq = 45f * 45f;
            for (int i = 0; i < props.Length; i++)
            {
                var kind = props[i].Kind;
                if (kind != TW.Sim.Terrain.PropKind.Tree && kind != TW.Sim.Terrain.PropKind.BrokenTree) continue;
                float dx = props[i].Pos.x - burst.x, dz = props[i].Pos.z - burst.z, sq = dx * dx + dz * dz;
                if (sq < bestSq && sq > 9f) { bestSq = sq; best = i; }
            }
            if (best < 0) return;
            lastFlock = Time.time;
            Vector3 perch = new Vector3(props[best].Pos.x, RenderGround.Sample(Host.Local.Map, props[best].Pos.x, props[best].Pos.z) + 4.5f, props[best].Pos.z);
            Vector3 away = perch - burst; away.y = 0f; away = away.sqrMagnitude > 0.01f ? away.normalized : Vector3.forward;
            int flock = UnityEngine.Random.Range(5, 10);
            for (int k = 0; k < flock; k++)
            {
                Vector3 dir = Quaternion.Euler(0f, UnityEngine.Random.Range(-40f, 40f), 0f) * away;
                birds.Add(new Bird { Pos = perch + UnityEngine.Random.insideUnitSphere * 1.2f, Vel = dir * UnityEngine.Random.Range(6f, 10f) + Vector3.up * UnityEngine.Random.Range(3f, 6f),
                    Born = Time.time + k * 0.06f, Phase = UnityEngine.Random.value * 6.28f });
            }
        }

        void DrawBirds(float now, Bounds bounds)
        {
            birds.RemoveAll(b => now - b.Born > BirdLife);
            if (birds.Count == 0) return;
            float dt = Time.deltaTime;
            batch.Clear();
            var rp = new RenderParams(birdMat) { worldBounds = new Bounds(bounds.center, bounds.size + new Vector3(200f, 120f, 200f)), shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < birds.Count; i++)
            {
                var b = birds[i];
                float age = now - b.Born; if (age < 0f) continue;
                b.Vel = Vector3.Lerp(b.Vel, new Vector3(b.Vel.x, 1.2f, b.Vel.z).normalized * 9f, dt * 0.8f);   // the climb flattens into flight
                b.Pos += b.Vel * dt; birds[i] = b;
                float flap = Mathf.Sin(age * 15f + b.Phase) * 48f, size = Mathf.Clamp01((BirdLife - age) * 0.7f);
                var body = Quaternion.LookRotation(b.Vel);
                for (int wing = -1; wing <= 1; wing += 2)
                    batch.Add(Matrix4x4.TRS(b.Pos, body * Quaternion.Euler(0f, 0f, wing * flap), Vector3.one) * Matrix4x4.TRS(new Vector3(wing * 0.2f, 0f, 0f), Quaternion.identity, new Vector3(0.40f, 0.025f, 0.15f) * size));
                batch.Add(Matrix4x4.TRS(b.Pos, body, new Vector3(0.07f, 0.07f, 0.30f) * size));
                if (batch.Count >= 1020) Flush(cube, rp);
            }
            if (batch.Count > 0) Flush(cube, rp);
        }

        /// <summary>Running men kick up mud, and chimneys and rained-on fires smoke: a few small chunks a step, near the view only.</summary>
        void Ambient(float now)
        {
            var cam = Camera.main; if (cam == null) return;
            Vector3 look = cam.transform.position + cam.transform.forward * (cam.transform.position.y / Mathf.Max(0.15f, -cam.transform.forward.y));
            if (now >= nextKick)
            {
                nextKick = now + 0.2f;
                var w = Host.Local.World; int found = 0;
                for (int n = 0; n < w.HighWater && n < 300 && found < 5 && chunks.Count < 480; n++)
                {
                    int i = (kickCursor + n) % w.HighWater;
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    var v = w.Velocity[i]; if (v.x * v.x + v.z * v.z < 4f) continue;
                    var p = w.Position[i]; float dx = p.x - look.x, dz = p.z - look.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                    Vector3 at = new Vector3(p.x, RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.05f, p.z);
                    if (SceneHooks.IsWater != null && SceneHooks.IsWater(at.x, at.z)) Throw(at + Vector3.up * 0.3f, 1, 4, 2.2f, 0.04f); else Throw(at, 1, 0, 1.7f, 0.045f);
                    found++; kickCursor = i + 1;
                }
                if (found < 5 && w.HighWater > 0) kickCursor = (kickCursor + 300) % w.HighWater;
            }
            if (now >= nextSmoke)
            {
                nextSmoke = now + 0.45f;
                for (int s = 0; s < SceneHooks.SmokeSources.Count && chunks.Count < 460; s++)
                {
                    Vector3 at = SceneHooks.SmokeSources[s];
                    if ((at - look).sqrMagnitude > 110f * 110f) continue;
                    chunks.Add(new Chunk { Pos = at, Vel = new Vector3(UnityEngine.Random.Range(-0.2f, 0.2f), 0.9f, UnityEngine.Random.Range(-0.2f, 0.2f)), Born = now, Life = UnityEngine.Random.Range(2.4f, 3.6f), Size = 0.22f, Kind = 2 });
                }
            }
        }

        void Throw(Vector3 at, int count, byte kind, float speed, float size)
        {
            for (int k = 0; k < count && chunks.Count < MaxChunks; k++)
            {
                Vector3 dir = UnityEngine.Random.onUnitSphere; dir.y = Mathf.Abs(dir.y) * (kind == 2 ? 0.4f : 1.4f) + 0.2f;
                chunks.Add(new Chunk { Pos = at, Vel = dir.normalized * speed * UnityEngine.Random.Range(0.5f, 1.2f), Born = Time.time, Life = kind == 2 ? UnityEngine.Random.Range(3.5f, 6f) : kind == 3 ? UnityEngine.Random.Range(0.45f, 1.1f) : UnityEngine.Random.Range(0.9f, 1.7f),
                    Size = size * UnityEngine.Random.Range(0.6f, 1.5f), Kind = kind });
            }
        }

        void DrawChunks(float now, Bounds bounds)
        {
            if (smokeMat == null) smokeMat = Transparent(Shader.Find("Universal Render Pipeline/Unlit"), new Color(0.16f, 0.15f, 0.14f, 0.30f));
            chunks.RemoveAll(c => now - c.Born > c.Life);
            float dt = Time.deltaTime; int landings = 0;
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind == 2) { c.Vel = Vector3.Lerp(c.Vel, new Vector3(0f, 1.2f, -0.8f), dt * 1.5f); }   // drifts up and down wind
                else if (c.Kind == 7) { c.Vel = Vector3.Lerp(c.Vel, new Vector3(0f, 0.45f, -0.35f), dt * 1.2f); }
                else c.Vel += Vector3.down * 9.8f * dt;
                c.Pos += c.Vel * dt;
                if (c.Kind == 0 && c.Size > 0.12f && c.Vel.y < -2f && books != null && books.Ready && landings < 6)
                {
                    // a big clod thrown by a burst lands: a little spurt of dust where it hits, and it is done
                    float floor = RenderGround.Sample(Host.Local.Map, c.Pos.x, c.Pos.z);
                    if (c.Pos.y <= floor + 0.05f)
                    {
                        landings++;
                        if (SceneHooks.IsWater != null && SceneHooks.IsWater(c.Pos.x, c.Pos.z)) SceneHooks.AddRing?.Invoke(c.Pos.x, c.Pos.z, 0.5f);
                        else books.Add(FlipbookFx.Book.Spurt, new Vector3(c.Pos.x, floor, c.Pos.z), c.Size * 3.5f, 0.35f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | ((i & 1) == 0 ? FlipbookFx.Kind.Mirror : 0), alpha: 0.7f, pop: 0.3f);
                        c.Life = 0f;
                    }
                }
                if (c.Kind == 5 || c.Kind == 6)
                {
                    float floor = RenderGround.Sample(Host.Local.Map, c.Pos.x, c.Pos.z) + (c.Kind == 5 ? 0.012f : 0.06f);
                    if (c.Pos.y <= floor && c.Vel.y < 0f)
                    {
                        c.Pos.y = floor;
                        if (c.Vel.y < -1.4f) c.Vel = new Vector3(c.Vel.x * 0.45f, -c.Vel.y * 0.32f, c.Vel.z * 0.45f);   // one bounce
                        else
                        {
                            bool water = SceneHooks.IsWater != null && SceneHooks.IsWater(c.Pos.x, c.Pos.z);
                            if (water) SceneHooks.AddRing?.Invoke(c.Pos.x, c.Pos.z, c.Kind == 5 ? 0.35f : 0.9f);
                            else if (c.Kind == 5) AddRest(Matrix4x4.TRS(c.Pos, Lie(c.Pos.x, c.Pos.z, c.Born * 733f, 0.15f), new Vector3(0.020f, 0.020f, 0.085f)), 9f, 0);
                            else AddRest(Matrix4x4.TRS(c.Pos + Vector3.up * 0.02f, Lie(c.Pos.x, c.Pos.z, c.Born * 733f) * Quaternion.Euler(UnityEngine.Random.Range(-16f, 16f), 0f, UnityEngine.Random.Range(150f, 210f)), new Vector3(0.33f, 0.15f, 0.35f)), 150f, 1);
                            c.Life = 0f;
                        }
                    }
                }
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
            // water thrown up by rounds, shells and boots: pale drops under gravity
            batch.Clear();
            var rpW = new RenderParams(waterMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind != 4) continue;
                float s = c.Size * (1f - 0.6f * (now - c.Born) / c.Life);
                batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.identity, new Vector3(s, s * 1.6f, s)));
                if (batch.Count == 1023) Flush(cube, rpW);
            }
            if (batch.Count > 0) Flush(cube, rpW);
            // in flight: brass cases and helmets tumble; breath, muzzle threads and crater steam are a pale vapour
            for (int kind = 5; kind <= 7; kind++)
            {
                batch.Clear();
                var rpC = new RenderParams(kind == 5 ? brassMat : kind == 6 ? helmetMat : vapourMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                for (int i = 0; i < chunks.Count; i++)
                {
                    var c = chunks[i];
                    if (c.Kind != kind) continue;
                    float k = (now - c.Born) / Mathf.Max(0.01f, c.Life);
                    if (kind == 7) { float s = c.Size * (1f + 3.2f * k) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.45f, 1f, k))); batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.identity, new Vector3(s, s, s))); }
                    else batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.Euler(now * 640f + c.Born * 997f, c.Born * 613f, now * 410f), kind == 5 ? new Vector3(0.020f, 0.020f, 0.085f) : new Vector3(0.33f, 0.15f, 0.35f)));
                    if (batch.Count == 1023) Flush(kind == 5 ? cube : kind == 6 ? sphere : puff, rpC);
                }
                if (batch.Count > 0) Flush(kind == 5 ? cube : kind == 6 ? sphere : puff, rpC);
            }
            DrawBirds(now, bounds);
            Ambient(now);
            var lens = Camera.main; if (lens != null) CloseLife(now, lens);
            impactsThisFrame = 0;
            // sparks: a bright streak along its own flight, shrinking as it burns out
            batch.Clear();
            var rpS = new RenderParams(sparkMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (c.Kind != 3 || c.Vel.sqrMagnitude < .01f) continue;
                float burn = 1f - (now - c.Born) / c.Life;
                batch.Add(Matrix4x4.TRS(c.Pos, Quaternion.LookRotation(c.Vel), new Vector3(c.Size * burn, c.Size * burn, c.Size + c.Vel.magnitude * .035f)));
                if (batch.Count == 1023) Flush(cube, rpS);
            }
            if (batch.Count > 0) Flush(cube, rpS);
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
