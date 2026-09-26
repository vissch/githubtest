// Phase: B1 (implemented; C4 VFX: the drawn bursts, hits and flares live in FlipbookFx; B5 ragdolls still stand-ins)
// Makes the fight readable: every Shot event becomes a short-lived tracer with a muzzle flare and a spurt where it
// lands, every Hit a spike and a puff on the man, every Explosion a drawn burst with its column and wings, every Death
// leaves a body, and every trench or objective capture raises a banner. Instanced draws, no GameObjects per effect.
// Listens to SimHost.Events, so it sees exactly what the local sim produced.
// One class in six files (2026-09-25): this one holds the event dispatch (OnSimEvent), Update, the materials and
// the tracer/body/burst pools; CombatFx.Ground.cs what only a close camera sees (marks, rests, trails, breath);
// CombatFx.Chunks.cs thrown chunks and cook-offs; CombatFx.Ambient.cs birds and ambient smoke; CombatFx.Bodies.cs
// gibs, tree breaks and where a man's muzzle and chest are drawn; CombatFx.Deaths.cs what a Death leaves (the body
// from the controller's record, the gibs, a burning man's pool and smoulder); CombatFx.Abilities.cs the aim, the
// strafe's aircraft and tracers, the beam's charge and sweep, the smoke screen. CameraShake is in its own file.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx : MonoBehaviour
    {
        public SimHost Host;
        public float TracerSeconds = 0.12f;
        public int MaxBodies = 600;

        struct Tracer { public Vector3 From, To; public float Born; public bool Hit; public byte Team; }
        struct Body { public Vector3 Pos; public Quaternion Rot; public float Born; public byte Team, Variant; }
        struct Burst { public Vector3 Pos; public float Radius, Born; public int Variant; }
        struct Flash { public Vector3 Pos, Direction; public float Born; }
        struct Marker { public Vector3 Pos, Dir; public float Length, Radius, Until; public bool Mine; }   // Dir, Length: a line ability's corridor (Length 0: a disc)
        const float MarkerSegment = 6f;   // a corridor marker is drawn in pieces this long, each on its own ground sample
        static readonly int WetId = Shader.PropertyToID("_TWWet");
        static readonly int WindGlobalId = Shader.PropertyToID("_TWWind");
        float lastFlock = -10f, nextKick, nextSmoke; int impactsThisFrame, kickCursor;
        Material waterMat, birdMat;
        int tintEpoch = -1;   // which SceneTints.Epoch these materials were last painted for

        readonly List<Tracer> tracers = new List<Tracer>(512);
        readonly List<Body> bodies = new List<Body>(600);
        readonly List<Burst> bursts = new List<Burst>(64);
        readonly List<Flash> flashes = new List<Flash>(256);
        readonly List<Marker> markers = new List<Marker>(8);
        readonly List<Matrix4x4> gasCards = new List<Matrix4x4>(2048);
        // the flamethrower's fire (Flamethrower.cs): jets, pools of burning fuel, men alight, big things burning.
        // Held here because it spends CombatFx's books and CombatFx already has the presenter and the ground.
        readonly Flamethrower flames = new Flamethrower();
        System.Func<int, Vector3> drawnAt; System.Func<float, float, float> groundAt;   // cached: Update must not allocate
        Material dirtMat, woodMat, smokeMat;
        Material smokeThin, smokeFaint;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        readonly Matrix4x4[] batchArray = new Matrix4x4[1023];
        Mesh cube, capsule, sphere, plume, puff, flashMesh;
        Mesh clod;   // a lump for the dirt a burst or a round throws: Unity's cube read as a cube from close by
        Material flashMat;
        Material tracerNightA, tracerNightB, tracerCore, sparkMat;
        /// <summary>
        /// Paint every material and flipbook the biome owns. Called when SceneTints.Epoch moves, not per frame:
        /// the old code compared waterMat.color against a static every frame, which is a native read to decide
        /// whether to do a native write, and it only covered ONE of these.
        /// If the books are not baked yet, tintEpoch is left behind and this runs again next frame.
        /// </summary>
        void ApplyTints()
        {
            if (books == null || !books.Ready) return;
            var t = SceneTints.Now;
            tintEpoch = SceneTints.Epoch;
            if (waterMat != null) waterMat.color = t.Splash;
            // the column is the half of a splash a player actually sees: 28 chunks of 0.18 m are sub-pixel at
            // the standard camera, while this stands up to 11 m out of the water for a second and a half
            books.Tint(FlipbookFx.Book.Splash, t.Splash);
            books.Tint(FlipbookFx.Book.Column, t.Column);
            books.Tint(FlipbookFx.Book.Wings, t.Column);
            books.Tint(FlipbookFx.Book.Spurt, t.Dust);
            books.Tint(FlipbookFx.Book.Puff, t.Dust);
            books.Tint(FlipbookFx.Book.Smoke, t.Smoke);
            if (smokeMat != null) smokeMat.color = new Color(t.Smoke.r, t.Smoke.g, t.Smoke.b, 0.36f);
            if (smokeThin != null) smokeThin.color = new Color(t.Smoke.r, t.Smoke.g, t.Smoke.b, 0.20f);
            if (smokeFaint != null) smokeFaint.color = new Color(t.Smoke.r, t.Smoke.g, t.Smoke.b, 0.07f);
            // the flash stays a mood question rather than a biome one: over-bright under the moon so bloom spreads it
            if (flashMat != null) flashMat.color = SceneMood.Night ? new Color(3.2f, 2.5f, 1.3f) : new Color(1f, 0.91f, 0.65f);
        }

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
        DebrisRenderer debris;   // what a blast breaks off: clods, splinters, a tree's crown, a man's limbs and kit (GPU-side arcs)
        Vector3 lastBlast; float lastBlastAt = -10f;   // the newest burst: what broke this tick fell away from it
        static readonly Color Mud = new Color(0.38f, 0.33f, 0.27f), Bark = new Color(0.36f, 0.30f, 0.24f), Charred = new Color(0.20f, 0.17f, 0.14f);
        static readonly Color ClothA = new Color(0.60f, 0.53f, 0.33f), ClothB = new Color(0.26f, 0.30f, 0.33f), Steel = new Color(0.27f, 0.30f, 0.26f), Skin = new Color(0.72f, 0.54f, 0.42f), Gore = new Color(0.30f, 0.06f, 0.05f);
        int hitsThisFrame;
        TW.Presentation.Units.VATRenderer units;
        string banner; float bannerUntil;
        bool subscribed;
        /// <summary>
        /// Drops the entries that are past it, in place and in order, allocating nothing. List.RemoveAll with a lambda
        /// that closes over now costs a closure and a delegate every frame at each of nine call sites; a static lambda
        /// is one cached delegate for the life of the process. The second argument is whatever the predicate needs
        /// (the time now, or the birth time a live entry must be after), so the predicate never has to capture.
        /// </summary>
        static void Prune<T>(List<T> list, float at, System.Func<T, float, bool> dead)
        {
            int w = 0, n = list.Count;
            for (int r = 0; r < n; r++)
            {
                var x = list[r];
                if (dead(x, at)) continue;
                if (w != r) list[w] = x;
                w++;
            }
            if (w < n) list.RemoveRange(w, n - w);
        }


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
            waterMat = new Material(unlit) { enableInstancing = true, color = SceneTints.Now.Splash };
            birdMat = new Material(unlit) { enableInstancing = true, color = new Color(0.05f, 0.05f, 0.07f) };
            SceneHooks.Sparks = (at, count) => Throw(at, count, 3, 2.5f, 0.04f);
            SceneHooks.CookOff = CookOff;
            // a walker's real footfalls, from the gait that solves its legs; the guess below is only for one nothing draws
            SceneHooks.FootFall = (at, yawDeg, pad) => AddMark(at.x, at.z, yawDeg, pad, SceneTints.Now.Frozen ? 300f : 110f, 2);
            var lens = Camera.main;
            if (lens != null && lens.GetComponent<CameraShake>() == null) lens.gameObject.AddComponent<CameraShake>();
            tracerCore = new Material(unlit) { enableInstancing = true, color = new Color(3.0f, 2.7f, 2.3f) };   // the streak itself: white-hot
            bodyMatA = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.25f, 0.14f) };
            bodyMatB = new Material(lit) { enableInstancing = true, color = new Color(0.19f, 0.22f, 0.28f) };
            sphere = Resources.GetBuiltinResource<Mesh>("Sphere.fbx");
            {
                var lv = new List<Vector3>(); var lt = new List<int>(); var lc = new List<Color>(); var lrng = new DebrisRng(Vector3.one, 5u);
                DebrisRenderer.Lump(lv, lt, lc, 5, 3, 0.5f, 0.30f, ref lrng, Color.white, Color.white, new Vector3(1f, 0.75f, 0.9f));
                clod = new Mesh { name = "Clod", hideFlags = HideFlags.HideAndDontSave };
                clod.SetVertices(lv); clod.SetTriangles(lt, 0); clod.RecalculateNormals(); clod.RecalculateBounds();
            }
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
            units = FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
            books = new FlipbookFx();
            // Where a man is DRAWN standing, which is not where the sim has him: the presenter's y is the sim's zero.
            // This is VATRenderer's own recipe for his feet, and it has to stay VATRenderer's, or fire hung on a man
            // parts company with him exactly when it is most visible - Lift has him climbing a parapet, Hop has him in
            // the air off a shell, and in both the mud under him is not where he is.
            drawnAt = slot =>
            {
                if (Host == null || Host.Presenter == null || Host.Local == null || slot < 0) return Vector3.zero;
                Vector3 p = (Vector3)Host.Presenter.Drawn(slot);
                var map = Host.Local.Map;
                float y = RenderGround.Sample(map, p.x, p.z);
                var anim = Host.Animation;
                if (anim != null && anim.Lift.IsCreated && slot < anim.Lift.Length)
                {
                    float lift = anim.Lift[slot];
                    if (lift > 0f) y = Mathf.Lerp(y, map.Height.Sample(p.x, p.z), lift);   // up the bank with him
                    y += anim.Hop[slot] * FigureScale();                                   // and off the ground with him
                }
                p.y = y;
                return p;
            };
            groundAt = (x, z) => Host != null && Host.Local != null ? RenderGround.Sample(Host.Local.Map, x, z) : 0f;
            // the nozzle is the rifle's muzzle socket: the flamethrower is carried where the rifle is carried, so the
            // stream leaves the figure's own weapon through whatever clip the controller has him in
            flames.Nozzle = slot =>
            {
                if (units != null && units.Sockets(slot, out var at, out var barrel, out _)) return (at, barrel);
                if (Host == null || Host.Local == null || slot < 0) return (Vector3.zero, Vector3.forward);
                EstimateMuzzle(slot, -1, 1f, out var m, out var b);
                return (m, b);
            };
            flames.Alighted = (slot, seconds) =>
            {
                var anim = Host != null ? Host.Animation : null;
                if (anim == null) return;
                if (seconds > 0f) anim.SetAlight(slot, seconds); else anim.Douse(slot);
            };
            // who is standing in the fire. Flamethrower knows where its flame is and nothing about the field, so it
            // hands the point over and this answers. Alight only - fire does not kill anyone here; the sim does that,
            // and a man the sim has already killed is left out of it.
            flames.Catch = (at, radius, seconds) =>
            {
                if (Host == null || Host.Local == null) return;
                var w = Host.Local.World;
                float r2 = radius * radius;
                for (int i = 0; i < w.HighWater; i++)
                {
                    uint fl = w.Flags[i];
                    if ((fl & (uint)UnitFlags.Alive) == 0 || (fl & (uint)UnitFlags.Vehicle) != 0) continue;
                    var q = w.Position[i];
                    float dx = q.x - at.x, dz = q.z - at.z;
                    if (dx * dx + dz * dz > r2) continue;
                    if (SceneHooks.IsWater != null && SceneHooks.IsWater(q.x, q.z)) continue;   // he is standing in water: it does not take
                    flames.Ignite(i, seconds);
                }
            };
            Flamethrower.Active = flames;
            if (!books.Ready) Debug.LogWarning("CombatFx: the flipbook textures (Resources/VFX) or TW/Flipbook are missing; drawing the painted stand-ins.");
            // the pieces: one renderer on this object, lent the flipbooks for the dust a landing piece or a falling wall raises
            debris = GetComponent<DebrisRenderer>();
            if (debris == null) debris = gameObject.AddComponent<DebrisRenderer>();
            debris.Host = Host;
            debris.Dust = (at, size) => { if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, at, size, 0.9f + size * 0.15f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.7f, grow: 0.9f, alpha: 0.65f, pop: 0.3f); };
            // the fallen lie as they fell: four deaths a side, in their side's cloth
            for (int k = 0; k < 8; k++)
                fallen[k] = TW.Presentation.Units.ProceduralSoldier.BuildFallen(k & 3, k < 4 ? new Color(0.47f, 0.40f, 0.24f) : new Color(0.34f, 0.38f, 0.40f));
            fallenMat = Painted(Color.white, 1.2f);
            brassMat = Painted(new Color(0.80f, 0.60f, 0.24f), 0f); brassMat.SetColor("_Emission", new Color(0.20f, 0.14f, 0.04f));
            helmetMat = Painted(new Color(0.25f, 0.28f, 0.23f), 0.9f);
            vapourMat = Transparent(unlit, new Color(0.74f, 0.80f, 0.90f, 0.13f));
            var markShader = Shader.Find("TW/GroundMark (URP)");
            if (markShader != null)
                for (int k = 0; k < markMats.Length; k++)
                {
                    markMats[k] = new Material(markShader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
                    markMats[k].SetFloat("_Shape", k); markMats[k].SetFloat("_Alpha", 0.88f);
                    // a boot print is a close-camera thing and goes within a few tens of metres; what a machine leaves is
                    // metres across, and is still on the ground at the view the game is played at
                    bool small = k == 0;
                    markMats[k].SetFloat("_FadeFrom", small ? 30f : MachineMarkReach - 26f);
                    markMats[k].SetFloat("_FadeOver", small ? 12f : 26f);
                    // and how far out the fine detail is worth computing. A boot is 34 cm long, so its cleats are gone
                    // by the time a man is twenty metres off; a rut or a pad is metres across and holds its own further.
                    // The shader narrows both again on its own once a pattern's period approaches a pixel.
                    markMats[k].SetFloat("_DetailFrom", small ? 8f : 15f);
                    markMats[k].SetFloat("_DetailOver", small ? 10f : 22f);
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
            if (SceneHooks.CookOff == (System.Action<Vector3, float>)CookOff) SceneHooks.CookOff = null;
            if (Flamethrower.Active == flames) Flamethrower.Active = null;
            flames.Clear();
            SceneHooks.FootFall = null;
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
                    float scale = 1f;
                    var cam = Camera.main;
                    if (units != null)
                    {
                        float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
                        scale = units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
                    }
                    // the round leaves the muzzle of the rifle as it is drawn this frame (the figure's baked sockets for the clip
                    // the controller chose) and goes into the chest of the man it was fired at; without sockets (a vehicle, the
                    // far tier, no controller) both ends are estimated from the stance
                    Vector3 from, barrel, to;
                    if (units == null || !units.Sockets(e.A, out from, out barrel, out _)) EstimateMuzzle(e.A, e.B, scale, out from, out barrel);
                    if (units == null || !units.Sockets(e.B, out _, out _, out to)) to = EstimateChest(e.B, scale);
                    tracers.Add(new Tracer { From = from, To = to, Born = Time.time, Team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0 });
                    Vector3 direction = (to - from).normalized;
                    bool drawn = books != null && books.Ready;
                    Vector3 carried = Host.Presenter != null && e.A >= 0 ? (Vector3)Host.Presenter.Velocity(e.A, w.Config.TickSeconds) : Vector3.zero;   // a man firing on the run carries his flash
                    carried.y = 0f;
                    if (e.Scalar < 0.5f && drawn)
                    {
                        // the flare: the root of the book's flame (the left edge of every cell) sits on the muzzle and it streams
                        // out down the barrel; half the flares are flipped across the barrel for variety (mirror and half a turn),
                        // never along it. Over-bright at night so the bloom takes it.
                        float flare = (1.05f + UnityEngine.Random.value * 0.4f) * scale;
                        float roll = FlipbookFx.ScreenRoll(cam, barrel);
                        Vector3 along = cam != null ? cam.transform.right * Mathf.Cos(roll) + cam.transform.up * Mathf.Sin(roll) : barrel;   // the barrel as the screen sees it
                        bool flip = UnityEngine.Random.value < 0.5f;
                        books.Add(FlipbookFx.Book.Muzzle, from + along * (flare * 0.44f), flare, 0.18f, flip ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: carried, roll: roll + (flip ? Mathf.PI : 0f), glow: (SceneMood.Night ? 3.2f : 1.6f) * SceneTints.Now.Glow);
                    }
                    else if (flashes.Count < 256 && e.Scalar < 0.5f) flashes.Add(new Flash { Pos = from + barrel * (0.1f * scale), Direction = barrel, Born = Time.time });
                    // a rifle leaves a little smoke at the muzzle: one small puff that drifts forward and thins out. Capped well
                    // under the chunk budget so a big firefight never starves the shell bursts of theirs.
                    // the round that misses lands somewhere: a spurt of dirt beside the man shot at, a splash and a ring if he
                    // stands in water, now and then a ricochet spark at night. A few a frame at most, whatever the firefight.
                    // near the look point first: a round landing under the eye always draws, one far off only while there is room
                    float nearHit = CameraShake.DistanceToLook(to);
                    if (e.Scalar < 0.5f && impactsThisFrame < (nearHit < 45f ? 24 : 8) && chunks.Count < 700)
                    {
                        impactsThisFrame++;
                        float angle = UnityEngine.Random.value * 6.2832f, off = UnityEngine.Random.Range(0.35f, 1.7f);
                        Vector3 hit = new Vector3(to.x + Mathf.Cos(angle) * off, 0f, to.z + Mathf.Sin(angle) * off);
                        hit.y = RenderGround.Sample(Host.Local.Map, hit.x, hit.z);
                        bool mirror = UnityEngine.Random.value < 0.5f;
                        if (SceneHooks.IsWater != null && SceneHooks.IsWater(hit.x, hit.z))
                        {
                            SceneHooks.AddRing?.Invoke(hit.x, hit.z, 0.8f);
                            Throw(hit + Vector3.up * 0.4f, 7, 4, 6f, 0.07f);
                            // a round in the water stands up a little white column
                            if (drawn) books.Add(FlipbookFx.Book.Splash, hit, 0.9f * scale, 0.55f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0), alpha: 0.9f);
                        }
                        else
                        {
                            Throw(hit, drawn ? 5 : 7, 0, 5.5f, 0.09f);
                            if (SceneMood.Night && UnityEngine.Random.value < 0.35f) Throw(hit, 3, 3, 11f, 0.035f);
                            // and in the mud a spurt of dust that leans away from the shooter
                            if (drawn) books.Add(FlipbookFx.Book.Spurt, hit, (1.3f + UnityEngine.Random.value * 0.6f) * scale, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (Vector3.Dot(direction, cam != null ? cam.transform.right : Vector3.right) < 0f ? FlipbookFx.Kind.Mirror : 0), grow: 0.3f, alpha: 0.85f, pop: 0.3f);
                        }
                    }
                    if (e.Scalar < 0.5f && chunks.Count < 420)
                        chunks.Add(new Chunk { Pos = from + barrel * (0.2f * scale), Vel = barrel * 1.4f + Vector3.up * 0.35f + carried, Born = Time.time, Life = UnityEngine.Random.Range(1.1f, 1.9f), Size = 0.16f * scale * UnityEngine.Random.Range(0.8f, 1.3f), Kind = 2 });
                    if (e.Scalar < 0.5f && chunks.Count < 600 && Near(from, 34f))
                    {
                        // up close every shot throws its case out of the breech to the right, and the muzzle keeps a thread of smoke
                        Vector3 right = Vector3.Cross(Vector3.up, barrel).normalized, breech = from - barrel * (0.75f * scale);
                        chunks.Add(new Chunk { Pos = breech + right * (0.05f * scale), Vel = right * UnityEngine.Random.Range(1.5f, 2.5f) + Vector3.up * UnityEngine.Random.Range(1.6f, 2.4f) - barrel * UnityEngine.Random.Range(0.1f, 0.6f),
                            Born = Time.time, Life = 3f, Size = 1f, Kind = 5 });
                        chunks.Add(new Chunk { Pos = from + barrel * (0.05f * scale), Vel = barrel * 0.25f + Vector3.up * 0.5f, Born = Time.time, Life = UnityEngine.Random.Range(1.8f, 2.6f), Size = 0.06f, Kind = 7 });
                    }
                    break;
                }
                case SimEventType.Hit:
                {
                    // a man struck: a spike of light where the round lands and a small cloud off his coat, at chest height for
                    // his stance. A ricochet (negative damage) is only the spike. A few a frame at most, whatever the fight.
                    if (books == null || !books.Ready || e.B < 0 || e.B >= w.HighWater) break;
                    if (hitsThisFrame >= (CameraShake.DistanceToLook(w.Position[e.B]) < 45f ? 40 : 10)) break;
                    hitsThisFrame++;
                    var cam = Camera.main;
                    float scale = 1f;
                    if (units != null)
                    {
                        float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
                        scale = units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
                    }
                    // a tank that died this tick has no flags left, and its slot may hold a man already: ask the tank view
                    bool vehicle = SceneHooks.IsTankSlot != null ? SceneHooks.IsTankSlot(e.B) : (w.Flags[e.B] & (uint)UnitFlags.Vehicle) != 0;
                    if (vehicle && SceneHooks.TanksDrawn) { hitsThisFrame--; break; }   // TankRenderer strikes the sparks where the round met the plate
                    Vector3 p;
                    if (vehicle || units == null || !units.Sockets(e.B, out _, out _, out p)) p = EstimateChest(e.B, scale);   // his chest as he is drawn
                    Vector3 toward = new Vector3(e.Dir.x, 0f, e.Dir.z); if (toward.sqrMagnitude < 0.01f) toward = Vector3.forward;
                    p -= toward.normalized * (0.18f * scale);   // on the side the round came from
                    p += new Vector3(UnityEngine.Random.Range(-0.12f, 0.12f), UnityEngine.Random.Range(-0.15f, 0.15f), UnityEngine.Random.Range(-0.12f, 0.12f)) * scale;
                    if (vehicle) books.Add(FlipbookFx.Book.Star, p, 1.5f * scale * UnityEngine.Random.Range(0.8f, 1.2f), 0.07f, roll: UnityEngine.Random.value * 6.2832f, glow: (SceneMood.Night ? 4f : 1.8f) * SceneTints.Now.Glow);
                    else books.Add(FlipbookFx.Book.Flash, p, 2.0f * scale, 0.09f, roll: UnityEngine.Random.value * 6.2832f, glow: (SceneMood.Night ? 3.2f : 1.4f) * SceneTints.Now.Glow, pop: 0.5f);
                    if (e.Scalar > 0f)
                        books.Add(FlipbookFx.Book.Puff, p, (vehicle ? 1.9f : 1.9f) * scale, 0.7f, UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                            velocity: toward.normalized * 1.3f + Vector3.up * 1.1f, grow: 1.0f, roll: UnityEngine.Random.Range(-0.5f, 0.5f), alpha: 0.85f, pop: 0.4f);
                    if (vehicle && SceneMood.Night) Throw(p, 10, 3, 10f, 0.035f);   // sparks off armour
                    else if (e.Scalar > 0f) Throw(p, 3, 0, 3.5f, 0.05f);           // and something physical comes off a man struck
                    break;
                }
                case SimEventType.Death:
                    OnDeath(e);   // CombatFx.Deaths.cs: the body, by the controller's record of the death
                    break;
                case SimEventType.UnitAlight:
                    OnAlight(e);  // the sim's BurningSystem lit or doused him
                    break;
                case SimEventType.MinePlaced:
                    OnMinePlaced(e);   // CombatFx.Mines.cs: our own marked on the ground
                    break;
                case SimEventType.MineTriggered:
                    OnMineGone(e, true);    // the fuse's flash; the burst is the next tick's Explosion
                    break;
                case SimEventType.MineCleared:
                    OnMineGone(e, false);   // a crater took it: the mark goes
                    break;
                case SimEventType.Explosion:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
                    if (LightBurst(e, p, Time.time)) break;   // a strafe's rounds, a beam's scorch: not a shell (CombatFx.Abilities.cs)
                    bool wet = SceneHooks.IsWater != null && SceneHooks.IsWater(p.x, p.z);
                    // Water damps a shell; melt does not. IsWater is map data and knows nothing about the
                    // biome, so on the lava field it is true over the river - 11% of the ground, measured -
                    // and every shell that landed there lost its burst, its smoke and its debris: the
                    // quietest impact in the game, where it should be the loudest.
                    bool melt = wet && SceneTints.Now.MoltenLiquid;
                    // Damped: the shell landed in WATER, which absorbs it. Melt does not, so every number that
                    // exists to represent that absorption has to key on this rather than on `wet`.
                    bool damp = wet && !melt;
                    bool drawn = books != null && books.Ready;
                    // Which way the shell was going. The sim puts the flight direction in Dir.xz and the shape of the
                    // thing that went off in Dir.y (0 shell, 1 falling masonry, 2 cook-off). `lean` is 0 for anything
                    // with no flight -- a cook-off, masonry, or the Kettle's mortar coming almost straight down -- and
                    // every use below multiplies by it, so a leanless burst draws exactly what it always drew.
                    Vector3 flight = new Vector3(e.Dir.x, 0f, e.Dir.z);
                    float lean = flight.magnitude;
                    if (lean > 1e-3f) flight /= lean; else { flight = Vector3.zero; lean = 0f; }
                    if (drawn)
                    {
                        // the drawn burst: its own light for an instant, the earth (or water) stood up in a column, the low
                        // burst running out either side, and the boiling cloud that rises off it and thins
                        float r = Mathf.Clamp(e.Scalar, 2f, 9f);
                        bool mirror = ((Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 1) == 0;
                        var ground = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
                        Vector4 wind = Shader.GetGlobalVector(WindGlobalId); Vector3 drift = new Vector3(wind.x, 0f, wind.y) * 3.5f + Vector3.up * 0.55f;   // _TWWind is the breeze at 0.034 per m/s (Atmosphere)
                        // up close the flash card was wider than the picture (a white-out) and the smoke filled it for seconds:
                        // both come down as the lens goes in (SceneHooks.CloseUp: 0 at the standard view, 1 among the men)
                        float closeUp = SceneHooks.CloseUp;
                        books.Add(FlipbookFx.Book.Flash, p + Vector3.up * (r * 0.3f) + flight * (r * 0.25f * lean), r * Mathf.Lerp(3.2f, 1.6f, closeUp), 0.18f, roll: UnityEngine.Random.value * 6.2832f, glow: (SceneMood.Night ? 7f : 2.5f) * SceneTints.Now.Glow * Mathf.Lerp(1f, 0.5f, closeUp), pop: 0.5f);
                        // The column, and the piece cycle 11 missed. It restored the burst, the smoke and the clods
                        // on melt and left THIS keyed on `wet`, so a shell in molten rock still threw a plume at
                        // 1.25r for 1.5 s - 60% of the size, because that is what water does to a shell. ApplyTints
                        // says twelve lines above the tint it applies that this is "the half of a splash a player
                        // actually sees", so melt was left damped in exactly the place it shows.
                        // The BOOK still keys on `wet`: melt is liquid and keeps a liquid's silhouette, and Splash
                        // is the book SplashTint was authored against - Column would hand it basalt instead.
                        // Alpha still keys on `wet` on purpose: a fully opaque plume in lava's SplashTint
                        // (1.00, 0.46, 0.12) on a field that already reins GlowScale in to 0.55 is a brightness
                        // guess, and this project has been burned twice by those.
                        books.Add(wet ? FlipbookFx.Book.Splash : FlipbookFx.Book.Column, p, r * (damp ? 1.25f : 2.1f), damp ? 1.5f : 1.8f, ground | (mirror ? FlipbookFx.Kind.Mirror : 0), grow: 0.35f, alpha: wet ? 0.85f : 1f, pop: 0.15f,
                            velocity: flight * (r * 0.45f * lean));
                        // the two wings are not a mirror pair: the second is born a little later and a little smaller
                        books.Add(FlipbookFx.Book.Wings, p, r * 2.5f, 0.95f, ground, grow: 0.4f, alpha: wet ? 0.6f : 0.9f, pop: 0.2f);
                        books.Add(FlipbookFx.Book.Wings, p + Vector3.up * 0.1f, r * 2.1f, 1.1f, ground | FlipbookFx.Kind.Mirror, grow: 0.5f, alpha: wet ? 0.5f : 0.8f, pop: 0.1f);
                        if (!wet || melt)
                        {
                            books.Add(FlipbookFx.Book.Burst, p + Vector3.up * (r * 0.55f) + flight * (r * 0.35f * lean), r * 2.6f, 1.8f, FlipbookFx.Kind.Upright | (mirror ? 0 : FlipbookFx.Kind.Mirror),
                                velocity: Vector3.up * (r * 0.5f) + drift + flight * (r * 0.5f * lean), grow: 0.5f, roll: UnityEngine.Random.Range(-0.15f, 0.15f), glow: (SceneMood.Night ? 3.4f : 1.6f) * SceneTints.Now.Glow, pop: 0.3f);
                            // what a burst leaves: dark smoke that climbs, spreads and drifts off down wind for seconds
                            int puffs = closeUp > 0.5f ? 5 : 7;
                            float shrink = Mathf.Lerp(1f, 0.7f, closeUp);
                            for (int k = 0; k < puffs; k++)
                            {
                                Vector3 off = new Vector3(UnityEngine.Random.Range(-0.5f, 0.5f), 0.3f + k * 0.18f, UnityEngine.Random.Range(-0.5f, 0.5f)) * r
                                             + flight * (r * lean * (0.25f + k * 0.12f));
                                books.Add(FlipbookFx.Book.Smoke, p + off, r * UnityEngine.Random.Range(1.1f, 1.6f) * shrink, UnityEngine.Random.Range(4f, 6.5f) * shrink, (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                    velocity: drift * UnityEngine.Random.Range(1.4f, 2.2f) + Vector3.up * 0.4f, grow: Mathf.Lerp(2.4f, 1.5f, closeUp), roll: UnityEngine.Random.Range(-0.6f, 0.6f), alpha: 0.65f, pop: 0.3f, delay: 0.5f + k * 0.15f);
                            }
                        }
                    }
                    else if (bursts.Count < 64) bursts.Add(new Burst { Pos = p, Radius = e.Scalar, Born = Time.time, Variant = (Mathf.FloorToInt(p.x * 19f) ^ Mathf.FloorToInt(p.z * 7f)) & 3 });
                    lastBlast = p; lastBlastAt = Time.time;
                    // the spatter: liquid either way, and SplashTint has already made it orange on the lava field
                    if (wet) Throw(p + Vector3.up * 0.4f, drawn ? 28 : 48, 4, 20f, 0.18f);   // a shell in the water throws a white column, not earth
                    else Throw(p, drawn ? 10 : 28, 0, 15f, 0.30f, flight * (0.8f * lean));
                    // Thrown pieces: dry earth throws clods and melt throws cooling spatter (DebrisRenderer.Biome
                    // already carries the basalt tint), but WATER throws neither. The rim and the hot crater
                    // below stay dry-only on purpose - a ring of clods lying round a hole in a running river is
                    // not a thing, on either liquid.
                    if ((!wet || melt) && debris != null && debris.Ready)
                    {
                        // the earth itself: clods the size of a fist to a head, thrown up and out, which lie where they land
                        // for half a minute; and a hail of smaller ones flung high that comes down over the next seconds
                        float r = Mathf.Clamp(e.Scalar, 2f, 9f);
                        // the fragments carry on the way the shell was travelling: the heavy clods lean with it and the
                        // light fast ones lean harder, which is what makes a burst read as having come FROM somewhere
                        debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.3f, Mathf.RoundToInt(8f + r * 2.2f), 7f + r * 0.9f, 0.16f + r * 0.02f, Mud, 30f, 0f, 1.8f, flight * (0.85f * lean), e.Tick);
                        debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.5f, Mathf.RoundToInt(4f + r), 14f + r, 0.09f, Mud, 12f, 0f, 2.4f, flight * (1.25f * lean), e.Tick + 7u);
                    }
                    if (!wet)
                    {
                        // a fresh hole: clods lie thrown round its rim, and the hot earth steams in the rain (seen from close by)
                        float rim = Mathf.Clamp(e.Scalar * 0.55f, 1.2f, 4.5f);
                        float bearing = lean > 0f ? Mathf.Atan2(flight.z, flight.x) : 0f;
                        for (int k = 0; k < 12; k++)
                        {
                            float a = (k + UnityEngine.Random.value) * 0.785f, d = rim * UnityEngine.Random.Range(0.75f, 1.5f), s = UnityEngine.Random.Range(0.13f, 0.34f);
                            d *= 1f + 0.8f * lean * Mathf.Cos(a - bearing);   // the ejecta is thrown on, not spread evenly
                            float cx = p.x + Mathf.Cos(a) * d, cz = p.z + Mathf.Sin(a) * d;
                            AddRest(Matrix4x4.TRS(new Vector3(cx, RenderGround.Sample(Host.Local.Map, cx, cz) + s * 0.25f, cz), Quaternion.Euler(a * 97f, a * 311f, a * 53f), new Vector3(s * 1.3f, s * 0.7f, s)), 140f, 2);
                        }
                        if (hotCraters.Count >= 16) hotCraters.RemoveAt(0);
                        hotCraters.Add(new Vector4(p.x, p.y, p.z, Time.time + 22f));
                    }
                    Throw(p + Vector3.up * 0.5f, 7, 2, 2.6f, 2.1f);
                    Startle(p);
                    CameraShake.Add(p, e.Scalar * 1.5f);
                    if (SceneMood.Night) Throw(p + Vector3.up * 0.3f, 26, 3, 26f, 0.055f);   // burning fragments arc out of the burst and die on the way down
                    break;
                }
                case SimEventType.AbilityFired:
                {
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.15f;
                    float radius = e.Scalar > 0f ? e.Scalar : 8f;
                    // a line ability's dir is its heading times its length and its scalar the corridor's half width
                    // (docs/02): the marker is the whole corridor, not a spot at its start
                    var corridor = new Vector3(e.Dir.x, 0f, e.Dir.z); float corridorLength = corridor.magnitude;
                    bool line = corridorLength > 1e-3f;
                    markers.Add(new Marker { Pos = p, Dir = line ? corridor / corridorLength : Vector3.zero, Length = line ? corridorLength : 0f, Radius = radius, Until = Time.time + 10f, Mine = e.B == 0 });
                    OnAbilityFired(e);   // the aircraft's run-in, the beam's charge (CombatFx.Abilities.cs)
                    string what = AbilityWord(e.A);
                    Banner(e.B == 0 ? $"Your {what} is on its way" : $"INCOMING {what.ToUpper()}: fall back or keep below the rim", 3f);
                    break;
                }
                case SimEventType.PropChanged:
                {
                    Vector3 p = (Vector3)e.Pos;
                    float foot = RenderGround.Sample(Host.Local.Map, p.x, p.z);
                    p.y = foot + 1.5f;
                    Throw(p, 8, 1, 7f, 0.16f);   // splinters where a tree broke, scrap where a wreck settled
                    if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, p, 2.2f, 0.7f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.8f, grow: 0.6f, alpha: 0.7f);
                    if (debris != null && debris.Ready) TreeBreaks(e, new Vector3(p.x, foot - 0.05f, p.z));
                    break;
                }
                case SimEventType.VehicleCrushed:
                {
                    // a man under the tracks or a claw (b = 2): what is left of him comes out from under, low and slow
                    if (e.B != 2 || debris == null || !debris.Ready || DebrisRenderer.Gore <= 0f) break;
                    Vector3 p = (Vector3)e.Pos;
                    p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.3f;
                    debris.Burst(DebrisRenderer.Piece.Helmet, p, 1, 3.5f, 0.32f * FigureScale(), Steel, 60f, 0f, 1.0f, default, e.Tick);
                    debris.Burst(DebrisRenderer.Piece.Limb, p, 1, 3f, 0.7f * FigureScale(), ClothOf(e.A) == ClothA ? ClothB : ClothA, 30f, 0f, 0.8f, default, e.Tick + 3u);   // e.A is the machine: the man under it is the other side's
                    debris.Burst(DebrisRenderer.Piece.Clod, p, Mathf.RoundToInt(4f * DebrisRenderer.Gore), 4f, 0.12f, Gore, 8f, 0f, 0.6f, default, e.Tick + 5u);
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
            using var perf = TW.Sim.PerfMarkers.FxUpdate.Auto();
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));

            // how much of every debris burst is worth throwing at this zoom (docs/21 phase 3): all of it among the men,
            // less at the standard view, little from far out
            var view = Camera.main;   // once a frame: the zoom share here, the tracers and the men's growth below read it
            float zoomNow = view != null && view.TryGetComponent<IZoomSource>(out var zoomSource) ? zoomSource.CurrentZoom : 0f;
            DebrisRenderer.ZoomShare = SceneHooks.CloseUp > 0f ? Mathf.Lerp(0.6f, 1f, SceneHooks.CloseUp) : zoomNow > 60f ? 0.3f : 0.6f;
            // tracers
            float now = Time.time;
            Prune(tracers, now - TracerSeconds, static (t, cut) => t.Born < cut);
            bool night = SceneMood.Night;
            if (tintEpoch != SceneTints.Epoch) ApplyTints();
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

            Prune(flashes, now - 0.12f, static (f, cut) => f.Born < cut);
            batch.Clear();
            for (int i = 0; i < flashes.Count; i++)
            {
                var f = flashes[i]; float s = Mathf.Lerp(0.80f, 0.22f, (now - f.Born) / 0.065f);
                batch.Add(Matrix4x4.TRS(f.Pos, f.Direction.sqrMagnitude > 0.01f ? Quaternion.LookRotation(f.Direction) : Quaternion.identity, new Vector3(s, s, s * 1.6f)));
            }
            if (batch.Count > 0) Flush(flashMesh, new RenderParams(flashMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });

            // Earth rises sharply, then collapses. The silhouette faces the view but stays vertical and rooted.
            Vector3 facing = view != null ? -view.transform.forward : Vector3.forward; facing.y = 0f;
            var splashRotation = facing.sqrMagnitude > 0.001f ? Quaternion.LookRotation(facing) : Quaternion.identity;
            Prune(bursts, now - 0.8f, static (b, cut) => b.Born < cut);
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
            flames.SimNow = SimNow;   // the torches expire by the sim's clock (CombatFx.Abilities.cs)
            flames.Update(now, view, books, drawnAt, groundAt);
            TickSmoulders(now);
            books?.Draw(now, bounds);
            hitsThisFrame = 0;

            // target markers (both sides see where support fire was called) and the aiming circle
            Prune(markers, now, static (m, at) => at > m.Until);
            for (int pass = 0; pass < 2; pass++)
            {
                var rpMark = new RenderParams(pass == 0 ? markMine : markTheirs) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off };
                // a point ability: a disc on the ground where it was called (not at sea level)
                batch.Clear();
                for (int i = 0; i < markers.Count; i++)
                {
                    var m = markers[i];
                    if (m.Mine != (pass == 0) || m.Length > 0f) continue;
                    batch.Add(Matrix4x4.TRS(new Vector3(m.Pos.x, RenderGround.Sample(Host.Local.Map, m.Pos.x, m.Pos.z) + 0.4f, m.Pos.z), Quaternion.identity, new Vector3(m.Radius * 2f, 0.05f, m.Radius * 2f)));
                }
                if (batch.Count > 0) Flush(sphere, rpMark);
                // a line ability: the whole corridor, as wide as the payload scatters
                batch.Clear();
                for (int i = 0; i < markers.Count; i++)
                {
                    var m = markers[i];
                    if (m.Mine != (pass == 0) || m.Length <= 0f) continue;
                    // in pieces, each on its own ground sample, so a long corridor follows a ridge instead of floating over it
                    var rot = Quaternion.LookRotation(m.Dir);
                    for (float s = 0f; s < m.Length; s += MarkerSegment)
                    {
                        float len = Mathf.Min(MarkerSegment, m.Length - s);
                        var mid = m.Pos + m.Dir * (s + len * 0.5f);
                        mid.y = RenderGround.Sample(Host.Local.Map, mid.x, mid.z) + 0.4f;
                        batch.Add(Matrix4x4.TRS(mid, rot, new Vector3(m.Radius * 2f, 0.05f, len)));
                    }
                }
                if (batch.Count > 0) Flush(cube, rpMark);
            }
            DrawMineMarks(bounds);         // our mines and tripwires on the ground (CombatFx.Mines.cs)
            DrawAim(bounds);               // the disc or the corridor being aimed (CombatFx.Abilities.cs)
            TickAbilities(now, bounds);    // the aircraft's run, the beam's sweep

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

            DrawSmokeScreen(gas, now, bounds);   // the smoke field as pale cards (CombatFx.Abilities.cs)

            // the fallen: a still figure in one of four deaths, lying on the slope where he fell
            float grow = 1f;
            if (units != null && view != null) grow = units.UnitScale * Mathf.Clamp((view.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f) / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
            var rpF = new RenderParams(fallenMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = true };
            // the mud takes these too (the box stand-ins, when the figures are not drawn): the same half minute, the same sinking
            float bodyLife = units != null ? units.FallenSeconds : 30f;
            if (bodyLife > 0f)
            {
                float cut = Time.time - bodyLife;
                int drop = 0;
                while (drop < bodies.Count && bodies[drop].Born <= cut) drop++;
                if (drop > 0) bodies.RemoveRange(0, drop);
            }
            for (int kind = 0; kind < 8; kind++)
            {
                batch.Clear();
                for (int i = 0; i < bodies.Count; i++)
                {
                    var b = bodies[i];
                    if ((b.Team & 1) * 4 + b.Variant != kind) continue;
                    float sunk = bodyLife > 0f ? Mathf.Clamp01((Time.time - b.Born - (bodyLife - TW.Presentation.Units.VATRenderer.SinkSeconds)) / TW.Presentation.Units.VATRenderer.SinkSeconds) * TW.Presentation.Units.VATRenderer.SinkDepth : 0f;
                    batch.Add(Matrix4x4.TRS(sunk > 0f ? b.Pos - new Vector3(0f, sunk, 0f) : b.Pos, b.Rot, new Vector3(grow, grow, grow)));
                    if (batch.Count == 1023) Flush(fallen[kind], rpF);
                }
                if (batch.Count > 0) Flush(fallen[kind], rpF);
            }
            DrawClose(now, bounds);
        }

        /// <summary>Draw a batch that is not the shared one (the marks keep a list a shape, so one sweep can fill them all).</summary>
        void Flush(Mesh mesh, List<Matrix4x4> from, RenderParams rp)
        {
            from.CopyTo(batchArray);
            FrameBudget.Draw(rp, mesh, 0, batchArray, from.Count);
            from.Clear();
        }

        void Flush(Mesh mesh, RenderParams rp)
        {
            batch.CopyTo(batchArray);
            FrameBudget.Draw(rp, mesh, 0, batchArray, batch.Count);
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
