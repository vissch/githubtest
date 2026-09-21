// Phase: B2 (shared procedural kit; no scene or map placement responsibilities)
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldKit : System.IDisposable
    {
        public sealed class Module
        {
            public Mesh Mesh; public Material Material; public bool Shadows;
        }
        public Module trunk, snag, fallen, stump, log, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin, duckboards, dugout, roof, supplies, fork, bunker, branches, looseBoards, shellCases, bush, tuft, stones;
        public readonly Module[] TrenchWalls = new Module[3], TrenchBags = new Module[3], TrenchFloors = new Module[3];
        readonly List<Module> modules = new List<Module>();
        public IReadOnlyList<Module> Modules => modules;
        readonly List<Mesh> ownedMeshes = new List<Mesh>();
        readonly List<Texture2D> ownedTextures = new List<Texture2D>();
        readonly Dictionary<BattlefieldPigment.Surface, Texture2D> pigments = new Dictionary<BattlefieldPigment.Surface, Texture2D>();
        Texture2D Pigment(BattlefieldPigment.Surface surface)
        {
            if (!pigments.TryGetValue(surface, out var texture))
            { texture = BattlefieldPigment.Bake(surface); pigments.Add(surface, texture); ownedTextures.Add(texture); }
            return texture;
        }
        static Mesh Primitive(PrimitiveType type) => Resources.GetBuiltinResource<Mesh>(type + ".fbx");

        /// <summary>A sphere of a few dozen vertices: sandbags and tree knots are drawn by the hundred.</summary>
        public Mesh Blob(int segments, int rings)
        {
            var v = new List<Vector3>(); var t = new List<int>();
            for (int r = 0; r <= rings; r++)
            for (int k = 0; k <= segments; k++)
            {
                float lat = Mathf.PI * r / rings, lon = 2f * Mathf.PI * k / segments;
                v.Add(new Vector3(Mathf.Sin(lat) * Mathf.Cos(lon), Mathf.Cos(lat), Mathf.Sin(lat) * Mathf.Sin(lon)) * 0.5f);
            }
            for (int r = 0; r < rings; r++)
            for (int k = 0; k < segments; k++)
            {
                int i = r * (segments + 1) + k, j = i + segments + 1;
                t.Add(i); t.Add(i + 1); t.Add(j); t.Add(i + 1); t.Add(j + 1); t.Add(j);
            }
            var m = new Mesh { name = "Blob", hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(v); m.SetTriangles(t, 0); m.RecalculateNormals();
            ownedMeshes.Add(m); return m;
        }

        /// <summary>A tapering six-sided trunk from y = 0 to height, leaning by lean metres at the top.</summary>
        public Mesh Taper(float r0, float r1, float height, Vector2 lean, float point = 1.6f)
        {
            const int sides = 6;
            var v = new List<Vector3>(); var t = new List<int>();
            for (int ring = 0; ring < 2; ring++)
            for (int k = 0; k <= sides; k++)
            {
                float a = 2f * Mathf.PI * k / sides, r = ring == 0 ? r0 : r1;
                v.Add(new Vector3(Mathf.Cos(a) * r + lean.x * ring, height * ring, Mathf.Sin(a) * r + lean.y * ring));
            }
            for (int k = 0; k < sides; k++)
            {
                int i = k, j = k + sides + 1;
                t.Add(i); t.Add(j); t.Add(i + 1); t.Add(i + 1); t.Add(j); t.Add(j + 1);
            }
            if (point < 1f)
            {
                int top = v.Count; v.Add(new Vector3(lean.x, height + r1 * point, lean.y));   // a sawn or worn top
                for (int k = 0; k < sides; k++) { t.Add(sides + 1 + k); t.Add(top); t.Add(sides + 2 + k); }
            }
            else
            {
                // Shell-splintered: every side ends in its own spike of a different length, around a torn hollow in the
                // middle, instead of one clean stake point.
                int hollow = v.Count; v.Add(new Vector3(lean.x, height - r1 * .5f, lean.y));
                for (int k = 0; k < sides; k++)
                {
                    float a = 2f * Mathf.PI * (k + .5f) / sides;
                    float h = r1 * point * (.35f + 1.15f * Mathf.Abs(Mathf.Sin(k * 2.399f + height * 3.1f + r0 * 17f)));
                    int tip = v.Count; v.Add(new Vector3(Mathf.Cos(a) * r1 * .72f + lean.x, height + h, Mathf.Sin(a) * r1 * .72f + lean.y));
                    int i = sides + 1 + k;
                    t.Add(i); t.Add(tip); t.Add(i + 1);             // the outer face of the spike
                    t.Add(i + 1); t.Add(tip); t.Add(hollow);        // its torn inner faces
                    t.Add(tip); t.Add(i); t.Add(hollow);
                }
            }
            var m = new Mesh { name = "Taper", hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(v); m.SetTriangles(t, 0); m.RecalculateNormals();
            ownedMeshes.Add(m); return m;
        }

        /// <summary>Joins parts into one mesh and stores a smoothed normal per vertex in UV3 for the ink outline:
        /// a hard-edged box pushed out along its face normals would come apart at the corners.</summary>
        Mesh Combine(string name, params (Mesh mesh, Vector3 pos, Vector3 euler, Vector3 scale)[] parts)
        {
            var ci = new CombineInstance[parts.Length];
            for (int i = 0; i < parts.Length; i++) ci[i] = new CombineInstance { mesh = parts[i].mesh, transform = Matrix4x4.TRS(parts[i].pos, Quaternion.Euler(parts[i].euler), parts[i].scale) };
            var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            m.CombineMeshes(ci, true, true);
            var verts = m.vertices; var normals = m.normals;
            var sum = new Dictionary<Vector3Int, Vector3>();
            var keys = new Vector3Int[verts.Length];
            for (int i = 0; i < verts.Length; i++)
            {
                keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
            }
            var smooth = new List<Vector3>(verts.Length);
            for (int i = 0; i < verts.Length; i++) smooth.Add(sum[keys[i]].normalized);
            m.SetUVs(3, smooth);
            // Baked pigment variation per part: no texture fetch, material variant or runtime work.
            var colors = new List<Color>(verts.Length);
            var paintUv = new List<Vector2>(verts.Length);
            for (int p = 0; p < parts.Length; p++)
            {
                float pigment = 0.83f + 0.21f * Rand(p, 173);
                var source = parts[p].mesh;
                var sourceColors = source.colors;
                var sourceVerts = source.vertices;
                var sourceNormals = source.normals;
                var dimensions = Vector3.Scale(source.bounds.size, parts[p].scale);
                int longAxis = Mathf.Abs(dimensions.x) >= Mathf.Abs(dimensions.y) && Mathf.Abs(dimensions.x) >= Mathf.Abs(dimensions.z) ? 0 : Mathf.Abs(dimensions.y) >= Mathf.Abs(dimensions.z) ? 1 : 2;
                for (int v = 0; v < source.vertexCount; v++)
                {
                    float height = Mathf.InverseLerp(source.bounds.min.y, source.bounds.max.y, sourceVerts[v].y);
                    float paintedForm = Mathf.Lerp(source.name == "Blob" ? 0.62f : 0.82f, 1.06f, height);
                    Color color = sourceColors.Length == source.vertexCount ? sourceColors[v] : Color.white;
                    color *= pigment * paintedForm; color.a = 1f;
                    colors.Add(color);
                    if (source.name == "Blob")
                        paintUv.Add(new Vector2(Mathf.Atan2(sourceVerts[v].z, sourceVerts[v].x) / (2f * Mathf.PI) + .5f, Mathf.Acos(Mathf.Clamp(sourceVerts[v].y * 2f, -1f, 1f)) / Mathf.PI));
                    else
                    {
                        var n = sourceNormals[v];
                        int face = Mathf.Abs(n.x) >= Mathf.Abs(n.y) && Mathf.Abs(n.x) >= Mathf.Abs(n.z) ? 0 : Mathf.Abs(n.y) >= Mathf.Abs(n.z) ? 1 : 2;
                        int u = face == longAxis ? (longAxis + 1) % 3 : longAxis, vertical = 3 - face - u;
                        paintUv.Add(new Vector2(sourceVerts[v][u] + .5f, sourceVerts[v][vertical] + .5f));
                    }
                }
            }
            m.SetColors(colors);
            m.SetUVs(0, paintUv); // Grain follows each part's longest axis before its rotation, not world coordinates.
            ownedMeshes.Add(m); return m;
        }

        Module Make(Mesh mesh, Color color, bool shadows = true, float outline = 1.5f)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            mat.SetFloat("_OutlineWidth", outline);
            var module = new Module { Mesh = mesh, Material = mat, Shadows = shadows }; modules.Add(module); return module;
        }

        /// <summary>A unit sandbag: a squared-off ellipsoid whose underside is flattened and spread, with the ends pinched
        /// where the sack is tied. About 100 vertices; long axis X.</summary>
        public Mesh Sack()
        {
            const int segments = 12, rings = 7;
            var v = new List<Vector3>(); var t = new List<int>();
            for (int r = 0; r <= rings; r++)
            for (int k = 0; k <= segments; k++)
            {
                float lat = Mathf.PI * r / rings, lon = 2f * Mathf.PI * k / segments;
                float cx = Mathf.Sin(lat) * Mathf.Cos(lon), cy = Mathf.Cos(lat), cz = Mathf.Sin(lat) * Mathf.Sin(lon);
                // squared: |c|^0.6 pushes the surface out towards a box
                float x = Mathf.Sign(cx) * Mathf.Pow(Mathf.Abs(cx), .6f), y = Mathf.Sign(cy) * Mathf.Pow(Mathf.Abs(cy), .7f), z = Mathf.Sign(cz) * Mathf.Pow(Mathf.Abs(cz), .6f);
                float pinch = 1f - .38f * Mathf.Pow(Mathf.Abs(x), 5f);   // the tied ends
                y *= pinch; z *= pinch;
                if (y < 0f) { float spread = 1f + .16f * -y; x *= spread; z *= spread; y *= .62f; }   // sag onto the course below
                else y *= 1f - .10f * (1f - Mathf.Abs(x));   // a slack top
                v.Add(new Vector3(x, y, z) * .5f);
            }
            for (int r = 0; r < rings; r++)
            for (int k = 0; k < segments; k++)
            {
                int i = r * (segments + 1) + k, j = i + segments + 1;
                t.Add(i); t.Add(i + 1); t.Add(j); t.Add(i + 1); t.Add(j + 1); t.Add(j);
            }
            var mesh = new Mesh { name = "Sack", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(v); mesh.SetTriangles(t, 0); mesh.RecalculateNormals(); ownedMeshes.Add(mesh);
            return mesh;
        }

        public Mesh WornBox(float bevel = .05f, float wear = .04f, int seed = 17)
        { var mesh = BattlefieldGeometry.WornBox(bevel, wear, seed); ownedMeshes.Add(mesh); return mesh; }

        /// <summary>Author new compound props without modifying placement or rendering. Pivots and dimensions are metres.</summary>
        public Module CreateModule(string name, BattlefieldPigment.Surface pigment, Color tint, params (Mesh mesh, Vector3 pos, Vector3 euler, Vector3 scale)[] parts)
        {
            var module = Make(Combine(name, parts), tint);
            module.Material.SetTexture("_BaseMap", Pigment(pigment)); return module;
        }

        public BattlefieldKit()
        {
            var cyl = Primitive(PrimitiveType.Cylinder); var cube = Primitive(PrimitiveType.Cube);
            var bag = Blob(12, 7);
            var bark = new Color(0.45f, 0.38f, 0.31f); var charred = new Color(0.32f, 0.28f, 0.25f);
            var timber = new Color(0.49f, 0.405f, 0.31f); var sack = new Color(0.72f, 0.655f, 0.53f);
            // the wood is dead: a standing tree is a bare, leaning trunk with a few broken limbs
            trunk = Make(Combine("DeadTree",
                (Taper(0.54f, 0.27f, 6.3f, new Vector2(0.55f, 0.2f), 1.5f), Vector3.zero, Vector3.zero, Vector3.one),   // a thick trunk, so its torn top reads
                (Taper(0.21f, 0.09f, 2.4f, new Vector2(0.2f, 0f), 2f), new Vector3(0.30f, 3.0f, 0.1f), new Vector3(0f, 0f, -55f), Vector3.one),
                (Taper(0.17f, 0.07f, 1.8f, new Vector2(0f, 0.2f), 2f), new Vector3(0.36f, 4.3f, 0.1f), new Vector3(15f, 0f, 50f), Vector3.one),
                (Taper(0.12f, 0.05f, 1.2f, Vector2.zero), new Vector3(0.46f, 5.3f, 0.15f), new Vector3(-40f, 0f, -35f), Vector3.one)), bark);
            snag = Make(Combine("Snag",
                (Taper(0.58f, 0.36f, 2.7f, new Vector2(-0.15f, 0.1f), 1.3f), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.10f, 0.03f, 1.1f, Vector2.zero), new Vector3(-0.1f, 1.7f, 0f), new Vector3(0f, 0f, 60f), Vector3.one)), charred);
            fallen = Make(Combine("FallenTop", (Taper(0.26f, 0.10f, 4.2f, Vector2.zero), new Vector3(0.6f, 0.22f, 0.3f), new Vector3(86f, 18f, 0f), Vector3.one)), bark);
            stump = Make(Combine("Stump", (Taper(0.42f, 0.32f, 0.55f, Vector2.zero, 0.25f), Vector3.zero, Vector3.zero, Vector3.one)), charred);
            log = Make(Combine("Log", (cyl, new Vector3(0f, 0.26f, 0f), new Vector3(0f, 0f, 90f), new Vector3(0.48f, 2.0f, 0.48f))), bark);
            wreck = Make(Combine("Wreck",
                (cube, new Vector3(0f, 1.0f, 0f), new Vector3(0f, 0f, 7f), new Vector3(2.0f, 1.4f, 7.4f)),
                (cube, new Vector3(-1.45f, 0.85f, 0f), new Vector3(0f, 0f, 7f), new Vector3(0.9f, 2.0f, 7.9f)),
                (cube, new Vector3(1.45f, 0.75f, 0.3f), new Vector3(4f, 3f, 7f), new Vector3(0.9f, 1.8f, 7.6f)),
                (cube, new Vector3(0.3f, 1.95f, 1.4f), new Vector3(0f, 25f, 12f), new Vector3(1.4f, 0.4f, 1.6f))), new Color(0.27f, 0.25f, 0.21f));
            bridge = Make(Combine("Bridge",
                (cube, new Vector3(0f, 0.12f, 0f), Vector3.zero, new Vector3(5.2f, 0.18f, 24f)),
                (cube, new Vector3(-2.5f, 0.6f, 0f), Vector3.zero, new Vector3(0.14f, 0.9f, 24f)),
                (cube, new Vector3(2.5f, 0.6f, 0f), Vector3.zero, new Vector3(0.14f, 0.9f, 24f))), timber);
            // knife rest: two timber crosses on a spar, wire strung between them
            knifeRest = Make(Combine("KnifeRest",
                (cube, new Vector3(-0.85f, 0.55f, 0f), new Vector3(32f, 0f, 0f), new Vector3(0.11f, 1.5f, 0.11f)),
                (cube, new Vector3(-0.85f, 0.55f, 0f), new Vector3(-32f, 0f, 0f), new Vector3(0.11f, 1.5f, 0.11f)),
                (cube, new Vector3(0.85f, 0.55f, 0f), new Vector3(32f, 0f, 0f), new Vector3(0.11f, 1.5f, 0.11f)),
                (cube, new Vector3(0.85f, 0.55f, 0f), new Vector3(-32f, 0f, 0f), new Vector3(0.11f, 1.5f, 0.11f)),
                (cube, new Vector3(0f, 0.62f, 0f), Vector3.zero, new Vector3(2.0f, 0.09f, 0.09f))), new Color(0.36f, 0.24f, 0.17f), false);
            wire = Make(Combine("Wire",
                (cube, new Vector3(0f, 1.05f, 0.30f), new Vector3(0f, 0f, 3f), new Vector3(2.1f, 0.025f, 0.025f)),
                (cube, new Vector3(0f, 1.05f, -0.30f), new Vector3(0f, 0f, -3f), new Vector3(2.1f, 0.025f, 0.025f)),
                (cube, new Vector3(0f, 0.22f, 0.36f), new Vector3(0f, 4f, -2f), new Vector3(2.1f, 0.025f, 0.025f)),
                (cube, new Vector3(0f, 0.22f, -0.36f), new Vector3(0f, -4f, 2f), new Vector3(2.1f, 0.025f, 0.025f))), new Color(0.20f, 0.18f, 0.17f), false, 0.8f);
            // two courses of fat bags, the upper one staggered
            // Burlap sacks, not pebbles: squarish, sagging onto what is under them, pinched at the tied ends. The lower
            // course is squashed wider by the weight above; the upper course is laid half a bag along (running bond), so
            // one of its bags spans the joint into the next 2 m module.
            var sackMesh = Sack();
            sandbags = Make(Combine("Sandbags",
                (sackMesh, new Vector3(-0.5f, 0.15f, 0f), new Vector3(0f, 4f, 0f), new Vector3(1.04f, 0.34f, 0.74f)),
                (sackMesh, new Vector3(0.5f, 0.15f, 0.02f), new Vector3(0f, -5f, 0f), new Vector3(1.04f, 0.34f, 0.74f)),
                (sackMesh, new Vector3(0f, 0.45f, -0.03f), new Vector3(0f, 3f, 2f), new Vector3(0.98f, 0.38f, 0.62f)),
                (sackMesh, new Vector3(1.0f, 0.45f, -0.01f), new Vector3(0f, -4f, -2f), new Vector3(0.98f, 0.38f, 0.62f))), sack);
            planks = Make(Combine("Revetment",
                (cube, new Vector3(0f, 0.30f, 0f), Vector3.zero, new Vector3(2.0f, 0.50f, 0.07f)),
                (cube, new Vector3(0f, 0.88f, 0f), Vector3.zero, new Vector3(2.0f, 0.50f, 0.07f)),
                (cube, new Vector3(0f, 1.46f, 0f), Vector3.zero, new Vector3(2.0f, 0.50f, 0.07f)),
                (cube, new Vector3(-0.95f, 0.95f, -0.09f), Vector3.zero, new Vector3(0.16f, 2.0f, 0.16f))), timber, false, 2.0f);
            ladder = Make(Combine("Ladder",
                (cube, new Vector3(-0.28f, 1.0f, 0f), new Vector3(-16f, 0f, 0f), new Vector3(0.09f, 2.3f, 0.09f)),
                (cube, new Vector3(0.28f, 1.0f, 0f), new Vector3(-16f, 0f, 0f), new Vector3(0.09f, 2.3f, 0.09f)),
                (cube, new Vector3(0f, 0.5f, -0.14f), Vector3.zero, new Vector3(0.56f, 0.07f, 0.07f)),
                (cube, new Vector3(0f, 1.0f, 0f), Vector3.zero, new Vector3(0.56f, 0.07f, 0.07f)),
                (cube, new Vector3(0f, 1.5f, 0.14f), Vector3.zero, new Vector3(0.56f, 0.07f, 0.07f))), new Color(0.55f, 0.43f, 0.28f), false, 2.0f);
            // what is left of a building: broken columns under a lintel, a fallen block
            ruin = Make(Combine("Ruin",
                (cube, new Vector3(-3f, 3.0f, 0f), Vector3.zero, new Vector3(1.1f, 6.0f, 1.1f)),
                (cube, new Vector3(0f, 3.0f, 0f), Vector3.zero, new Vector3(1.1f, 6.0f, 1.1f)),
                (cube, new Vector3(3f, 2.1f, 0f), new Vector3(0f, 0f, 4f), new Vector3(1.1f, 4.2f, 1.1f)),
                (cube, new Vector3(-1.4f, 6.4f, 0f), new Vector3(0f, 0f, -3f), new Vector3(5.0f, 0.9f, 1.4f)),
                (cube, new Vector3(4.6f, 0.5f, 1.2f), new Vector3(0f, 30f, 12f), new Vector3(1.8f, 1.0f, 1.2f)),
                (cube, new Vector3(-6.5f, 1.6f, -0.5f), new Vector3(0f, 8f, 0f), new Vector3(4.0f, 3.2f, 0.9f))), new Color(0.50f, 0.49f, 0.47f));
            BuildFieldKit(cube, bag, timber);
            var woodPaint = Pigment(BattlefieldPigment.Surface.Timber);
            var earthPaint = Pigment(BattlefieldPigment.Surface.Earth);
            var concretePaint = Pigment(BattlefieldPigment.Surface.Concrete);
            var canvasPaint = Pigment(BattlefieldPigment.Surface.Canvas);
            var barkPaint = Pigment(BattlefieldPigment.Surface.Bark);
            foreach (var b in new[] { planks, ladder, duckboards, dugout, supplies, knifeRest, bridge }) b.Material.SetTexture("_BaseMap", woodPaint);
            roof.Material.SetTexture("_BaseMap", earthPaint);
            bunker.Material.SetTexture("_BaseMap", concretePaint);
            sandbags.Material.SetTexture("_BaseMap", canvasPaint);
            BuildTrenchVariants(cube, sackMesh, timber, sack, woodPaint, canvasPaint);
            foreach (var b in new[] { trunk, snag, fallen, stump, fork, log }) b.Material.SetTexture("_BaseMap", barkPaint);
        }

        void BuildTrenchVariants(Mesh cube, Mesh sackMesh, Color timber, Color sack, Texture2D woodPaint, Texture2D canvasPaint)
        {
            var worn = WornBox(.035f, .10f, 217);
            for (int variant = 0; variant < 3; variant++)
            {
                var parts = new List<(Mesh, Vector3, Vector3, Vector3)>();
                for (int row = 0; row < 3; row++)
                    parts.Add((row == 2 ? worn : cube, new Vector3((Rand(row, variant + 302) - .5f) * .10f, .30f + row * .58f, .025f * row),
                        new Vector3(0f, 0f, (Rand(row, variant + 303) - .5f) * 4f), new Vector3(2.14f, .46f + Rand(row, variant + 304) * .07f, .10f)));
                parts.Add((cube, new Vector3(-.91f, .96f, -.10f), new Vector3(0f, 0f, variant == 1 ? 4f : -2f), new Vector3(.18f, 2f, .20f)));
                if (variant == 2) parts.Add((cube, new Vector3(.28f, .90f, -.13f), new Vector3(0f, 0f, -24f), new Vector3(.15f, 1.6f, .16f)));
                TrenchWalls[variant] = Make(Combine("Weathered revetment " + variant, parts.ToArray()), timber * (variant == 2 ? .9f : 1f), false, 1.6f);
                TrenchWalls[variant].Material.SetTexture("_BaseMap", woodPaint);
                parts.Clear();
                for (int bag = 0; bag < 2; bag++)
                    parts.Add((sackMesh, new Vector3((bag - .5f) * 1.02f, .15f, (Rand(bag, variant + 315) - .5f) * .10f),
                        new Vector3(0f, (Rand(bag, variant + 316) - .5f) * 15f, (Rand(bag, variant + 317) - .5f) * 5f), new Vector3(1.06f, .33f, .73f)));
                if (variant != 2) parts.Add((sackMesh, new Vector3(variant == 0 ? -.04f : .30f, .44f, -.08f), new Vector3(0f, -8f, 3f), new Vector3(1.02f, .37f, .65f)));
                if (variant == 0) parts.Add((sackMesh, new Vector3(1.0f, .43f, -.05f), new Vector3(0f, 7f, -3f), new Vector3(.95f, .35f, .63f)));
                TrenchBags[variant] = Make(Combine("Settled parapet " + variant, parts.ToArray()), sack);
                TrenchBags[variant].Material.SetTexture("_BaseMap", canvasPaint);
                parts.Clear();
                for (int board = 0; board < 5; board++)
                    parts.Add((cube, new Vector3((board - 2) * .39f, .08f + Rand(board, variant + 321) * .025f, (Rand(board, variant + 322) - .5f) * .14f),
                        new Vector3(0f, (Rand(board, variant + 323) - .5f) * 7f, 0f), new Vector3(.32f + Rand(board, variant + 324) * .045f, .10f, 1.43f + Rand(board, variant + 325) * .30f)));
                for (int rail = -1; rail <= 1; rail += 2) parts.Add((cube, new Vector3(0f, .025f, rail * .55f), Vector3.zero, new Vector3(2f, .10f, .14f)));
                TrenchFloors[variant] = Make(Combine("Uneven duckboards " + variant, parts.ToArray()), timber, false, .8f);
                TrenchFloors[variant].Material.SetTexture("_BaseMap", woodPaint);
            }
        }

        // Every module has a ground pivot and metre dimensions; placement reads the map, never modifies it.
        void BuildFieldKit(Mesh cube, Mesh bag, Color timber)
        {
            var boards = new List<(Mesh, Vector3, Vector3, Vector3)>();
            for (int i = 0; i < 5; i++) boards.Add((cube, new Vector3((i - 2) * 0.39f, 0.07f + Rand(i, 82) * 0.025f, 0f), new Vector3(0f, Rand(i, 83) * 5f - 2.5f, 0f), new Vector3(0.34f, 0.10f, 1.55f + Rand(i, 84) * 0.18f)));
            boards.Add((cube, new Vector3(0f, 0.01f, -0.58f), Vector3.zero, new Vector3(2f, 0.08f, 0.12f)));
            boards.Add((cube, new Vector3(0f, 0.01f, 0.58f), Vector3.zero, new Vector3(2f, 0.08f, 0.12f)));
            duckboards = Make(Combine("Duckboard panel 2m", boards.ToArray()), timber, false, 0.8f);
            var wood = BattlefieldGeometry.WornBox(.045f, .035f, 17); ownedMeshes.Add(wood);
            var stone = BattlefieldGeometry.WornBox(.09f, .14f, 31); ownedMeshes.Add(stone);
            var shelter = new List<(Mesh, Vector3, Vector3, Vector3)>();
            // Recessed, dark interior: a roofed supply recess, kept outside traversable trench and links.
            shelter.Add((wood, new Vector3(0f, 0.85f, 0.9f), Vector3.zero, new Vector3(3.4f, 1.7f, 0.22f)));
            for (int i = 0; i < 4; i++)
            {
                shelter.Add((wood, new Vector3(-1.65f, 0.24f + i * 0.43f, 0f), Vector3.zero, new Vector3(0.22f, 0.38f, 2.2f)));
                shelter.Add((wood, new Vector3(1.65f, 0.24f + i * 0.43f, 0f), Vector3.zero, new Vector3(0.22f, 0.38f, 2.2f)));
            }
            shelter.Add((wood, new Vector3(-1.43f, 0.95f, -1.08f), new Vector3(0f, 0f, -3f), new Vector3(0.23f, 2f, 0.26f)));
            shelter.Add((wood, new Vector3(1.43f, 0.95f, -1.08f), new Vector3(0f, 0f, 2f), new Vector3(0.23f, 2f, 0.26f)));
            shelter.Add((wood, new Vector3(0f, 1.94f, -1.12f), new Vector3(0f, 0f, -2f), new Vector3(3.7f, 0.32f, 0.36f)));
            for (int i = 0; i < 8; i++) shelter.Add((wood, new Vector3((i - 3.5f) * 0.48f, 1.92f, 0f), new Vector3(0f, 0f, Rand(i, 90) * 3f), new Vector3(0.45f, 0.18f, 2.8f)));
            dugout = Make(Combine("Timber supply dugout", shelter.ToArray()), timber, true, 1.25f);
            var innerColors = dugout.Mesh.colors;
            for (int v = 0; v < wood.vertexCount; v++) innerColors[v] = new Color(0.24f, 0.27f, 0.29f, 1f); // painted dark back of the recess
            dugout.Mesh.colors = innerColors;
            bunker = Make(Combine("Ruined concrete shelter",
                (stone, new Vector3(0f, 0.9f, 0.9f), Vector3.zero, new Vector3(3.4f, 1.8f, 0.3f)),
                (stone, new Vector3(-1.45f, 0.9f, -0.1f), new Vector3(0f, 0f, -3f), new Vector3(0.65f, 1.9f, 2.5f)),
                (stone, new Vector3(1.45f, 0.9f, -0.1f), new Vector3(0f, 0f, 2f), new Vector3(0.65f, 1.9f, 2.5f)),
                (stone, new Vector3(0f, 1.87f, -0.1f), new Vector3(0f, 0f, -2f), new Vector3(3.7f, 0.44f, 2.7f)),
                (stone, new Vector3(-1.2f, 2.22f, 0.32f), new Vector3(0f, 7f, 6f), new Vector3(0.7f, 0.65f, 0.9f)),
                (stone, new Vector3(-0.6f, 2.1f, 0.35f), new Vector3(0f, -5f, -9f), new Vector3(0.55f, 0.4f, 0.8f)),
                (stone, new Vector3(1.38f, 2.3f, 0.23f), new Vector3(0f, 0f, -7f), new Vector3(0.5f, 0.65f, 0.85f)),
                (stone, new Vector3(2.2f, 0.2f, -0.8f), new Vector3(8f, 28f, 12f), new Vector3(0.8f, 0.55f, 0.65f))), new Color(0.49f, 0.51f, 0.50f), true, 1.6f);
            var bunkerColors = bunker.Mesh.colors;
            for (int v = 0; v < stone.vertexCount; v++) bunkerColors[v] = new Color(0.22f, 0.25f, 0.27f, 1f);
            bunker.Mesh.colors = bunkerColors;
            roof = Make(ShelterMound(), new Color(.60f, .54f, .455f), true, 0f);
            roof.Material.SetShaderPassEnabled("SRPDefaultUnlit", false);
            supplies = Make(Combine("Braced ammunition crate",
                (cube, new Vector3(0f, 0.36f, 0f), Vector3.zero, new Vector3(1.05f, 0.68f, 0.68f)),
                (cube, new Vector3(-0.37f, 0.37f, -0.365f), Vector3.zero, new Vector3(0.10f, 0.75f, 0.06f)),
                (cube, new Vector3(0.37f, 0.37f, -0.365f), Vector3.zero, new Vector3(0.10f, 0.75f, 0.06f)),
                (cube, new Vector3(0f, 0.36f, -0.39f), new Vector3(0f, 0f, 28f), new Vector3(1.06f, 0.10f, 0.065f)),
                (cube, new Vector3(-0.37f, 0.735f, 0f), Vector3.zero, new Vector3(0.10f, 0.06f, 0.78f)),
                (cube, new Vector3(0.37f, 0.735f, 0f), Vector3.zero, new Vector3(0.10f, 0.06f, 0.78f))), new Color(0.43f, 0.435f, 0.31f), true, 1f);
            // The small and medium shapes that gather round the big ones (BattlefieldComposer.Clumps): dead scrub, dry
            // grass, stones. No shadows and a thin line: there are hundreds of them.
            bush = Make(Combine("Dead scrub",
                (Taper(0.05f, 0.012f, 1.25f, new Vector2(0.10f, 0.05f), 2f), Vector3.zero, new Vector3(8f, 0f, 6f), Vector3.one),
                (Taper(0.04f, 0.010f, 1.05f, Vector2.zero, 2f), new Vector3(0.05f, 0f, 0f), new Vector3(28f, 40f, -22f), Vector3.one),
                (Taper(0.04f, 0.010f, 0.95f, Vector2.zero, 2f), new Vector3(-0.04f, 0f, 0.03f), new Vector3(-30f, 110f, 18f), Vector3.one),
                (Taper(0.035f, 0.010f, 0.85f, Vector2.zero, 2f), new Vector3(0f, 0f, -0.05f), new Vector3(24f, 200f, 30f), Vector3.one),
                (Taper(0.03f, 0.008f, 0.6f, Vector2.zero, 2f), new Vector3(0.12f, 0.55f, 0.04f), new Vector3(48f, 70f, -40f), Vector3.one),
                (Taper(0.03f, 0.008f, 0.55f, Vector2.zero, 2f), new Vector3(-0.10f, 0.5f, 0.02f), new Vector3(-44f, 250f, 36f), Vector3.one)), new Color(0.40f, 0.34f, 0.25f), false, 0.8f);
            tuft = Make(Combine("Dry grass",
                (Taper(0.035f, 0.004f, 0.42f, new Vector2(0.06f, 0f), 1f), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.03f, 0.004f, 0.36f, new Vector2(-0.07f, 0.04f), 1f), new Vector3(0.06f, 0f, 0.03f), Vector3.zero, Vector3.one),
                (Taper(0.03f, 0.004f, 0.48f, new Vector2(0.02f, -0.08f), 1f), new Vector3(-0.05f, 0f, 0.04f), Vector3.zero, Vector3.one),
                (Taper(0.03f, 0.004f, 0.30f, new Vector2(0.09f, 0.07f), 1f), new Vector3(0.01f, 0f, -0.07f), Vector3.zero, Vector3.one),
                (Taper(0.025f, 0.004f, 0.38f, new Vector2(-0.05f, -0.06f), 1f), new Vector3(-0.07f, 0f, -0.04f), Vector3.zero, Vector3.one)), new Color(0.62f, 0.58f, 0.34f), false, 0.35f);
            stones = Make(Combine("Stones",
                (WornBox(.10f, .08f, 3), new Vector3(0f, 0.09f, 0f), new Vector3(6f, 20f, -5f), new Vector3(0.42f, 0.24f, 0.34f)),
                (WornBox(.10f, .08f, 4), new Vector3(0.32f, 0.05f, 0.12f), new Vector3(-8f, 65f, 4f), new Vector3(0.22f, 0.13f, 0.19f)),
                (WornBox(.10f, .08f, 8), new Vector3(-0.12f, 0.04f, 0.28f), new Vector3(3f, 130f, 9f), new Vector3(0.15f, 0.10f, 0.13f))), new Color(0.43f, 0.41f, 0.38f), false, 1.0f);
            // loose battlefield litter, scattered by BattlefieldComposer.Debris
            branches = Make(Combine("Broken branches",
                (Taper(0.09f, 0.025f, 2.3f, new Vector2(0.15f, 0f)), new Vector3(0f, 0.08f, 0f), new Vector3(88f, 0f, 0f), Vector3.one),
                (Taper(0.05f, 0.015f, 1.1f, Vector2.zero), new Vector3(0.05f, 0.10f, 1.0f), new Vector3(80f, 55f, 0f), Vector3.one),
                (Taper(0.07f, 0.02f, 1.5f, Vector2.zero), new Vector3(0.7f, 0.07f, -0.3f), new Vector3(86f, -70f, 0f), Vector3.one)), new Color(0.34f, 0.30f, 0.26f), false);
            looseBoards = Make(Combine("Loose boards",
                (WornBox(.02f, .02f, 5), new Vector3(0f, 0.05f, 0f), new Vector3(3f, 0f, 2f), new Vector3(0.22f, 0.05f, 1.9f)),
                (WornBox(.02f, .02f, 6), new Vector3(0.35f, 0.09f, 0.2f), new Vector3(-4f, 38f, 3f), new Vector3(0.20f, 0.05f, 1.3f))), new Color(0.49f, 0.405f, 0.31f), false);
            shellCases = Make(Combine("Spent shell cases",
                (Taper(0.065f, 0.055f, 0.5f, Vector2.zero, 0.1f), new Vector3(0f, 0.07f, 0f), new Vector3(90f, 20f, 0f), Vector3.one),
                (Taper(0.065f, 0.055f, 0.5f, Vector2.zero, 0.1f), new Vector3(0.3f, 0.07f, 0.25f), new Vector3(90f, -50f, 0f), Vector3.one),
                (Taper(0.065f, 0.055f, 0.5f, Vector2.zero, 0.1f), new Vector3(-0.2f, 0.07f, 0.4f), new Vector3(90f, 85f, 0f), Vector3.one)), new Color(0.62f, 0.50f, 0.24f), false);
            fork = Make(Combine("Forked shell tree",
                (Taper(0.58f, 0.17f, 5.5f, new Vector2(-0.5f, 0.12f)), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.20f, 0.04f, 3.2f, new Vector2(0.6f, 0f)), new Vector3(-0.22f, 2.2f, 0f), new Vector3(12f, 0f, -32f), Vector3.one),
                (Taper(0.12f, 0.015f, 1.4f, Vector2.zero), new Vector3(-0.35f, 3.8f, 0f), new Vector3(-10f, 0f, 58f), Vector3.one),
                (Taper(0.30f, 0.02f, 1.5f, new Vector2(0.1f, 0f)), new Vector3(0f, 0.12f, 0f), new Vector3(65f, 25f, 10f), Vector3.one)), new Color(0.32f, 0.28f, 0.24f));
        }

        Mesh ShelterMound()
        {
            // Cut-away front exposes the entrance; the sides and rear descend to the ground.
            const int w = 16, d = 12;
            var vertices = new Vector3[(w + 1) * (d + 1)];
            var uv = new Vector2[vertices.Length]; var colors = new Color[vertices.Length];
            var triangles = new List<int>();
            for (int z = 0; z <= d; z++) for (int x = 0; x <= w; x++)
            {
                int i = z * (w + 1) + x;
                float px = (x / (float)w - .5f) * 6.6f, pz = -1.12f + z / (float)d * 4.5f;
                float side = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(1.65f, 3.3f, Mathf.Abs(px)));
                float back = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(.95f, 3.38f, pz));
                float height = (3.1f + Mathf.PerlinNoise(px * 1.4f + 8f, pz * 1.4f) * .28f) * side * back - .75f;
                vertices[i] = new Vector3(px, height, pz);
                uv[i] = new Vector2(x / 8f, z / 6f);
                float shade = .73f + .27f * side * back;
                colors[i] = new Color(shade, shade, shade, 1f);
                if (x == w || z == d) continue;
                triangles.Add(i); triangles.Add(i + w + 1); triangles.Add(i + 1);
                triangles.Add(i + 1); triangles.Add(i + w + 1); triangles.Add(i + w + 2);
            }
            var mesh = new Mesh { name = "Buried shelter mound", vertices = vertices, uv = uv, colors = colors, triangles = triangles.ToArray() };
            mesh.RecalculateNormals(); mesh.RecalculateBounds(); ownedMeshes.Add(mesh); return mesh;
        }

        static float Rand(int i, int salt)
        {
            uint h = (uint)i * 0x9E3779B1u ^ (uint)salt * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        public void Dispose()
        {
            foreach (var mesh in ownedMeshes) if (mesh != null) Object.Destroy(mesh);
            foreach (var texture in ownedTextures) if (texture != null) Object.Destroy(texture);
            foreach (var module in Modules) if (module.Material != null) Object.Destroy(module.Material);
        }
    }
}
