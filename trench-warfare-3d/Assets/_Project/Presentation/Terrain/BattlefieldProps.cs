// Phase: B2 (implemented with code-made placeholder meshes; C4 art replaces the meshes, not the placement)
// Draws what stands on the battlefield, instanced: MapData.Props (trees, broken trees, stumps, logs, wrecks, the
// bridge), wire on Wire cells, and the trench kit along TrenchCells (sandbag parapets on both lips, plank revetment
// on both walls, a ladder where there is a link), and dead trees and ruins on the horizon. Everything is TW/Toon with
// an ink outline. Instance lists are rebuilt when the sim says something changed
// (PropChanged, WireBreached), never per frame.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Presentation;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldProps : MonoBehaviour
    {
        public SimHost Host;

        sealed class Batch
        {
            public Mesh Mesh; public Material Material; public bool Shadows;
            public readonly List<Matrix4x4[]> Pages = new List<Matrix4x4[]>(); public readonly List<int> Counts = new List<int>();
            public readonly List<Bounds> PageBounds = new List<Bounds>();
            readonly Dictionary<Vector2Int, int> spatialPages = new Dictionary<Vector2Int, int>();
            public void Clear() { Counts.Clear(); PageBounds.Clear(); spatialPages.Clear(); }
            public void Add(Matrix4x4 m)
            {
                var key = new Vector2Int(Mathf.FloorToInt(m.m03 / 32f), Mathf.FloorToInt(m.m23 / 32f));
                if (!spatialPages.TryGetValue(key, out int page) || Counts[page] == 1023)
                {
                    page = Counts.Count; Counts.Add(0); PageBounds.Add(default);
                    spatialPages[key] = page;
                    if (Pages.Count <= page) Pages.Add(new Matrix4x4[1023]);
                }
                Vector3 e = Mesh.bounds.extents;
                Vector3 a = m.MultiplyVector(new Vector3(e.x, 0f, 0f)), b = m.MultiplyVector(new Vector3(0f, e.y, 0f)), c = m.MultiplyVector(new Vector3(0f, 0f, e.z));
                var instanceBounds = new Bounds(m.MultiplyPoint3x4(Mesh.bounds.center), new Vector3(Mathf.Abs(a.x) + Mathf.Abs(b.x) + Mathf.Abs(c.x), Mathf.Abs(a.y) + Mathf.Abs(b.y) + Mathf.Abs(c.y), Mathf.Abs(a.z) + Mathf.Abs(b.z) + Mathf.Abs(c.z)) * 2f);
                var merged = Counts[page] == 0 ? instanceBounds : PageBounds[page];
                merged.Encapsulate(instanceBounds); PageBounds[page] = merged;
                Pages[page][Counts[page]] = m; Counts[page]++;
            }
        }

        Batch trunk, snag, fallen, stump, log, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin;
        Batch duckboards, earth, dugout, roof, supplies, fork, bunker;
        Batch[] all;
        readonly List<Mesh> ownedMeshes = new List<Mesh>();
        readonly Plane[] planes = new Plane[6];
        public int VisibleInstances { get; private set; }
        public int SubmittedVertices { get; private set; }
        public int DrawCalls { get; private set; }
        bool dirty = true, subscribed;
        Bounds bounds;

        static Mesh Primitive(PrimitiveType type) => Resources.GetBuiltinResource<Mesh>(type + ".fbx");

        /// <summary>A sphere of a few dozen vertices: sandbags and tree knots are drawn by the hundred.</summary>
        Mesh Blob(int segments, int rings)
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
        Mesh Taper(float r0, float r1, float height, Vector2 lean, float point = 1.6f)
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
            int top = v.Count; v.Add(new Vector3(lean.x, height + r1 * point, lean.y));   // a splintered point
            for (int k = 0; k < sides; k++) { t.Add(sides + 1 + k); t.Add(top); t.Add(sides + 2 + k); }
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
            for (int p = 0; p < parts.Length; p++)
            {
                float pigment = 0.83f + 0.21f * Rand(p, 173);
                var source = parts[p].mesh;
                var sourceColors = source.colors;
                var sourceVerts = source.vertices;
                for (int v = 0; v < source.vertexCount; v++)
                {
                    float height = Mathf.InverseLerp(source.bounds.min.y, source.bounds.max.y, sourceVerts[v].y);
                    float paintedForm = Mathf.Lerp(source.name == "Blob" ? 0.62f : 0.82f, 1.06f, height);
                    Color color = sourceColors.Length == source.vertexCount ? sourceColors[v] : Color.white;
                    color *= pigment * paintedForm; color.a = 1f;
                    colors.Add(color);
                }
            }
            m.SetColors(colors);
            ownedMeshes.Add(m); return m;
        }

        static Batch Make(Mesh mesh, Color color, bool shadows = true, float outline = 1.5f)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            mat.SetFloat("_OutlineWidth", outline);
            return new Batch { Mesh = mesh, Material = mat, Shadows = shadows };
        }

        void Start()
        {
            var cyl = Primitive(PrimitiveType.Cylinder); var cube = Primitive(PrimitiveType.Cube);
            var bag = Blob(12, 7);
            var bark = new Color(0.34f, 0.30f, 0.26f); var charred = new Color(0.24f, 0.225f, 0.21f);
            var timber = new Color(0.49f, 0.405f, 0.31f); var sack = new Color(0.72f, 0.655f, 0.53f);
            // the wood is dead: a standing tree is a bare, leaning trunk with a few broken limbs
            trunk = Make(Combine("DeadTree",
                (Taper(0.34f, 0.12f, 7.2f, new Vector2(0.5f, 0.2f)), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.12f, 0.04f, 2.2f, new Vector2(0.2f, 0f)), new Vector3(0.22f, 3.4f, 0.1f), new Vector3(0f, 0f, -55f), Vector3.one),
                (Taper(0.10f, 0.03f, 1.7f, new Vector2(0f, 0.2f)), new Vector3(0.30f, 4.8f, 0.1f), new Vector3(15f, 0f, 50f), Vector3.one),
                (Taper(0.08f, 0.03f, 1.2f, Vector2.zero), new Vector3(0.40f, 5.9f, 0.15f), new Vector3(-40f, 0f, -35f), Vector3.one)), bark);
            snag = Make(Combine("Snag",
                (Taper(0.36f, 0.20f, 2.9f, new Vector2(-0.15f, 0.1f)), Vector3.zero, Vector3.zero, Vector3.one),
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
            sandbags = Make(Combine("Sandbags",
                (bag, new Vector3(-0.5f, 0.17f, 0f), new Vector3(0f, 6f, 0f), new Vector3(1.05f, 0.42f, 0.66f)),
                (bag, new Vector3(0.5f, 0.17f, 0.03f), new Vector3(0f, -7f, 0f), new Vector3(1.05f, 0.42f, 0.66f)),
                (bag, new Vector3(0f, 0.47f, -0.02f), new Vector3(0f, 3f, 0f), new Vector3(1.05f, 0.40f, 0.62f))), sack);
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
            all = new[] { trunk, snag, fallen, stump, log, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin, duckboards, earth, dugout, roof, supplies, fork, bunker };
        }

        // Every module has a ground pivot and metre dimensions; placement reads the map, never modifies it.
        void BuildFieldKit(Mesh cube, Mesh bag, Color timber)
        {
            var boards = new List<(Mesh, Vector3, Vector3, Vector3)>();
            for (int i = 0; i < 5; i++) boards.Add((cube, new Vector3((i - 2) * 0.39f, 0.07f + Rand(i, 82) * 0.025f, 0f), new Vector3(0f, Rand(i, 83) * 5f - 2.5f, 0f), new Vector3(0.34f, 0.10f, 1.55f + Rand(i, 84) * 0.18f)));
            boards.Add((cube, new Vector3(0f, 0.01f, -0.58f), Vector3.zero, new Vector3(2f, 0.08f, 0.12f)));
            boards.Add((cube, new Vector3(0f, 0.01f, 0.58f), Vector3.zero, new Vector3(2f, 0.08f, 0.12f)));
            duckboards = Make(Combine("Duckboard panel 2m", boards.ToArray()), timber, false, 0.8f);
            var berm = Berm();
            earth = Make(berm, new Color(0.51f, 0.475f, 0.42f), false, 0f);
            earth.Material.SetShaderPassEnabled("SRPDefaultUnlit", false);
            var shelter = new List<(Mesh, Vector3, Vector3, Vector3)>();
            // Recessed, dark interior: a roofed supply recess, kept outside traversable trench and links.
            shelter.Add((cube, new Vector3(0f, 0.85f, 0.9f), Vector3.zero, new Vector3(3.4f, 1.7f, 0.22f)));
            for (int i = 0; i < 4; i++)
            {
                shelter.Add((cube, new Vector3(-1.65f, 0.24f + i * 0.43f, 0f), Vector3.zero, new Vector3(0.22f, 0.38f, 2.2f)));
                shelter.Add((cube, new Vector3(1.65f, 0.24f + i * 0.43f, 0f), Vector3.zero, new Vector3(0.22f, 0.38f, 2.2f)));
            }
            shelter.Add((cube, new Vector3(-1.43f, 0.95f, -1.08f), new Vector3(0f, 0f, -3f), new Vector3(0.23f, 2f, 0.26f)));
            shelter.Add((cube, new Vector3(1.43f, 0.95f, -1.08f), new Vector3(0f, 0f, 2f), new Vector3(0.23f, 2f, 0.26f)));
            shelter.Add((cube, new Vector3(0f, 1.94f, -1.12f), new Vector3(0f, 0f, -2f), new Vector3(3.7f, 0.32f, 0.36f)));
            for (int i = 0; i < 8; i++) shelter.Add((cube, new Vector3((i - 3.5f) * 0.48f, 1.92f, 0f), new Vector3(0f, 0f, Rand(i, 90) * 3f), new Vector3(0.45f, 0.18f, 2.8f)));
            dugout = Make(Combine("Timber supply dugout", shelter.ToArray()), timber, true, 1.25f);
            var innerColors = dugout.Mesh.colors;
            for (int v = 0; v < cube.vertexCount; v++) innerColors[v] = new Color(0.24f, 0.27f, 0.29f, 1f); // painted dark back of the recess
            dugout.Mesh.colors = innerColors;
            bunker = Make(Combine("Ruined concrete shelter",
                (cube, new Vector3(0f, 0.9f, 0.9f), Vector3.zero, new Vector3(3.4f, 1.8f, 0.3f)),
                (cube, new Vector3(-1.45f, 0.9f, -0.1f), new Vector3(0f, 0f, -3f), new Vector3(0.65f, 1.9f, 2.5f)),
                (cube, new Vector3(1.45f, 0.9f, -0.1f), new Vector3(0f, 0f, 2f), new Vector3(0.65f, 1.9f, 2.5f)),
                (cube, new Vector3(0f, 1.87f, -0.1f), new Vector3(0f, 0f, -2f), new Vector3(3.7f, 0.44f, 2.7f)),
                (cube, new Vector3(-1.2f, 2.22f, 0.32f), new Vector3(0f, 7f, 6f), new Vector3(0.7f, 0.65f, 0.9f)),
                (cube, new Vector3(-0.6f, 2.1f, 0.35f), new Vector3(0f, -5f, -9f), new Vector3(0.55f, 0.4f, 0.8f)),
                (cube, new Vector3(1.38f, 2.3f, 0.23f), new Vector3(0f, 0f, -7f), new Vector3(0.5f, 0.65f, 0.85f)),
                (cube, new Vector3(2.2f, 0.2f, -0.8f), new Vector3(8f, 28f, 12f), new Vector3(0.8f, 0.55f, 0.65f))), new Color(0.49f, 0.51f, 0.50f), true, 1.6f);
            var bunkerColors = bunker.Mesh.colors;
            for (int v = 0; v < cube.vertexCount; v++) bunkerColors[v] = new Color(0.22f, 0.25f, 0.27f, 1f);
            bunker.Mesh.colors = bunkerColors;
            roof = Make(Combine("Earth shelter roof",
                (berm, new Vector3(0f, 1.96f, 0.18f), Vector3.zero, new Vector3(1.65f, 1.1f, 1.5f)),
                (berm, new Vector3(-1.8f, 0.25f, 0.25f), new Vector3(0f, 90f, 0f), new Vector3(1.05f, 3.3f, 0.9f)),
                (berm, new Vector3(1.8f, 0.25f, 0.25f), new Vector3(0f, 90f, 0f), new Vector3(1.05f, 3.1f, 0.9f))), new Color(0.49f, 0.46f, 0.415f), true, 0.4f);
            supplies = Make(Combine("Braced ammunition crate",
                (cube, new Vector3(0f, 0.36f, 0f), Vector3.zero, new Vector3(1.05f, 0.68f, 0.68f)),
                (cube, new Vector3(-0.37f, 0.37f, -0.365f), Vector3.zero, new Vector3(0.10f, 0.75f, 0.06f)),
                (cube, new Vector3(0.37f, 0.37f, -0.365f), Vector3.zero, new Vector3(0.10f, 0.75f, 0.06f)),
                (cube, new Vector3(0f, 0.36f, -0.39f), new Vector3(0f, 0f, 28f), new Vector3(1.06f, 0.10f, 0.065f)),
                (cube, new Vector3(-0.37f, 0.735f, 0f), Vector3.zero, new Vector3(0.10f, 0.06f, 0.78f)),
                (cube, new Vector3(0.37f, 0.735f, 0f), Vector3.zero, new Vector3(0.10f, 0.06f, 0.78f))), new Color(0.43f, 0.435f, 0.31f), true, 1f);
            fork = Make(Combine("Forked shell tree",
                (Taper(0.58f, 0.17f, 5.5f, new Vector2(-0.5f, 0.12f)), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.20f, 0.04f, 3.2f, new Vector2(0.6f, 0f)), new Vector3(-0.22f, 2.2f, 0f), new Vector3(12f, 0f, -32f), Vector3.one),
                (Taper(0.12f, 0.015f, 1.4f, Vector2.zero), new Vector3(-0.35f, 3.8f, 0f), new Vector3(-10f, 0f, 58f), Vector3.one),
                (Taper(0.30f, 0.02f, 1.5f, new Vector2(0.1f, 0f)), new Vector3(0f, 0.12f, 0f), new Vector3(65f, 25f, 10f), Vector3.one)), new Color(0.32f, 0.28f, 0.24f));
        }

        Mesh Berm()
        {
            // A low open-bottom earth bank. Overlapping irregular skirts hide module joins without bead silhouettes.
            const int w = 8, d = 6;
            var v = new Vector3[(w + 1) * (d + 1)]; var colors = new Color[v.Length];
            var tris = new List<int>();
            for (int z = 0; z <= d; z++)
            for (int x = 0; x <= w; x++)
            {
                int i = z * (w + 1) + x;
                float fx = x / (float)w, fz = z / (float)d;
                float profile = Mathf.Sin(fz * Mathf.PI);
                float height = profile * (0.30f + 0.21f * Rand(x, z + 511));
                v[i] = new Vector3((fx - 0.5f) * 2.6f, height - 0.12f, (fz - 0.5f) * 2.5f + (Rand(x, 512) - 0.5f) * 0.24f);
                float pigment = 0.85f + Rand(x / 2, z / 2 + 513) * 0.17f;
                colors[i] = new Color(pigment, pigment, pigment, 1f);
                if (x == w || z == d) continue;
                tris.Add(i); tris.Add(i + w + 1); tris.Add(i + 1);
                tris.Add(i + 1); tris.Add(i + w + 1); tris.Add(i + w + 2);
            }
            var mesh = new Mesh { name = "Ragged earth bank", vertices = v, colors = colors, triangles = tris.ToArray() };
            mesh.RecalculateNormals(); mesh.RecalculateBounds(); ownedMeshes.Add(mesh);
            return mesh;
        }

        static float Rand(int i, int salt)
        {
            uint h = (uint)i * 0x9E3779B1u ^ (uint)salt * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        /// <summary>Dead trees and a few ruins on the land beyond the map: shapes in the haze, never part of the sim.</summary>
        void Horizon(MapData map)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            for (int i = 0; i < 520; i++)
            {
                float x = -260f + Rand(i, 1) * (w + 520f), z = -200f + Rand(i, 2) * (l + 400f);
                float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
                if (outside < 5f) continue;
                float level = GreyboxTerrainView.SkirtHeight(map, x, z) - 0.05f;
                float s = 0.9f + 0.7f * Rand(i, 3);
                var m = Matrix4x4.TRS(new Vector3(x, level, z), Quaternion.Euler(0f, Rand(i, 4) * 360f, 0f), new Vector3(s, s, s));
                if (Rand(i, 5) < 0.26f) fork.Add(m); else if (Rand(i, 5) < 0.62f) trunk.Add(m); else if (Rand(i, 5) < 0.85f) snag.Add(m); else stump.Add(m);
            }
            for (int i = 0; i < 9; i++)
            {
                bool farSide = i < 6;   // the standard view looks along -X, so most of them stand there
                float x = farSide ? -45f - Rand(i, 6) * 120f : w + 45f + Rand(i, 6) * 90f, z = Rand(i, 7) * l;
                float s = 1.2f + Rand(i, 8) * 1.0f;
                ruin.Add(Matrix4x4.TRS(new Vector3(x, GreyboxTerrainView.SkirtLevel - 0.25f, z), Quaternion.Euler(0f, 80f + Rand(i, 9) * 40f, 0f), new Vector3(s, s, s)));
            }
        }

        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type == TW.Sim.SimEventType.PropChanged || e.Type == TW.Sim.SimEventType.WireBreached) dirty = true;
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            foreach (var mesh in ownedMeshes) if (mesh != null) Destroy(mesh);
            if (all != null) foreach (var b in all) if (b.Material != null) Destroy(b.Material);
        }

        void Rebuild()
        {
            var map = Host.Local.Map;
            var hf = map.Height;
            foreach (var b in all) b.Clear();
            bounds = new Bounds(new Vector3(map.SizeMeters.x * 0.5f, 0f, map.SizeMeters.y * 0.5f), new Vector3(map.SizeMeters.x + 700f, 120f, map.SizeMeters.y + 600f));

            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float s = 0.85f + 0.3f * ((i * 37) % 100) / 100f;   // no two trees the same height
                var m = Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.05f, p.Pos.z), Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f), new Vector3(s, s, s));
                switch (p.Kind)
                {
                    case PropKind.Tree: (i % 3 == 0 ? fork : trunk).Add(m); break;
                    case PropKind.BrokenTree: snag.Add(m); fallen.Add(m); break;
                    case PropKind.Stump: stump.Add(m); break;
                    case PropKind.Log: log.Add(m); break;
                    case PropKind.Wreck: wreck.Add(Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.25f, p.Pos.z), Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f), Vector3.one)); break;
                    case PropKind.Bridge: bridge.Add(Matrix4x4.TRS(new Vector3(p.Pos.x, map.WaterLevel + 0.3f, p.Pos.z), Quaternion.identity, Vector3.one)); break;
                }
            }

            float n = MapData.NavCellSize;
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                if ((map.NavLayers[map.NavIndex(x, z)] & (byte)NavLayer.Wire) == 0) continue;
                float wx = (x + 0.5f) * n, wz = (z + 0.5f) * n;
                var at = new Vector3(wx, hf.Sample(wx, wz), wz);
                var turn = Quaternion.Euler(0f, (x * 13 + z * 7) % 16 - 8f, 0f);
                knifeRest.Add(Matrix4x4.TRS(at, turn, Vector3.one));
                wire.Add(Matrix4x4.TRS(at, turn, Vector3.one));
            }

            Horizon(map);

            // trench kit: every trench is two nav cells deep in Z; the lips are the rows just outside it
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                if (def.CellCount == 0) continue;
                int zMin = int.MaxValue, zMax = int.MinValue;
                for (int k = 0; k < def.CellCount; k++) { int cz = map.TrenchCells[def.CellStart + k] / map.NavWidth; zMin = Mathf.Min(zMin, cz); zMax = Mathf.Max(zMax, cz); }
                float rearZ = zMin * n, frontZ = (zMax + 1) * n;
                for (int k = 0; k < def.CellCount; k++)
                {
                    int cell = map.TrenchCells[def.CellStart + k];
                    int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
                    if (cz != zMin) continue;   // once per column
                    float wx = (cx + 0.5f) * n;
                    bool link = (map.NavLayers[cell] & (byte)NavLayer.Link) != 0;
                    float floor = hf.Sample(wx, (rearZ + frontZ) * 0.5f);
                    duckboards.Add(Matrix4x4.TRS(new Vector3(wx, floor + 0.035f, (rearZ + frontZ) * 0.5f), Quaternion.Euler(0f, Rand(cell, 91) * 3f - 1.5f, 0f), Vector3.one));
                    if (link)
                    {
                        ladder.Add(Matrix4x4.TRS(new Vector3(wx, floor, frontZ - 0.35f), Quaternion.identity, Vector3.one));
                        ladder.Add(Matrix4x4.TRS(new Vector3(wx, floor, rearZ + 0.35f), Quaternion.Euler(0f, 180f, 0f), Vector3.one));
                        continue;
                    }
                    float tilt = Rand(cell, 73) * 4f - 2f;
                    planks.Add(Matrix4x4.TRS(new Vector3(wx, floor, frontZ - 0.55f), Quaternion.Euler(-7f, 0f, tilt), new Vector3(1f, 0.95f, 1.5f)));
                    planks.Add(Matrix4x4.TRS(new Vector3(wx, floor, rearZ + 0.55f), Quaternion.Euler(-7f, 180f, tilt), new Vector3(1f, 0.95f, 1.5f)));
                    for (int side = 0; side < 2; side++)
                    {
                        float lip = side == 0 ? frontZ + 0.65f : rearZ - 0.65f;
                        float y = hf.Sample(wx, lip);
                        earth.Add(Matrix4x4.TRS(new Vector3(wx, y + 0.03f, lip), Quaternion.Euler(0f, Rand(cell, side + 74) * 14f - 7f, 0f), new Vector3(1f, 0.9f + Rand(cell, 75) * 0.5f, 1f)));
                        sandbags.Add(Matrix4x4.TRS(new Vector3(wx, y + 0.25f, lip), Quaternion.Euler(0f, tilt * 2f, tilt), new Vector3(1.04f, 0.82f, 1.20f)));
                    }
                }
                DressTrench(map, t, rearZ, frontZ);
            }
        }

        void DressTrench(MapData map, int trench, float rearZ, float frontZ)
        {
            // Sparse composition anchors, proportional to map width. Skip links, water and existing props.
            for (int anchor = 0; anchor < 2; anchor++)
            {
                float x = map.SizeMeters.x * (anchor == 0 ? 0.28f : 0.72f);
                float z = trench < map.Trenches.Length / 2 ? rearZ - 4.4f : frontZ + 4.4f;
                if (x < 4f || x > map.SizeMeters.x - 4f || z < 4f || z > map.SizeMeters.y - 4f) continue;
                bool clear = true;
                for (int dz = -3; dz <= 3; dz++)
                for (int dx = -3; dx <= 3; dx++)
                {
                    int nx = Mathf.Clamp((int)((x + dx) / MapData.NavCellSize), 0, map.NavWidth - 1);
                    int nz = Mathf.Clamp((int)((z + dz) / MapData.NavCellSize), 0, map.NavLength - 1);
                    if (((NavLayer)map.NavLayers[map.NavIndex(nx, nz)] & (NavLayer.Link | NavLayer.Trench | NavLayer.Wire | NavLayer.Blocked)) != 0) clear = false;
                    if (map.Height.Sample(x + dx, z + dz) < map.WaterLevel + 0.15f) clear = false;
                }
                foreach (var prop in map.Props) if (Mathf.Abs(prop.Pos.x - x) < 5f && Mathf.Abs(prop.Pos.z - z) < 5f) clear = false;
                if (!clear) continue;
                var at = new Vector3(x, map.Height.Sample(x, z) - 0.15f, z);
                var turn = Quaternion.Euler(0f, -65f + Rand(trench, anchor + 96) * 12f, 0f);
                var m = Matrix4x4.TRS(at, turn, Vector3.one);
                if (anchor == 0) { dugout.Add(m); roof.Add(m); }
                else bunker.Add(m);
                supplies.Add(m * Matrix4x4.TRS(new Vector3(-0.8f, 0f, -0.25f), Quaternion.Euler(0f, 8f, 0f), Vector3.one));
                supplies.Add(m * Matrix4x4.TRS(new Vector3(-0.75f, 0.76f, -0.20f), Quaternion.Euler(0f, -5f, 0f), Vector3.one * 0.85f));
                supplies.Add(m * Matrix4x4.TRS(new Vector3(2.25f, 0f, -0.75f), Quaternion.Euler(0f, 17f, 0f), Vector3.one));
            }
        }

        void Update()
        {
            if (Host == null || Host.Local == null || all == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (dirty) { dirty = false; Rebuild(); }
            var cam = Camera.main;
            if (cam != null) GeometryUtility.CalculateFrustumPlanes(cam, planes);
            VisibleInstances = SubmittedVertices = DrawCalls = 0;
            foreach (var b in all)
            {
                for (int p = 0; p < b.Counts.Count; p++)
                {
                    var pageBounds = b.PageBounds[p]; pageBounds.Expand(16f); // retain nearby offscreen shadow casters
                    if (cam != null && !GeometryUtility.TestPlanesAABB(planes, pageBounds)) continue;
                    var rp = new RenderParams(b.Material) { worldBounds = pageBounds, shadowCastingMode = b.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true };
                    Graphics.RenderMeshInstanced(rp, b.Mesh, 0, b.Pages[p], b.Counts[p]);
                    VisibleInstances += b.Counts[p]; SubmittedVertices += b.Counts[p] * b.Mesh.vertexCount; DrawCalls++;
                }
            }
        }
    }
}
