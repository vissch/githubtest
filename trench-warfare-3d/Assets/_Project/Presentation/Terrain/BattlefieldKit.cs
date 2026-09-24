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
            /// <summary>Small things are not drawn beyond this (metres from the camera to their page), and only while the camera is close.</summary>
            public float MaxDistance = float.PositiveInfinity;
            /// <summary>The side of the square pages its instances are batched and culled in (BattlefieldProps): a sparse module
            /// spread over the whole field takes a draw call per page, so it uses larger ones.</summary>
            public float PageSize = 32f;
            /// <summary>"Set/Prop" for an imported prop, null for the procedural kit: named props can be edited by hand (PropLayout).</summary>
            public string Name;
            /// <summary>The kind's drawn scale (PropLayout.Look.Baseline, set by BattlefieldProps): the composer spaces the
            /// pieces round a prop and checks its footprint by it.</summary>
            public Vector3 Size = Vector3.one;
            /// <summary>False for a module that is only bookkeeping: its instances are placed, found, hidden and remembered
            /// like any other, but never drawn (a house chunk; its house's whole mesh draws it).</summary>
            public bool Drawn = true;
            /// <summary>Drawn with a per-instance chunk mask (BattlefieldProps.MaskOf), on a _CHUNKMASK material.</summary>
            public bool Masked;
            /// <summary>A kit prop that comes apart chunk by chunk (Resources/Env/&lt;set&gt;/Chunks): placed, named and edited as
            /// itself, but never drawn or hit as itself; BattlefieldProps puts its building's whole mesh and chunks where it stands.</summary>
            public HouseKit.House Sliced;
        }
        public Module trunk, snag, fallen, stump, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin, duckboards, dugout, roof, supplies, fork, bunker, branches, looseBoards, shellCases, bush, tuft, stones, reeds;
        /// <summary>The small things a close camera finds (Module.MaxDistance): what men drop, what a trench is hung with, what catches on the wire.</summary>
        public Module helmet, messKit, spade, ammoTin, boots, graveMarker, leanRifle, signBoard, bucket, hangingTins, phoneWire, rag, wireTins;
        /// <summary>Winter only (docs/18 W7): a run of ice hanging from an eave. Close-tier, like the rest above.</summary>
        public Module icicles;
        /// <summary>Winter ground micro-kit: what stands on the snow for the camera among the men and is not
        /// submitted at all at the standard view (BattlefieldProps culls a finite MaxDistance on CloseUp).</summary>
        public Module drift, iceShard, frostTuft, snowClod;
        /// <summary>
        /// The imported sets (Resources/Env, split per prop by Tools/envsplit.py, prepared by EnvKitImport): metre scale,
        /// ground pivot, front +Z, one graded texture per set (Tools/envgrade.py). The landmarks stand sparingly (BattlefieldComposer
        /// .Landmarks, the site blueprints, the horizon), the wire obstacles only on wire, and the planks, sacks, grass,
        /// rocks, stumps and cattails wherever the scatter rules put their kind, mixed in with the procedural pieces.
        /// </summary>
        public Module sodShelter, mgNest, armouredStand, pillbox, well,               // Siege
            fieldGun, tankTurret, biplane, shellStack, limber, dudShell,                // Weapons
            wallStub, rebarSlab, boulder, sandbag, gabion,                               // Stones
            bracedPlank, crossedBoards, hatchLid, plankDoor, corrugated,                 // Wood
            stakes, hedgehog, wireFence, wirePost, barricade,                            // Fence
            fallenLog, stumpTall, stumpSplit, stumpMoss, poppies, cattails, grass;       // Plants
        /// <summary>The village houses (HouseKit, Resources/Env/Houses): each a set of chunks drawn at one matrix, and
        /// the chunk a module draws. The chunks are unnamed modules, so no look or hand edit moves one out of its house.</summary>
        public HouseKit.House[] Houses = System.Array.Empty<HouseKit.House>();
        public readonly Dictionary<Module, HouseKit.Chunk> HouseChunkOf = new Dictionary<Module, HouseKit.Chunk>();
        public readonly Dictionary<Module, HouseKit.House> HouseOfWhole = new Dictionary<Module, HouseKit.House>();
        public const float SmallReach = 55f;
        /// <summary>The ground micro-kit's reach. Shorter than SmallReach because it is placed about four times
        /// as densely, and because below about 20 cm a thing is a speck rather than a shape past 30 m.</summary>
        public const float MicroReach = 30f;
        public readonly Module[] TrenchWalls = new Module[3], TrenchBags = new Module[3], TrenchFloors = new Module[3];
        readonly List<Module> modules = new List<Module>();
        public IReadOnlyList<Module> Modules => modules;
        readonly List<Mesh> ownedMeshes = new List<Mesh>();
        readonly List<Texture2D> ownedTextures = new List<Texture2D>();
        Texture2DArray pigmentSheet;
        /// <summary>
        /// The painted surfaces are layers of one shared array rather than a texture each: the shader is told which
        /// layer with _Pigment, and the sheet itself is global, so a module that is painted binds no texture of its
        /// own at all. Baked on first use and kept for the life of the kit.
        /// </summary>
        void Paint(Module module, BattlefieldPigment.Surface surface)
        {
            if (pigmentSheet == null)
            {
                pigmentSheet = BattlefieldPigment.Sheet();
                pigmentSheet.hideFlags = HideFlags.HideAndDontSave;
                Shader.SetGlobalTexture("_PigmentSheet", pigmentSheet);
            }
            module.Material.SetFloat("_Pigment", (int)surface);
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

        /// <summary>
        /// One set of buildings: a module a chunk for the bookkeeping (placed, hit, hidden and remembered one by one, never
        /// drawn), and one drawn module a building, its chunks masked off as they go. Unnamed, so no look's jitter pulls a
        /// building apart. A set's chunks share one material and its buildings one more: the same atlas cell, one tint.
        /// </summary>
        HouseKit.House[] BuildingSet(string set, Color tint, int first)
        {
            Material material = null;
            var houses = HouseKit.Load(set, chunk =>
            {
                var module = Imported(set, chunk, tint, true, 1.3f);
                module.Name = null; module.PageSize = 48f; module.Drawn = false;
                if (material == null) material = module.Material;
                else { Discard(module.Material); module.Material = material; }
                return module;
            }, first);
            if (material == null) return houses;
            var masked = new Material(material) { hideFlags = HideFlags.HideAndDontSave, enableInstancing = true };
            // _CHUNKMASK is a shader_feature: Resources/Env/Houses/HouseMask.mat carries it so a build keeps the variant
            masked.EnableKeyword("_CHUNKMASK");
            foreach (var house in houses)
            {
                var whole = HouseKit.BuildWhole(house);
                if (whole == null) continue;
                ownedMeshes.Add(whole);
                house.Whole = Make(whole, tint, true, 1.3f);
                Discard(house.Whole.Material);
                house.Whole.Material = masked; house.Whole.Masked = true; house.Whole.PageSize = 48f;
                HouseOfWhole[house.Whole] = house;
            }
            return houses;
        }

        /// <summary>The prop is drawn and hit as its sliced building from now on; without one (no chunks imported) it stays whole.</summary>
        void Slice(Module prop, string name)
        {
            if (prop == null) return;
            var house = System.Array.Find(Houses, h => h.Name == name && h.Whole != null);
            if (house == null) { Debug.LogWarning("BattlefieldKit: no sliced " + name + "; it stays whole"); return; }
            prop.Sliced = house; prop.Drawn = false;
        }

        static void Discard(Material m) { if (m == null) return; if (Application.isPlaying) Object.Destroy(m); else Object.DestroyImmediate(m); }

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
            Paint(module, pigment); return module;
        }

        public BattlefieldKit()
        {
            var cube = Primitive(PrimitiveType.Cube);
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
            // (the plain cylinder log is gone: PropKind.Log draws the imported fallen log)
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
            foreach (var b in new[] { planks, ladder, duckboards, dugout, supplies, knifeRest, bridge }) Paint(b, BattlefieldPigment.Surface.Timber);
            Paint(roof, BattlefieldPigment.Surface.Earth);
            Paint(bunker, BattlefieldPigment.Surface.Concrete);
            // sandbags are woven hessian up close, not a smooth seamed sheet: Sacking rather than Canvas
            Paint(sandbags, BattlefieldPigment.Surface.Sacking);
            BuildTrenchVariants(cube, sackMesh, timber, sack);
            foreach (var b in new[] { trunk, snag, fallen, stump, fork }) Paint(b, BattlefieldPigment.Surface.Bark);
            BuildImported();
        }

        /// <summary>
        /// The six Tripo sheets, halved and packed into one 4096x2048 image by Tools/envatlas.py: a 4 x 2 grid of
        /// 1024 cells in the order below, with the last two spare. Six 2048 sheets were about 17 MB of compressed
        /// VRAM and six texture bindings to draw props that are mostly a metre across. Nothing in the shader had to
        /// change — TW/Toon (URP) already transforms its UVs by _BaseMap_ST, so a set is chosen by giving the
        /// material its cell's scale and offset, and a prop's UVs (which live inside 0..1 of its own sheet) land in
        /// the cell. The grid is 4 wide rather than 3 because 3072 is not a power of two and the block compressor
        /// would not take it, which cost more memory than the packing saved.
        /// This order is the same list as SETS in envatlas.py and the two must not drift apart.
        /// </summary>
        static readonly string[] EnvSets = { "Fence", "Plants", "Siege", "Stones", "Weapons", "Wood", "Houses", "Military" };
        const int EnvCols = 4, EnvRows = 2;
        Texture2D envAtlas; bool envAtlasLoaded;

        /// <summary>Unity's V runs from the bottom and the packer's Y from the top, so the first row of cells is the
        /// upper half of the atlas.</summary>
        static Vector2 EnvOffset(int index) => new Vector2((index % EnvCols) / (float)EnvCols, 1f - (index / EnvCols + 1) / (float)EnvRows);

        /// <summary>A module drawn from an imported prop: its own mesh (Resources/Env/set/name) on the shared sheet.</summary>
        Module Imported(string set, string name, Color tint, bool shadows, float outline, float sway = 0f, float gloss = 0f)
        {
            var mesh = Resources.Load<Mesh>("Env/" + set + "/" + name);
            if (mesh == null) { Debug.LogError("BattlefieldKit: no mesh in Resources/Env/" + set + "/" + name + ".fbx"); mesh = Primitive(PrimitiveType.Cube); }
            if (!envAtlasLoaded) { envAtlasLoaded = true; envAtlas = Resources.Load<Texture2D>("Env/EnvAtlas"); if (envAtlas == null) Debug.LogError("BattlefieldKit: no Resources/Env/EnvAtlas — run Tools/envatlas.py"); }
            int cell = System.Array.IndexOf(EnvSets, set);
            var module = Make(mesh, tint, shadows, outline);
            module.PageSize = 64f;   // few and scattered: eight pages cover the field instead of twenty-odd draws
            module.Name = set + "/" + name;
            if (envAtlas != null && cell >= 0)
            {
                module.Material.SetTexture("_BaseMap", envAtlas);
                module.Material.SetTextureScale("_BaseMap", new Vector2(1f / EnvCols, 1f / EnvRows));
                module.Material.SetTextureOffset("_BaseMap", EnvOffset(cell));
            }
            else if (cell < 0) Debug.LogError("BattlefieldKit: set '" + set + "' is not in EnvSets, so it has no cell in the atlas");
            if (sway > 0f) module.Material.SetFloat("_Sway", sway);
            if (gloss > 0f) module.Material.SetFloat("_Gloss", gloss);
            return module;
        }

        void BuildImported()
        {
            // the sets are painted lighter and brighter than the field: brought down into its narrow value range
            var paint = new Color(.84f, .83f, .80f); var growth = new Color(.74f, .76f, .66f); var steel = new Color(.80f, .80f, .78f);
            sodShelter = Imported("Siege", "SodShelterRuin", growth, true, 1.4f);
            mgNest = Imported("Siege", "MGNest", paint, true, 1.3f);
            armouredStand = Imported("Siege", "ArmouredStand", paint, true, 1.4f);
            pillbox = Imported("Siege", "Pillbox", paint, true, 1.6f);
            well = Imported("Siege", "Well", paint, true, 1.3f);
            fieldGun = Imported("Weapons", "FieldGun", steel, true, 1.3f, 0f, .25f);
            tankTurret = Imported("Weapons", "TankTurret", steel, true, 1.3f, 0f, .25f);
            biplane = Imported("Weapons", "Biplane", paint, true, 1.3f);
            shellStack = Imported("Weapons", "ShellStack", steel, true, 1.0f, 0f, .2f);
            limber = Imported("Weapons", "WreckedLimber", paint, true, 1.1f);
            dudShell = Imported("Weapons", "DudShell", steel, true, .9f, 0f, .3f);
            wallStub = Imported("Stones", "WallStub", paint, true, 1.4f);
            rebarSlab = Imported("Stones", "RebarSlab", paint, true, 1.0f);
            boulder = Imported("Stones", "Boulder", paint, false, 1.0f);
            sandbag = Imported("Stones", "Sandbag", paint, false, .8f);
            gabion = Imported("Stones", "Gabion", paint, true, 1.0f);
            bracedPlank = Imported("Wood", "BracedPlank", paint, false, .8f);
            crossedBoards = Imported("Wood", "CrossedBoards", paint, true, .9f);
            hatchLid = Small(Imported("Wood", "HatchLid", paint, false, .7f));
            plankDoor = Imported("Wood", "PlankDoor", paint, false, .8f);
            corrugated = Imported("Wood", "CorrugatedSheet", steel, false, .8f, 0f, .2f);
            stakes = Imported("Fence", "Stakes", paint, false, .9f);
            hedgehog = Imported("Fence", "TimberHedgehog", paint, false, .9f);
            wireFence = Imported("Fence", "WireFence", paint, false, .8f);
            wirePost = Imported("Fence", "WirePost", paint, false, .8f);
            barricade = Imported("Fence", "StoneBarricade", paint, true, 1.1f);
            fallenLog = Imported("Plants", "FallenLog", paint, true, 1.1f);
            stumpTall = Imported("Plants", "SplitStumpTall", paint, true, 1.1f);
            stumpSplit = Imported("Plants", "SplitStump", paint, true, 1.0f);
            stumpMoss = Imported("Plants", "MossStump", growth, true, 1.0f);
            poppies = Imported("Plants", "Poppies", new Color(.88f, .84f, .82f), false, .4f, .45f);
            cattails = Imported("Plants", "Cattails", growth, false, .4f, .30f);
            grass = Imported("Plants", "GrassClump", growth, false, .35f, .50f);
            // the buildings, a set at a time: the village houses and the rear's military buildings (HouseKit), one array
            // and the kit's own props that come apart the same way, each set's in its Chunks folder (Tools/housesplit.py TW_KEEP)
            var all = new List<HouseKit.House>();
            foreach (var set in new[] { "Houses", "Military", "Siege", "Stones", "Weapons" }) all.AddRange(BuildingSet(set, paint, all.Count));
            Houses = all.ToArray();
            foreach (var house in Houses) foreach (var chunk in house.Chunks) HouseChunkOf[chunk.Module] = chunk;
            Slice(well, "Well"); Slice(wallStub, "WallStub"); Slice(biplane, "Biplane"); Slice(fieldGun, "FieldGun");
        }

        void BuildTrenchVariants(Mesh cube, Mesh sackMesh, Color timber, Color sack)
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
                Paint(TrenchWalls[variant], BattlefieldPigment.Surface.Timber);
                parts.Clear();
                for (int bag = 0; bag < 2; bag++)
                    parts.Add((sackMesh, new Vector3((bag - .5f) * 1.02f, .15f, (Rand(bag, variant + 315) - .5f) * .10f),
                        new Vector3(0f, (Rand(bag, variant + 316) - .5f) * 15f, (Rand(bag, variant + 317) - .5f) * 5f), new Vector3(1.06f, .33f, .73f)));
                if (variant != 2) parts.Add((sackMesh, new Vector3(variant == 0 ? -.04f : .30f, .44f, -.08f), new Vector3(0f, -8f, 3f), new Vector3(1.02f, .37f, .65f)));
                if (variant == 0) parts.Add((sackMesh, new Vector3(1.0f, .43f, -.05f), new Vector3(0f, 7f, -3f), new Vector3(.95f, .35f, .63f)));
                TrenchBags[variant] = Make(Combine("Settled parapet " + variant, parts.ToArray()), sack);
                Paint(TrenchBags[variant], BattlefieldPigment.Surface.Sacking);
                parts.Clear();
                for (int board = 0; board < 5; board++)
                    parts.Add((cube, new Vector3((board - 2) * .39f, .08f + Rand(board, variant + 321) * .025f, (Rand(board, variant + 322) - .5f) * .14f),
                        new Vector3(0f, (Rand(board, variant + 323) - .5f) * 7f, 0f), new Vector3(.32f + Rand(board, variant + 324) * .045f, .10f, 1.43f + Rand(board, variant + 325) * .30f)));
                for (int rail = -1; rail <= 1; rail += 2) parts.Add((cube, new Vector3(0f, .025f, rail * .55f), Vector3.zero, new Vector3(2f, .10f, .14f)));
                TrenchFloors[variant] = Make(Combine("Uneven duckboards " + variant, parts.ToArray()), timber, false, .8f);
                Paint(TrenchFloors[variant], BattlefieldPigment.Surface.Timber);
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
            reeds = Make(Combine("Reeds",
                (Taper(0.028f, 0.006f, 1.45f, new Vector2(0.10f, 0.02f), 1f), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.026f, 0.006f, 1.20f, new Vector2(-0.12f, 0.06f), 1f), new Vector3(0.09f, 0f, 0.04f), Vector3.zero, Vector3.one),
                (Taper(0.026f, 0.006f, 1.62f, new Vector2(0.03f, -0.13f), 1f), new Vector3(-0.07f, 0f, 0.06f), Vector3.zero, Vector3.one),
                (Taper(0.024f, 0.006f, 0.95f, new Vector2(0.16f, 0.10f), 1f), new Vector3(0.02f, 0f, -0.10f), Vector3.zero, Vector3.one),
                (Taper(0.024f, 0.006f, 1.30f, new Vector2(-0.08f, -0.10f), 1f), new Vector3(-0.11f, 0f, -0.05f), Vector3.zero, Vector3.one),
                (Taper(0.022f, 0.006f, 0.80f, new Vector2(0.20f, -0.04f), 1f), new Vector3(0.13f, 0f, -0.03f), Vector3.zero, Vector3.one),
                (Taper(0.045f, 0.030f, 0.20f, Vector2.zero, 0.5f), new Vector3(0.10f, 1.40f, 0.02f), new Vector3(6f, 0f, 4f), Vector3.one),      // seed heads
                (Taper(0.045f, 0.030f, 0.18f, Vector2.zero, 0.5f), new Vector3(-0.04f, 1.56f, -0.07f), new Vector3(-5f, 0f, 8f), Vector3.one)), new Color(0.44f, 0.45f, 0.27f), false, 0.35f);
            // what grows bends in the wind (TW/Toon _Sway); reeds most, scrub least
            reeds.Material.SetFloat("_Sway", .34f); tuft.Material.SetFloat("_Sway", .55f); bush.Material.SetFloat("_Sway", .10f);
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
            BuildSmallKit(cube);
            fork = Make(Combine("Forked shell tree",
                (Taper(0.58f, 0.17f, 5.5f, new Vector2(-0.5f, 0.12f)), Vector3.zero, Vector3.zero, Vector3.one),
                (Taper(0.20f, 0.04f, 3.2f, new Vector2(0.6f, 0f)), new Vector3(-0.22f, 2.2f, 0f), new Vector3(12f, 0f, -32f), Vector3.one),
                (Taper(0.12f, 0.015f, 1.4f, Vector2.zero), new Vector3(-0.35f, 3.8f, 0f), new Vector3(-10f, 0f, 58f), Vector3.one),
                (Taper(0.30f, 0.02f, 1.5f, new Vector2(0.1f, 0f)), new Vector3(0f, 0.12f, 0f), new Vector3(65f, 25f, 10f), Vector3.one)), new Color(0.32f, 0.28f, 0.24f));
        }

        Module Small(Module module) { module.MaxDistance = SmallReach; return module; }
        Module Micro(Module module) { module.MaxDistance = MicroReach; return module; }

        /// <summary>
        /// Things no bigger than a man's hand or boot. None casts a shadow, all are a hundred-odd vertices at most, and none
        /// is drawn beyond SmallReach or at the standard view (BattlefieldProps): they are there for the camera among the men.
        /// </summary>
        void BuildSmallKit(Mesh cube)
        {
            var dome = Blob(8, 5); var tin = Taper(0.07f, 0.07f, 0.10f, Vector2.zero, 0.1f);
            var steel = new Color(0.36f, 0.40f, 0.34f); var leather = new Color(0.17f, 0.14f, 0.115f); var rifleWood = new Color(0.31f, 0.23f, 0.16f);
            // W3 and the ground micro-kit. A drift is two shallow wedges set a few degrees apart, so the crest
            // wanders instead of being a ruled line, with a thin lip along the windward side where the pack has
            // been cut back. Under 60 vertices and no shadow.
            drift = Micro(Make(Combine("Snow drift",
                (dome, new Vector3(0f, -0.13f, 0f), new Vector3(0f, 4f, 0f), new Vector3(2.90f, 0.23f, 0.70f)),
                (dome, new Vector3(0.95f, -0.14f, 0.16f), new Vector3(0f, -13f, 0f), new Vector3(1.90f, 0.17f, 0.52f)),
                (dome, new Vector3(-1.15f, -0.15f, -0.10f), new Vector3(0f, 11f, 0f), new Vector3(1.40f, 0.14f, 0.44f)),
                // the crest: three short wedges end to end, each tipped a little differently, so the top is a
                // scalloped ridge with hard edges the ink pass can find rather than a smooth swelling
                (cube, new Vector3(-0.85f, 0.045f, -0.02f), new Vector3(0f, 7f, 15f), new Vector3(1.15f, 0.10f, 0.20f)),
                (cube, new Vector3(0.20f, 0.060f, 0.03f), new Vector3(0f, -4f, -11f), new Vector3(1.25f, 0.11f, 0.22f)),
                (cube, new Vector3(1.20f, 0.040f, -0.04f), new Vector3(0f, 12f, 19f), new Vector3(0.95f, 0.08f, 0.17f)),
                // the lee scarp: where the pack has broken away, near vertical and cut back under the crest
                (cube, new Vector3(-0.10f, -0.06f, -0.30f), new Vector3(-72f, 5f, 0f), new Vector3(2.35f, 0.18f, 0.05f))),
                new Color(0.94f, 0.96f, 1.00f), false, 0.7f));
            // A crust broken and tilted out of itself. Pale, and the one thing here with a hard edge.
            iceShard = Micro(Make(Combine("Broken crust",
                (cube, new Vector3(0f, 0.05f, 0f), new Vector3(24f, 12f, 7f), new Vector3(0.34f, 0.028f, 0.30f)),
                (cube, new Vector3(0.19f, 0.03f, -0.13f), new Vector3(-13f, 52f, 5f), new Vector3(0.22f, 0.024f, 0.20f))),
                new Color(0.90f, 0.95f, 1.00f), false, 0.6f));
            iceShard.Material.SetFloat("_Gloss", 0.45f);
            // Dead stalks: the only vertical break in a field of horizontals.
            frostTuft = Micro(Make(Combine("Frozen tuft",
                (Taper(0.012f, 0.002f, 0.30f, new Vector2(0.06f, 0f), 1.3f), new Vector3(0f, 0f, 0f), new Vector3(9f, 0f, 6f), Vector3.one),
                (Taper(0.010f, 0.002f, 0.24f, new Vector2(0.09f, 0f), 1.3f), new Vector3(0.05f, 0f, 0.04f), new Vector3(14f, 70f, -11f), Vector3.one),
                (Taper(0.011f, 0.002f, 0.34f, new Vector2(0.04f, 0f), 1.3f), new Vector3(-0.04f, 0f, 0.03f), new Vector3(-7f, 200f, 9f), Vector3.one),
                (dome, new Vector3(0f, 0.01f, 0f), Vector3.zero, new Vector3(0.20f, 0.05f, 0.18f))),
                new Color(0.55f, 0.52f, 0.46f), false, 0.7f));
            // The lump of frozen earth a shell threw, snowed over.
            snowClod = Micro(Make(Combine("Frozen clod",
                (dome, new Vector3(0f, -0.02f, 0f), new Vector3(0f, 31f, 0f), new Vector3(0.30f, 0.20f, 0.26f))),
                new Color(0.88f, 0.91f, 0.96f), false, 0.7f));
            // W7. A row of seven spikes of uneven length along a thin ledge of frozen melt, leaning very slightly
            // off the wall the way a drip freezes as it runs. Taper's point exponent is what makes it a needle
            // rather than a cone: an icicle is concave, thickest at the eave and drawn out to nothing.
            var iceLengths = new[] { 0.42f, 0.17f, 0.61f, 0.28f, 0.50f, 0.13f, 0.35f };
            var iceParts = new System.Collections.Generic.List<(Mesh, Vector3, Vector3, Vector3)>
            {
                (cube, new Vector3(0f, 0.02f, 0f), Vector3.zero, new Vector3(1.30f, 0.045f, 0.075f)),   // the frozen run along the eave
            };
            for (int s = 0; s < iceLengths.Length; s++)
            {
                float at = -0.60f + s * 0.20f, len = iceLengths[s];
                iceParts.Add((Taper(0.032f + len * 0.05f, 0.004f, len, new Vector2(0f, 0.05f), 3.1f),
                              new Vector3(at, -len * 0.5f + 0.02f, 0f), new Vector3(0f, 0f, 180f), Vector3.one));
            }
            icicles = Small(Make(Combine("Icicles", iceParts.ToArray()), new Color(0.95f, 0.98f, 1.00f), false, 0.55f));
            // Ice is brighter than the wall it hangs on and it is the one glossy thing on a matt winter field:
            // fresh snow is held matt on purpose (SnowSparkle 0.15) so that the ice carries the highlight. Make
            // sets only colour and outline, so the gloss the Toon shader already has goes on here by hand.
            icicles.Material.SetFloat("_Gloss", 0.62f);
            helmet = Small(Make(Combine("Lost helmet",
                (dome, new Vector3(0f, 0.06f, 0f), new Vector3(0f, 0f, 14f), new Vector3(0.29f, 0.15f, 0.31f)),
                (dome, new Vector3(0f, 0.035f, 0f), new Vector3(0f, 0f, 14f), new Vector3(0.40f, 0.03f, 0.43f))), steel, false, 0.9f));
            messKit = Small(Make(Combine("Mess tin and bottle",
                (tin, new Vector3(0f, 0f, 0f), Vector3.zero, new Vector3(1.2f, 1f, 0.8f)),
                (tin, new Vector3(0.22f, 0.05f, 0.10f), new Vector3(90f, 35f, 0f), new Vector3(0.7f, 1.1f, 0.7f)),
                (Taper(0.045f, 0.018f, 0.27f, Vector2.zero, 0.1f), new Vector3(-0.16f, 0.045f, -0.12f), new Vector3(90f, -60f, 0f), Vector3.one)), new Color(0.44f, 0.46f, 0.45f), false, 0.7f));
            spade = Small(Make(Combine("Entrenching tool",
                (cube, new Vector3(0f, 0.36f, 0f), new Vector3(9f, 0f, -7f), new Vector3(0.035f, 0.62f, 0.035f)),
                (cube, new Vector3(0.04f, 0.70f, 0.055f), new Vector3(9f, 0f, -7f), new Vector3(0.13f, 0.035f, 0.04f)),
                (WornBox(.02f, .02f, 41), new Vector3(-0.015f, 0.06f, -0.02f), new Vector3(9f, 0f, -7f), new Vector3(0.17f, 0.24f, 0.022f))), new Color(0.34f, 0.29f, 0.22f), false, 0.8f));
            ammoTin = Small(Make(Combine("Open ammunition tin",
                (WornBox(.03f, .02f, 42), new Vector3(0f, 0.10f, 0f), new Vector3(0f, 8f, 0f), new Vector3(0.42f, 0.20f, 0.24f)),
                (WornBox(.03f, .02f, 43), new Vector3(0f, 0.27f, -0.19f), new Vector3(-68f, 8f, 0f), new Vector3(0.42f, 0.02f, 0.24f)),
                (cube, new Vector3(0.24f, 0.015f, 0.22f), new Vector3(0f, 40f, 0f), new Vector3(0.10f, 0.03f, 0.05f)),
                (cube, new Vector3(0.10f, 0.015f, 0.30f), new Vector3(0f, -25f, 0f), new Vector3(0.10f, 0.03f, 0.05f))), new Color(0.33f, 0.36f, 0.25f), false, 0.9f));
            boots = Small(Make(Combine("A pair of boots",
                (WornBox(.04f, .03f, 44), new Vector3(0f, 0.05f, 0.05f), Vector3.zero, new Vector3(0.11f, 0.10f, 0.29f)),
                (WornBox(.04f, .03f, 45), new Vector3(0f, 0.19f, -0.04f), new Vector3(-6f, 0f, 0f), new Vector3(0.10f, 0.22f, 0.12f)),
                (WornBox(.04f, .03f, 46), new Vector3(0.24f, 0.055f, 0f), new Vector3(0f, 35f, 88f), new Vector3(0.11f, 0.10f, 0.29f)),
                (WornBox(.04f, .03f, 47), new Vector3(0.36f, 0.05f, -0.07f), new Vector3(0f, 35f, 88f), new Vector3(0.10f, 0.22f, 0.12f))), leather, false, 0.8f));
            graveMarker = Small(Make(Combine("Rifle and helmet",
                (cube, new Vector3(0f, 0.55f, 0f), new Vector3(4f, 0f, -5f), new Vector3(0.045f, 1.10f, 0.06f)),
                (cube, new Vector3(0.035f, 0.98f, 0.03f), new Vector3(4f, 0f, -5f), new Vector3(0.055f, 0.36f, 0.12f)),
                (dome, new Vector3(0.05f, 1.20f, 0.04f), new Vector3(6f, 0f, -14f), new Vector3(0.29f, 0.15f, 0.31f)),
                (dome, new Vector3(0.05f, 1.175f, 0.04f), new Vector3(6f, 0f, -14f), new Vector3(0.40f, 0.03f, 0.43f))), new Color(0.29f, 0.27f, 0.21f), false, 0.9f));
            // a trench is lived in: rifles stood against the wall, a board that points the way, a bucket, tins on a nail, the telephone wire
            leanRifle = Small(Make(Combine("Rifle stood against the wall",
                (cube, new Vector3(0f, 0.62f, 0f), Vector3.zero, new Vector3(0.04f, 1.22f, 0.055f)),
                (cube, new Vector3(0f, 0.20f, -0.02f), Vector3.zero, new Vector3(0.05f, 0.40f, 0.11f)),
                (cube, new Vector3(0.22f, 0.60f, 0.02f), new Vector3(0f, 0f, 3f), new Vector3(0.04f, 1.18f, 0.055f)),
                (cube, new Vector3(0.21f, 0.19f, 0f), new Vector3(0f, 0f, 3f), new Vector3(0.05f, 0.40f, 0.11f))), rifleWood, false, 0.8f));
            signBoard = Small(Make(Combine("Trench signboard",
                (cube, new Vector3(0f, 0.70f, 0f), new Vector3(0f, 0f, 3f), new Vector3(0.07f, 1.40f, 0.07f)),
                (WornBox(.03f, .03f, 48), new Vector3(0.20f, 1.22f, 0.05f), new Vector3(0f, 0f, -4f), new Vector3(0.74f, 0.20f, 0.035f)),
                (WornBox(.03f, .03f, 49), new Vector3(-0.12f, 0.95f, 0.05f), new Vector3(0f, 0f, 6f), new Vector3(0.52f, 0.16f, 0.035f))), new Color(0.52f, 0.45f, 0.34f), false, 0.9f));
            bucket = Small(Make(Combine("Bucket",
                (Taper(0.12f, 0.16f, 0.28f, Vector2.zero, 0.1f), Vector3.zero, new Vector3(0f, 0f, 3f), Vector3.one),
                (cube, new Vector3(0f, 0.30f, 0f), new Vector3(0f, 20f, 0f), new Vector3(0.30f, 0.015f, 0.015f))), new Color(0.40f, 0.42f, 0.43f), false, 0.8f));
            hangingTins = Small(Make(Combine("Tins on a nail",
                (cube, new Vector3(0f, 0f, 0f), Vector3.zero, new Vector3(0.5f, 0.05f, 0.03f)),
                (tin, new Vector3(-0.15f, -0.24f, -0.06f), new Vector3(0f, 0f, 6f), new Vector3(0.9f, 1.3f, 0.6f)),
                (tin, new Vector3(0.10f, -0.20f, -0.06f), new Vector3(0f, 0f, -8f), new Vector3(0.8f, 1.0f, 0.8f)),
                (cube, new Vector3(0.22f, -0.22f, -0.04f), new Vector3(0f, 0f, 4f), new Vector3(0.10f, 0.34f, 0.02f))), new Color(0.42f, 0.43f, 0.40f), false, 0.7f));
            var line = new List<(Mesh, Vector3, Vector3, Vector3)>();
            for (int i = 0; i < 4; i++)
            {
                // a 2 m length that sags between its two staples: four straight pieces along a shallow curve
                float t0 = i / 4f, t1 = (i + 1) / 4f, y0 = -0.14f * (1f - (2f * t0 - 1f) * (2f * t0 - 1f)), y1 = -0.14f * (1f - (2f * t1 - 1f) * (2f * t1 - 1f));
                line.Add((cube, new Vector3((t0 + t1) - 1f, (y0 + y1) * 0.5f, 0f), new Vector3(0f, 0f, Mathf.Atan2(y1 - y0, 0.5f) * Mathf.Rad2Deg), new Vector3(0.52f, 0.016f, 0.016f)));
            }
            phoneWire = Small(Make(Combine("Telephone wire", line.ToArray()), new Color(0.10f, 0.09f, 0.09f), false, 0f));
            // caught on the wire: a torn strip of cloth that the wind pulls at, and the tins hung there to rattle when it is touched.
            // Both hang from their origin (y below 0), so TW/Toon's sway moves the free end.
            rag = Small(Make(Combine("Rag on the wire",
                (cube, new Vector3(0f, -0.24f, 0f), new Vector3(0f, 0f, 5f), new Vector3(0.17f, 0.50f, 0.012f)),
                (cube, new Vector3(0.10f, -0.15f, 0.01f), new Vector3(0f, 12f, -9f), new Vector3(0.09f, 0.32f, 0.012f))), new Color(0.52f, 0.50f, 0.44f), false, 0.6f));
            rag.Material.SetFloat("_Sway", 1.6f);
            wireTins = Small(Make(Combine("Tins on the wire",
                (cube, new Vector3(-0.12f, -0.07f, 0f), Vector3.zero, new Vector3(0.008f, 0.14f, 0.008f)),
                (tin, new Vector3(-0.12f, -0.24f, 0f), Vector3.zero, new Vector3(0.75f, 1.0f, 0.75f)),
                (cube, new Vector3(0.10f, -0.05f, 0f), Vector3.zero, new Vector3(0.008f, 0.10f, 0.008f)),
                (tin, new Vector3(0.10f, -0.20f, 0f), new Vector3(0f, 0f, 10f), new Vector3(0.75f, 1.0f, 0.75f))), new Color(0.43f, 0.41f, 0.37f), false, 0.6f));
            wireTins.Material.SetFloat("_Sway", 3.5f);
            // steel and tin are wet and bright-edged: under the moon they are what picks a small thing out of the mud
            // Painted metal, and it has been out in the rain: pitted patches with the rust running down from them.
            // These were flat colour until now because a pigment used to cost a texture and a binding; on the
            // shared sheet it costs a float, so the small things a close camera finds finally have a surface.
            foreach (var metal in new[] { helmet, messKit, ammoTin, bucket, hangingTins, wireTins, graveMarker })
            { metal.Material.SetFloat("_Gloss", .42f); Paint(metal, BattlefieldPigment.Surface.Rust); }
            Paint(stones, BattlefieldPigment.Surface.Stone);
            Paint(shellCases, BattlefieldPigment.Surface.Rust);
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
            if (pigmentSheet != null) Object.Destroy(pigmentSheet);
            foreach (var module in Modules) if (module.Material != null) Object.Destroy(module.Material);
        }
    }
}
