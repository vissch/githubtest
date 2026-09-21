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
            public void Clear() { Counts.Clear(); }
            public void Add(Matrix4x4 m)
            {
                int page = Counts.Count - 1;
                if (page < 0 || Counts[page] == 1023) { Counts.Add(0); page++; if (Pages.Count <= page) Pages.Add(new Matrix4x4[1023]); }
                Pages[page][Counts[page]] = m; Counts[page]++;
            }
        }

        Batch trunk, snag, fallen, stump, log, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin;
        Batch[] all;
        bool dirty = true, subscribed;
        Bounds bounds;

        static Mesh Primitive(PrimitiveType type) => Resources.GetBuiltinResource<Mesh>(type + ".fbx");

        /// <summary>A sphere of a few dozen vertices: sandbags and tree knots are drawn by the hundred.</summary>
        static Mesh Blob(int segments, int rings)
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
            return m;
        }

        /// <summary>A tapering six-sided trunk from y = 0 to height, leaning by lean metres at the top.</summary>
        static Mesh Taper(float r0, float r1, float height, Vector2 lean, float point = 1.6f)
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
            return m;
        }

        /// <summary>Joins parts into one mesh and stores a smoothed normal per vertex in UV3 for the ink outline:
        /// a hard-edged box pushed out along its face normals would come apart at the corners.</summary>
        static Mesh Combine(string name, params (Mesh mesh, Vector3 pos, Vector3 euler, Vector3 scale)[] parts)
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
            return m;
        }

        static Batch Make(Mesh mesh, Color color, bool shadows = true, float outline = 2.6f)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            mat.SetFloat("_OutlineWidth", outline);
            return new Batch { Mesh = mesh, Material = mat, Shadows = shadows };
        }

        void Start()
        {
            var cyl = Primitive(PrimitiveType.Cylinder); var cube = Primitive(PrimitiveType.Cube);
            var bag = Blob(8, 5);
            var bark = new Color(0.30f, 0.23f, 0.18f); var charred = new Color(0.22f, 0.17f, 0.15f);
            var timber = new Color(0.42f, 0.31f, 0.21f); var sack = new Color(0.60f, 0.55f, 0.45f);
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
            all = new[] { trunk, snag, fallen, stump, log, wreck, bridge, knifeRest, wire, sandbags, planks, ladder, ruin };
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
            const float level = 1.55f;   // GreyboxTerrainView's skirt levels out here
            for (int i = 0; i < 420; i++)
            {
                float x = -260f + Rand(i, 1) * (w + 520f), z = -200f + Rand(i, 2) * (l + 400f);
                float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
                if (outside < 32f) continue;   // the skirt is still sloping here
                float s = 0.9f + 0.7f * Rand(i, 3);
                var m = Matrix4x4.TRS(new Vector3(x, level, z), Quaternion.Euler(0f, Rand(i, 4) * 360f, 0f), new Vector3(s, s, s));
                if (Rand(i, 5) < 0.62f) trunk.Add(m); else if (Rand(i, 5) < 0.85f) snag.Add(m); else stump.Add(m);
            }
            for (int i = 0; i < 9; i++)
            {
                bool farSide = i < 6;   // the standard view looks along -X, so most of them stand there
                float x = farSide ? -45f - Rand(i, 6) * 120f : w + 45f + Rand(i, 6) * 90f, z = Rand(i, 7) * l;
                float s = 1.2f + Rand(i, 8) * 1.0f;
                ruin.Add(Matrix4x4.TRS(new Vector3(x, level - 0.2f, z), Quaternion.Euler(0f, 80f + Rand(i, 9) * 40f, 0f), new Vector3(s, s, s)));
            }
        }

        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type == TW.Sim.SimEventType.PropChanged || e.Type == TW.Sim.SimEventType.WireBreached) dirty = true;
        }

        void OnDestroy() { if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent; }

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
                    case PropKind.Tree: trunk.Add(m); break;
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
                    if (link)
                    {
                        ladder.Add(Matrix4x4.TRS(new Vector3(wx, floor, frontZ - 0.35f), Quaternion.identity, Vector3.one));
                        ladder.Add(Matrix4x4.TRS(new Vector3(wx, floor, rearZ + 0.35f), Quaternion.Euler(0f, 180f, 0f), Vector3.one));
                        continue;
                    }
                    planks.Add(Matrix4x4.TRS(new Vector3(wx, floor, frontZ - 0.1f), Quaternion.identity, Vector3.one));
                    planks.Add(Matrix4x4.TRS(new Vector3(wx, floor, rearZ + 0.1f), Quaternion.Euler(0f, 180f, 0f), Vector3.one));
                    sandbags.Add(Matrix4x4.TRS(new Vector3(wx, hf.Sample(wx, frontZ + 0.9f), frontZ + 0.7f), Quaternion.identity, Vector3.one));
                    sandbags.Add(Matrix4x4.TRS(new Vector3(wx, hf.Sample(wx, rearZ - 0.9f), rearZ - 0.7f), Quaternion.identity, Vector3.one));
                }
            }
        }

        void Update()
        {
            if (Host == null || Host.Local == null || all == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (dirty) { dirty = false; Rebuild(); }
            foreach (var b in all)
            {
                var rp = new RenderParams(b.Material) { worldBounds = bounds, shadowCastingMode = b.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true };
                for (int p = 0; p < b.Counts.Count; p++) Graphics.RenderMeshInstanced(rp, b.Mesh, 0, b.Pages[p], b.Counts[p]);
            }
        }
    }
}
