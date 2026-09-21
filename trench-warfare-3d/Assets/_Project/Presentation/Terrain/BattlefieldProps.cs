// Phase: B2 (implemented with code-made placeholder meshes; C4 art replaces the meshes, not the placement)
// Draws what stands on the battlefield, instanced: MapData.Props (trees, broken trees, stumps, logs, wrecks, the
// bridge), wire on Wire cells, and the trench kit along TrenchCells (sandbag parapets on both lips, plank revetment
// on both walls, a ladder where there is a link). Instance lists are rebuilt when the sim says something changed
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

        Batch trunk, crown, snag, fallen, stump, log, wreck, bridge, wirePost, wireCoil, sandbags, planks, ladder;
        Batch[] all;
        bool dirty = true, subscribed;
        Bounds bounds;

        static Mesh Primitive(PrimitiveType type) => Resources.GetBuiltinResource<Mesh>(type + ".fbx");

        static Mesh Combine(string name, params (Mesh mesh, Vector3 pos, Vector3 euler, Vector3 scale)[] parts)
        {
            var ci = new CombineInstance[parts.Length];
            for (int i = 0; i < parts.Length; i++) ci[i] = new CombineInstance { mesh = parts[i].mesh, transform = Matrix4x4.TRS(parts[i].pos, Quaternion.Euler(parts[i].euler), parts[i].scale) };
            var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            m.CombineMeshes(ci, true, true);
            return m;
        }

        static Batch Make(Mesh mesh, Color color, bool shadows = true, float smoothness = 0.05f)
        {
            var mat = new Material(Shader.Find("Universal Render Pipeline/Lit")) { enableInstancing = true, color = color, hideFlags = HideFlags.HideAndDontSave };
            mat.SetFloat("_Smoothness", smoothness);
            return new Batch { Mesh = mesh, Material = mat, Shadows = shadows };
        }

        void Start()
        {
            var cyl = Primitive(PrimitiveType.Cylinder); var cube = Primitive(PrimitiveType.Cube); var sphere = Primitive(PrimitiveType.Sphere);
            var bark = new Color(0.20f, 0.16f, 0.12f); var charred = new Color(0.12f, 0.10f, 0.09f);
            // Unity's cylinder is 2 m tall and 1 m across
            trunk = Make(Combine("Trunk",
                (cyl, new Vector3(0f, 3.4f, 0f), Vector3.zero, new Vector3(0.42f, 3.4f, 0.42f)),
                (cyl, new Vector3(0.7f, 5.2f, 0.1f), new Vector3(0f, 0f, -52f), new Vector3(0.14f, 1.0f, 0.14f)),
                (cyl, new Vector3(-0.6f, 4.4f, -0.2f), new Vector3(10f, 0f, 48f), new Vector3(0.12f, 0.9f, 0.12f))), bark);
            crown = Make(Combine("Crown",
                (sphere, new Vector3(0.2f, 6.6f, 0f), Vector3.zero, new Vector3(2.6f, 1.9f, 2.4f)),
                (sphere, new Vector3(-0.9f, 5.7f, 0.5f), Vector3.zero, new Vector3(1.7f, 1.3f, 1.6f))), new Color(0.21f, 0.27f, 0.15f));
            snag = Make(Combine("Snag",
                (cyl, new Vector3(0f, 1.5f, 0f), Vector3.zero, new Vector3(0.44f, 1.5f, 0.44f)),
                (cube, new Vector3(0.08f, 3.2f, 0f), new Vector3(0f, 20f, 14f), new Vector3(0.16f, 0.9f, 0.12f)),
                (cube, new Vector3(-0.1f, 3.0f, 0.06f), new Vector3(0f, -30f, -10f), new Vector3(0.12f, 0.6f, 0.14f))), charred);
            fallen = Make(Combine("FallenTop", (cyl, new Vector3(1.9f, 0.25f, 0.4f), new Vector3(0f, 18f, 84f), new Vector3(0.34f, 1.9f, 0.34f))), bark);
            stump = Make(Combine("Stump", (cyl, new Vector3(0f, 0.3f, 0f), Vector3.zero, new Vector3(0.55f, 0.32f, 0.55f))), charred);
            log = Make(Combine("Log", (cyl, new Vector3(0f, 0.28f, 0f), new Vector3(0f, 0f, 90f), new Vector3(0.5f, 2.2f, 0.5f))), bark);
            wreck = Make(Combine("Wreck",
                (cube, new Vector3(0f, 1.0f, 0f), new Vector3(0f, 0f, 7f), new Vector3(2.0f, 1.4f, 7.4f)),
                (cube, new Vector3(-1.45f, 0.85f, 0f), new Vector3(0f, 0f, 7f), new Vector3(0.9f, 2.0f, 7.9f)),
                (cube, new Vector3(1.45f, 0.75f, 0.3f), new Vector3(4f, 3f, 7f), new Vector3(0.9f, 1.8f, 7.6f)),
                (cube, new Vector3(0.3f, 1.95f, 1.4f), new Vector3(0f, 25f, 12f), new Vector3(1.4f, 0.4f, 1.6f))), new Color(0.17f, 0.13f, 0.11f));
            bridge = Make(Combine("Bridge",
                (cube, new Vector3(0f, 0.12f, 0f), Vector3.zero, new Vector3(5.2f, 0.18f, 24f)),
                (cube, new Vector3(-2.5f, 0.6f, 0f), Vector3.zero, new Vector3(0.12f, 0.9f, 24f)),
                (cube, new Vector3(2.5f, 0.6f, 0f), Vector3.zero, new Vector3(0.12f, 0.9f, 24f))), new Color(0.40f, 0.31f, 0.20f));
            wirePost = Make(Combine("WirePost",
                (cube, new Vector3(0f, 0.55f, 0f), new Vector3(0f, 0f, 28f), new Vector3(0.07f, 1.3f, 0.07f)),
                (cube, new Vector3(0f, 0.55f, 0f), new Vector3(0f, 0f, -28f), new Vector3(0.07f, 1.3f, 0.07f))), new Color(0.23f, 0.18f, 0.13f), false);
            wireCoil = Make(Combine("WireCoil",
                (cube, new Vector3(0f, 0.75f, 0f), new Vector3(0f, 0f, 0f), new Vector3(2.1f, 0.03f, 0.03f)),
                (cube, new Vector3(0f, 0.45f, 0.35f), new Vector3(0f, 8f, 6f), new Vector3(2.1f, 0.03f, 0.03f)),
                (cube, new Vector3(0f, 0.30f, -0.35f), new Vector3(0f, -8f, -5f), new Vector3(2.1f, 0.03f, 0.03f)),
                (cube, new Vector3(0f, 0.95f, -0.1f), new Vector3(0f, 5f, -7f), new Vector3(2.1f, 0.03f, 0.03f))), new Color(0.28f, 0.27f, 0.27f), false, 0.4f);
            sandbags = Make(Combine("Sandbags",
                (cube, new Vector3(-0.5f, 0.14f, 0f), new Vector3(0f, 4f, 0f), new Vector3(0.95f, 0.28f, 0.55f)),
                (cube, new Vector3(0.5f, 0.14f, 0.03f), new Vector3(0f, -5f, 0f), new Vector3(0.95f, 0.28f, 0.55f)),
                (cube, new Vector3(0f, 0.40f, 0f), new Vector3(0f, 3f, 0f), new Vector3(0.95f, 0.26f, 0.5f))), new Color(0.50f, 0.44f, 0.31f));
            planks = Make(Combine("Revetment",
                (cube, new Vector3(0f, 0.9f, 0f), Vector3.zero, new Vector3(2.0f, 1.8f, 0.08f)),
                (cube, new Vector3(-0.9f, 0.95f, 0.06f), Vector3.zero, new Vector3(0.1f, 1.9f, 0.1f)),
                (cube, new Vector3(0.9f, 0.95f, 0.06f), Vector3.zero, new Vector3(0.1f, 1.9f, 0.1f))), new Color(0.33f, 0.25f, 0.17f), false);
            ladder = Make(Combine("Ladder",
                (cube, new Vector3(-0.25f, 1.0f, 0f), new Vector3(-16f, 0f, 0f), new Vector3(0.07f, 2.3f, 0.07f)),
                (cube, new Vector3(0.25f, 1.0f, 0f), new Vector3(-16f, 0f, 0f), new Vector3(0.07f, 2.3f, 0.07f)),
                (cube, new Vector3(0f, 0.5f, -0.14f), Vector3.zero, new Vector3(0.5f, 0.05f, 0.06f)),
                (cube, new Vector3(0f, 1.0f, 0f), Vector3.zero, new Vector3(0.5f, 0.05f, 0.06f)),
                (cube, new Vector3(0f, 1.5f, 0.14f), Vector3.zero, new Vector3(0.5f, 0.05f, 0.06f))), new Color(0.45f, 0.36f, 0.24f), false);
            all = new[] { trunk, crown, snag, fallen, stump, log, wreck, bridge, wirePost, wireCoil, sandbags, planks, ladder };
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
            bounds = new Bounds(new Vector3(map.SizeMeters.x * 0.5f, 0f, map.SizeMeters.y * 0.5f), new Vector3(map.SizeMeters.x + 40f, 80f, map.SizeMeters.y + 40f));

            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float s = 0.85f + 0.3f * ((i * 37) % 100) / 100f;   // no two trees the same height
                var m = Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.05f, p.Pos.z), Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f), new Vector3(s, s, s));
                switch (p.Kind)
                {
                    case PropKind.Tree: trunk.Add(m); if (i % 3 != 0) crown.Add(m); break;   // a third are already bare
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
                wireCoil.Add(Matrix4x4.TRS(at, Quaternion.Euler(0f, (x * 13 + z * 7) % 24 - 12f, 0f), Vector3.one));
                if ((x + z) % 2 == 0) wirePost.Add(Matrix4x4.TRS(at, Quaternion.Euler(0f, 90f, 0f), Vector3.one));
            }

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
