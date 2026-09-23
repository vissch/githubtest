// Phase: B5 (implemented) — the village houses break chunk by chunk. The owner's Tripo sheet of six ruined houses
// (2026-09-23) is cut by Tools/housesplit.py (Blender) into chunks of at most about 2.4 m, each its own FBX in
// Resources/Env/Houses, pivot at the middle of its base, with Resources/Env/Houses/houses.json giving each chunk's
// offset in its house, its bounds and whether it is stone or timber. A house is placed as one matrix: every chunk is
// drawn at house * Translate(offset), so it stands whole until PropDestruction knocks chunks out of it.
//
// What rests on what is worked out here from the bounds alone: a chunk whose foot is on the ground stands by itself;
// any other chunk rests on the chunks below it that it overlaps and nearly touches. When everything a chunk rests on
// has gone, it comes down too (PropDestruction.Unsupported), so a shelled house loses its roof after its walls.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public static class HouseKit
    {
        public const string Folder = "Env/Houses/";
        /// <summary>A chunk whose foot is this near its house's floor stands on the ground.</summary>
        public const float GroundedBelow = 0.35f;
        /// <summary>How far a chunk's foot may sit above the top of one below it and still rest on it (cuts leave a seam,
        /// Tripo parts do not quite touch), and how much the two must overlap across.</summary>
        public const float RestGap = 0.25f, RestOverlap = 0.05f;

        public sealed class Chunk
        {
            public BattlefieldKit.Module Module;
            public int House, Index;
            /// <summary>Its pivot in its house's frame (house origin at the middle of its base, front +Z).</summary>
            public Vector3 Offset;
            /// <summary>Its bounds in its house's frame.</summary>
            public Bounds Local;
            public bool Timber, Grounded;
            /// <summary>Indices of the chunks it rests on, and of the chunks that rest on it.</summary>
            public int[] RestsOn = Array.Empty<int>(), Carries = Array.Empty<int>();
        }

        /// <summary>A house's chunks in one mesh, a chunk to a bit of a float's 24-bit whole-number range.</summary>
        public const int MaxChunks = 24;

        public sealed class House
        {
            public string Name;
            /// <summary>The set it came from, its folder in Resources/Env: "Houses" (the village), "Military" (the rear).</summary>
            public string Set;
            public int Index;
            /// <summary>The drawn module: every chunk in one mesh, chunk index in UV1.x, hidden per instance by its mask.</summary>
            public BattlefieldKit.Module Whole;
            /// <summary>Bits for every chunk the house has.</summary>
            public int AllBits => Chunks.Length >= 31 ? ~0 : (1 << Chunks.Length) - 1;
            /// <summary>Everything the house draws, in its own frame.</summary>
            public Bounds Bounds;
            public Chunk[] Chunks;
        }

        [Serializable] sealed class Row { public string house = "", chunk = "", mat = ""; public int tris, verts; public float[] offset = new float[3], min = new float[3], max = new float[3]; }
        [Serializable] sealed class Rows { public Row[] chunks = Array.Empty<Row>(); }

        /// <summary>Reads the manifest and makes a module per chunk with <paramref name="module"/> (chunk name to module).
        /// Empty if the manifest is missing.</summary>
        public static House[] Load(Func<string, BattlefieldKit.Module> module) => Load("Houses", module, 0);

        /// <summary>One set, from Resources/Env/&lt;set&gt;/houses.json. Its houses are numbered from <paramref name="first"/>, so
        /// sets loaded one after another share one array (BattlefieldKit.Houses) and a chunk's House indexes into it.</summary>
        public static House[] Load(string set, Func<string, BattlefieldKit.Module> module, int first)
        {
            var folder = "Env/" + set + "/";
            var text = Resources.Load<TextAsset>(folder + "houses");
            if (text == null) { Debug.LogError("HouseKit: no Resources/" + folder + "houses.json — run Tools/housesplit.py"); return Array.Empty<House>(); }
            var houses = Build(JsonUtility.FromJson<Rows>(text.text).chunks, module, first);
            foreach (var house in houses) house.Set = set;
            return houses;
        }

        static House[] Build(Row[] rows, Func<string, BattlefieldKit.Module> module, int first)
        {
            var order = new List<string>(); var byHouse = new Dictionary<string, List<Row>>();
            foreach (var row in rows)
            {
                if (!byHouse.TryGetValue(row.house, out var list)) { byHouse[row.house] = list = new List<Row>(); order.Add(row.house); }
                list.Add(row);
            }
            var houses = new House[order.Count];
            for (int h = 0; h < order.Count; h++)
            {
                var list = byHouse[order[h]];
                var chunks = new Chunk[list.Count];
                for (int i = 0; i < list.Count; i++)
                {
                    var r = list[i];
                    var offset = V(r.offset); var min = V(r.min) + offset; var max = V(r.max) + offset;
                    var local = new Bounds(); local.SetMinMax(Vector3.Min(min, max), Vector3.Max(min, max));
                    chunks[i] = new Chunk { Module = module(r.chunk), House = first + h, Index = i, Offset = offset, Local = local, Timber = r.mat == "timber" };
                }
                houses[h] = new House { Name = order[h], Index = first + h, Chunks = chunks };
                Solve(houses[h]);
            }
            return houses;
        }

        static Vector3 V(float[] a) => a != null && a.Length >= 3 ? new Vector3(a[0], a[1], a[2]) : Vector3.zero;

        /// <summary>Works out the house's bounds and what rests on what, from the chunks' bounds.</summary>
        public static void Solve(House house)
        {
            var c = house.Chunks;
            if (c.Length == 0) return;
            var all = c[0].Local; float floor = float.MaxValue;
            foreach (var chunk in c) { all.Encapsulate(chunk.Local); floor = Mathf.Min(floor, chunk.Local.min.y); }
            house.Bounds = all;
            var carries = new List<int>[c.Length];
            for (int i = 0; i < c.Length; i++) carries[i] = new List<int>();
            for (int i = 0; i < c.Length; i++)
            {
                var a = c[i].Local;
                c[i].Grounded = a.min.y - floor < GroundedBelow;
                if (c[i].Grounded) continue;
                var rests = new List<int>();
                for (int j = 0; j < c.Length; j++)
                    if (j != i && Below(c[j].Local, a, RestGap)) rests.Add(j);
                // nothing within reach under it (a piece whose props were cut away): rest it on the highest thing under it
                if (rests.Count == 0)
                {
                    int best = -1; float top = float.MinValue;
                    for (int j = 0; j < c.Length; j++)
                        if (j != i && Below(c[j].Local, a, float.MaxValue) && c[j].Local.max.y > top) { top = c[j].Local.max.y; best = j; }
                    if (best >= 0) rests.Add(best); else c[i].Grounded = true;
                }
                c[i].RestsOn = rests.ToArray();
                foreach (int j in rests) carries[j].Add(i);
            }
            for (int i = 0; i < c.Length; i++) c[i].Carries = carries[i].ToArray();
        }

        /// <summary>b lies under a: it starts lower, reaches up to within gap of a's foot, and the two overlap across.</summary>
        static bool Below(Bounds b, Bounds a, float gap)
            => b.min.y < a.min.y - 0.05f && b.max.y >= a.min.y - gap
            && b.min.x < a.max.x - RestOverlap && b.max.x > a.min.x + RestOverlap
            && b.min.z < a.max.z - RestOverlap && b.max.z > a.min.z + RestOverlap;

        /// <summary>
        /// One mesh for the whole house: each chunk's mesh moved to its offset, one submesh, and the chunk's index in UV1.x
        /// on every vertex (TW/Toon's _CHUNKMASK reads it). The chunk meshes must be readable (EnvKitImport makes them so).
        /// Null if a chunk cannot be read, or the house has more chunks than a mask holds.
        /// </summary>
        public static Mesh BuildWhole(House house)
        {
            if (house.Chunks.Length == 0 || house.Chunks.Length > MaxChunks) { Debug.LogError("HouseKit: " + house.Name + " has " + house.Chunks.Length + " chunks, a mask holds " + MaxChunks); return null; }
            var parts = new CombineInstance[house.Chunks.Length];
            var ids = new List<Vector2>();
            for (int i = 0; i < house.Chunks.Length; i++)
            {
                var mesh = house.Chunks[i].Module != null ? house.Chunks[i].Module.Mesh : null;
                if (mesh == null || !mesh.isReadable) { Debug.LogError("HouseKit: chunk " + i + " of " + house.Name + " is not readable; reimport Resources/Env/Houses"); return null; }
                parts[i] = new CombineInstance { mesh = mesh, subMeshIndex = 0, transform = Matrix4x4.Translate(house.Chunks[i].Offset) };
                for (int v = 0; v < mesh.vertexCount; v++) ids.Add(new Vector2(i, 0f));
            }
            var whole = new Mesh { name = house.Name + "_Whole", hideFlags = HideFlags.HideAndDontSave };
            if (ids.Count > 65535) whole.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
            whole.CombineMeshes(parts, true, true, false);
            whole.SetUVs(1, ids);
            whole.RecalculateBounds();
            whole.UploadMeshData(true);
            return whole;
        }

        /// <summary>A chunk's drawn matrix in a house placed at <paramref name="house"/>.</summary>
        public static Matrix4x4 Place(in Matrix4x4 house, Chunk chunk) => house * Matrix4x4.Translate(chunk.Offset);
        /// <summary>The house's matrix from one of its chunks' drawn matrices.</summary>
        public static Matrix4x4 HouseOf(in Matrix4x4 drawn, Chunk chunk) => drawn * Matrix4x4.Translate(-chunk.Offset);
    }
}
