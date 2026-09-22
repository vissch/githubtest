// Phase: C1 (implemented) — the atlas on disk and in memory. A baked atlas of a hundred clips is 4,000 frames by a
// thousand vertices; as Unity texture assets in a text-serialised project that is 150 MB of YAML per bake, so the
// baker writes one gzip'd .bytes file instead: positions quantised to 16 bits over the bounds (0.04 mm steps), normals
// to 8 bits, one row entry per clip (start frame, frame count, loop or hold, seconds). At load it becomes an RGBA64
// position texture and an RGBA32 normal texture; the shader dequantises with _PosMin / _PosSize. The box soldier goes
// through the same quantiser at runtime so there is one shader path.
using System;
using System.IO;
using System.IO.Compression;
using Unity.Collections;
using UnityEngine;

namespace TW.Presentation.Units
{
    public sealed class VatAsset
    {
        public Mesh Mesh;
        public Texture2D Positions, Normals;
        /// <summary>x = first frame, y = frame count; y negative = play once and hold the last frame.</summary>
        public Vector2[] RowTable;
        /// <summary>Seconds each row plays over (its authored length; loops wrap at it).</summary>
        public float[] RowSeconds;
        public Vector3 PosMin, PosSize;
        public int TotalFrames;
        /// <summary>A clip atlas has one row per controller Clip; the 18-row atlas is indexed by AnimRow.</summary>
        public bool ClipAtlas => RowTable != null && RowTable.Length >= (int)Clip.Count;
        public int Frames(int row) => Mathf.Max(1, (int)Mathf.Abs(RowTable[row].y));
        public bool Loops(int row) => RowTable[row].y > 0f;
    }

    public static class VatCodec
    {
        const string Magic = "TWVAT2";

        /// <summary>Quantise frames into the two textures. frames[f][v] in metres, normals unit length.</summary>
        public static void Textures(Vector3[][] frames, Vector3[][] normals, int vertexCount, string name, out Texture2D pos, out Texture2D nrm, out Vector3 min, out Vector3 size)
        {
            int total = frames.Length;
            min = new Vector3(float.MaxValue, float.MaxValue, float.MaxValue); var max = -min;
            for (int f = 0; f < total; f++) for (int i = 0; i < vertexCount; i++) { min = Vector3.Min(min, frames[f][i]); max = Vector3.Max(max, frames[f][i]); }
            size = Vector3.Max(max - min, new Vector3(1e-3f, 1e-3f, 1e-3f));
            var p = new NativeArray<ushort>(vertexCount * total * 4, Allocator.Temp);
            var n = new NativeArray<byte>(vertexCount * total * 4, Allocator.Temp);
            for (int f = 0; f < total; f++)
                for (int i = 0; i < vertexCount; i++)
                {
                    int k = (f * vertexCount + i) * 4;
                    Vector3 q = frames[f][i] - min;
                    p[k] = (ushort)Mathf.RoundToInt(Mathf.Clamp01(q.x / size.x) * 65535f); p[k + 1] = (ushort)Mathf.RoundToInt(Mathf.Clamp01(q.y / size.y) * 65535f);
                    p[k + 2] = (ushort)Mathf.RoundToInt(Mathf.Clamp01(q.z / size.z) * 65535f); p[k + 3] = 65535;
                    Vector3 d = normals[f][i];
                    n[k] = (byte)Mathf.RoundToInt(Mathf.Clamp01(d.x * 0.5f + 0.5f) * 255f); n[k + 1] = (byte)Mathf.RoundToInt(Mathf.Clamp01(d.y * 0.5f + 0.5f) * 255f);
                    n[k + 2] = (byte)Mathf.RoundToInt(Mathf.Clamp01(d.z * 0.5f + 0.5f) * 255f); n[k + 3] = 255;
                }
            pos = new Texture2D(vertexCount, total, TextureFormat.RGBA64, false, true) { name = name + "Positions", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            pos.SetPixelData(p, 0); pos.Apply(false, true);
            nrm = new Texture2D(vertexCount, total, TextureFormat.RGBA32, false, true) { name = name + "Normals", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            nrm.SetPixelData(n, 0); nrm.Apply(false, true);
            p.Dispose(); n.Dispose();
        }

        /// <summary>The baker's output: everything but the mesh, gzip'd.</summary>
        public static byte[] Encode(Vector3[][] frames, Vector3[][] normals, int vertexCount, Vector2[] rowTable, float[] rowSeconds)
        {
            int total = frames.Length;
            var min = new Vector3(float.MaxValue, float.MaxValue, float.MaxValue); var max = -min;
            for (int f = 0; f < total; f++) for (int i = 0; i < vertexCount; i++) { min = Vector3.Min(min, frames[f][i]); max = Vector3.Max(max, frames[f][i]); }
            var size = Vector3.Max(max - min, new Vector3(1e-3f, 1e-3f, 1e-3f));
            using (var ms = new MemoryStream())
            {
                using (var gz = new GZipStream(ms, System.IO.Compression.CompressionLevel.Optimal, true))
                using (var w = new BinaryWriter(gz))
                {
                    w.Write(Magic); w.Write(vertexCount); w.Write(total); w.Write(rowTable.Length);
                    for (int r = 0; r < rowTable.Length; r++) { w.Write((int)rowTable[r].x); w.Write((int)rowTable[r].y); w.Write(rowSeconds[r]); }
                    w.Write(min.x); w.Write(min.y); w.Write(min.z); w.Write(size.x); w.Write(size.y); w.Write(size.z);
                    for (int f = 0; f < total; f++)
                        for (int i = 0; i < vertexCount; i++)
                        {
                            Vector3 q = frames[f][i] - min;
                            w.Write((ushort)Mathf.RoundToInt(Mathf.Clamp01(q.x / size.x) * 65535f)); w.Write((ushort)Mathf.RoundToInt(Mathf.Clamp01(q.y / size.y) * 65535f)); w.Write((ushort)Mathf.RoundToInt(Mathf.Clamp01(q.z / size.z) * 65535f));
                        }
                    for (int f = 0; f < total; f++)
                        for (int i = 0; i < vertexCount; i++)
                        {
                            Vector3 d = normals[f][i];
                            w.Write((byte)Mathf.RoundToInt(Mathf.Clamp01(d.x * 0.5f + 0.5f) * 255f)); w.Write((byte)Mathf.RoundToInt(Mathf.Clamp01(d.y * 0.5f + 0.5f) * 255f)); w.Write((byte)Mathf.RoundToInt(Mathf.Clamp01(d.z * 0.5f + 0.5f) * 255f));
                        }
                }
                return ms.ToArray();
            }
        }

        /// <summary>Read a baked atlas; the mesh is supplied by the asset that carries the bytes.</summary>
        public static VatAsset Decode(byte[] bytes, Mesh mesh, string name, bool keepReadable = false)
        {
            using (var ms = new MemoryStream(bytes))
            using (var gz = new GZipStream(ms, CompressionMode.Decompress))
            using (var r = new BinaryReader(gz))
            {
                if (r.ReadString() != Magic) throw new InvalidDataException("not a TW VAT atlas");
                int vertexCount = r.ReadInt32(), total = r.ReadInt32(), rows = r.ReadInt32();
                var table = new Vector2[rows]; var seconds = new float[rows];
                for (int k = 0; k < rows; k++) { int start = r.ReadInt32(), count = r.ReadInt32(); seconds[k] = r.ReadSingle(); table[k] = new Vector2(start, count); }
                var min = new Vector3(r.ReadSingle(), r.ReadSingle(), r.ReadSingle()); var size = new Vector3(r.ReadSingle(), r.ReadSingle(), r.ReadSingle());
                var p = new NativeArray<ushort>(vertexCount * total * 4, Allocator.Temp);
                var n = new NativeArray<byte>(vertexCount * total * 4, Allocator.Temp);
                var buf = new byte[vertexCount * 6];
                for (int f = 0; f < total; f++)
                {
                    Fill(r, buf, buf.Length);
                    for (int i = 0; i < vertexCount; i++)
                    {
                        int k = (f * vertexCount + i) * 4, b = i * 6;
                        p[k] = (ushort)(buf[b] | (buf[b + 1] << 8)); p[k + 1] = (ushort)(buf[b + 2] | (buf[b + 3] << 8)); p[k + 2] = (ushort)(buf[b + 4] | (buf[b + 5] << 8)); p[k + 3] = 65535;
                    }
                }
                for (int f = 0; f < total; f++)
                {
                    Fill(r, buf, vertexCount * 3);
                    for (int i = 0; i < vertexCount; i++) { int k = (f * vertexCount + i) * 4, b = i * 3; n[k] = buf[b]; n[k + 1] = buf[b + 1]; n[k + 2] = buf[b + 2]; n[k + 3] = 255; }
                }
                var pos = new Texture2D(vertexCount, total, TextureFormat.RGBA64, false, true) { name = name + "Positions", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                pos.SetPixelData(p, 0); pos.Apply(false, !keepReadable);
                var nrm = new Texture2D(vertexCount, total, TextureFormat.RGBA32, false, true) { name = name + "Normals", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                nrm.SetPixelData(n, 0); nrm.Apply(false, !keepReadable);
                p.Dispose(); n.Dispose();
                return new VatAsset { Mesh = mesh, Positions = pos, Normals = nrm, RowTable = table, RowSeconds = seconds, PosMin = min, PosSize = size, TotalFrames = total };
            }
        }

        static void Fill(BinaryReader r, byte[] buf, int count)
        {
            int got = 0;
            while (got < count) { int k = r.Read(buf, got, count - got); if (k <= 0) throw new EndOfStreamException("VAT atlas truncated"); got += k; }
        }
    }
}
