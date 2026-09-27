// Tools/otr.py: a binary FBX mesh reader for the stand-in engine's Resources.Load<Mesh>, following how Unity's
// importer sees this project's exports (Blender, bake_space_transform, axis_forward -Z / up Y, apply_unit_scale; the
// .meta files: useFileScale 1, bakeAxisConversion 1, globalScale): the geometry's vertices are already Y-up, Unity
// mirrors Z for its left-handed space (z -> -z, and the winding with it) and scales by UnitScaleFactor / 100 times
// globalScale. Polygons are fan-triangulated; UV0 is read when it is by polygon vertex; normals are recalculated.
// The first Geometry in the file is the mesh. Checked against Tools/housesplit.py's houses.json: every chunk's bounds
// (which are centred on its pivot in x and z, so they cannot tell a mirror in x from one in z), and the chunks put back
// at their offsets landing on the whole prop's vertices (which can: mirroring x instead of z leaves them 0.3 m out).
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;

static class FakeFbx
{
    public sealed class Node { public string Name; public List<object> Props = new List<object>(); public List<Node> Children = new List<Node>(); public Node Child(string n) => Children.FirstOrDefault(c => c.Name == n); }
    public sealed class MeshOut { public float[] Positions; public int[] Triangles; public float[] Uv; }

    public static Node Read(byte[] data)
    {
        if (data.Length < 27 || Encoding.ASCII.GetString(data, 0, 20) != "Kaydara FBX Binary  ") throw new InvalidDataException("not a binary FBX");
        uint version = BitConverter.ToUInt32(data, 23);
        bool big = version >= 7500;
        var root = new Node { Name = "" };
        int pos = 27;
        while (pos < data.Length)
        {
            var n = ReadNode(data, ref pos, big);
            if (n == null) break;
            root.Children.Add(n);
        }
        return root;
    }

    static Node ReadNode(byte[] d, ref int pos, bool big)
    {
        long end, nprops, plen;
        if (big) { end = BitConverter.ToInt64(d, pos); nprops = BitConverter.ToInt64(d, pos + 8); plen = BitConverter.ToInt64(d, pos + 16); pos += 24; }
        else { end = BitConverter.ToUInt32(d, pos); nprops = BitConverter.ToUInt32(d, pos + 4); plen = BitConverter.ToUInt32(d, pos + 8); pos += 12; }
        int nlen = d[pos]; pos += 1;
        if (end == 0) return null;
        var node = new Node { Name = Encoding.ASCII.GetString(d, pos, nlen) }; pos += nlen;
        long pend = pos + plen;
        for (long k = 0; k < nprops && pos < pend; k++)
        {
            char t = (char)d[pos]; pos++;
            switch (t)
            {
                case 'Y': node.Props.Add((double)BitConverter.ToInt16(d, pos)); pos += 2; break;
                case 'C': node.Props.Add(d[pos] != 0); pos += 1; break;
                case 'I': node.Props.Add((double)BitConverter.ToInt32(d, pos)); pos += 4; break;
                case 'F': node.Props.Add((double)BitConverter.ToSingle(d, pos)); pos += 4; break;
                case 'D': node.Props.Add(BitConverter.ToDouble(d, pos)); pos += 8; break;
                case 'L': node.Props.Add((double)BitConverter.ToInt64(d, pos)); pos += 8; break;
                case 'f': case 'd': case 'l': case 'i': case 'b':
                {
                    int n = BitConverter.ToInt32(d, pos), enc = BitConverter.ToInt32(d, pos + 4), clen = BitConverter.ToInt32(d, pos + 8); pos += 12;
                    byte[] raw;
                    if (enc == 1)
                    {
                        using (var ms = new MemoryStream(d, pos + 2, clen - 2))   // skip the zlib header
                        using (var z = new DeflateStream(ms, CompressionMode.Decompress))
                        using (var o = new MemoryStream()) { z.CopyTo(o); raw = o.ToArray(); }
                    }
                    else { raw = new byte[clen]; Buffer.BlockCopy(d, pos, raw, 0, clen); }
                    pos += clen;
                    var arr = new double[n];
                    for (int i = 0; i < n; i++)
                        arr[i] = t == 'f' ? BitConverter.ToSingle(raw, i * 4) : t == 'd' ? BitConverter.ToDouble(raw, i * 8) : t == 'l' ? BitConverter.ToInt64(raw, i * 8) : t == 'i' ? BitConverter.ToInt32(raw, i * 4) : (double)(sbyte)raw[i];
                    node.Props.Add(arr);
                    break;
                }
                case 'S': case 'R':
                {
                    int n = BitConverter.ToInt32(d, pos); pos += 4;
                    node.Props.Add(t == 'S' ? (object)Encoding.UTF8.GetString(d, pos, n) : new byte[0]);
                    pos += n; break;
                }
                default: throw new InvalidDataException("fbx: property type " + t);
            }
        }
        pos = (int)pend;
        while (pos < end - (big ? 25 : 13))
        {
            var c = ReadNode(d, ref pos, big);
            if (c == null) break;
            node.Children.Add(c);
        }
        pos = (int)end;
        return node;
    }

    static double UnitScale(Node root)
    {
        var props = root.Child("GlobalSettings")?.Child("Properties70");
        if (props == null) return 1;
        foreach (var p in props.Children)
            if (p.Props.Count >= 5 && p.Props[0] as string == "UnitScaleFactor") return Convert.ToDouble(p.Props[4]);
        return 1;
    }

    public static MeshOut Load(string path, double globalScale)
    {
        var root = Read(File.ReadAllBytes(path));
        double s = UnitScale(root) / 100.0 * globalScale;
        var geo = root.Child("Objects")?.Children.FirstOrDefault(c => c.Name == "Geometry" && c.Child("Vertices") != null);
        if (geo == null) throw new InvalidDataException("fbx: no mesh in " + path);
        var v = (double[])geo.Child("Vertices").Props[0];
        var poly = (double[])geo.Child("PolygonVertexIndex").Props[0];
        // UV0 by polygon vertex (IndexToDirect or Direct), unrolled to one vertex per polygon corner
        double[] uvData = null, uvIndex = null;
        var uvl = geo.Children.FirstOrDefault(c => c.Name == "LayerElementUV");
        if (uvl != null && (uvl.Child("MappingInformationType")?.Props[0] as string) == "ByPolygonVertex")
        {
            uvData = uvl.Child("UV")?.Props[0] as double[];
            if ((uvl.Child("ReferenceInformationType")?.Props[0] as string) == "IndexToDirect") uvIndex = uvl.Child("UVIndex")?.Props[0] as double[];
        }
        // one output vertex per polygon corner (Unity splits by UV seams anyway; bounds are what the tests read)
        var pos = new List<float>(); var uv = new List<float>(); var tris = new List<int>();
        var face = new List<int>();
        for (int c = 0; c < poly.Length; c++)
        {
            int raw = (int)poly[c];
            bool last = raw < 0;
            int vi = last ? -raw - 1 : raw;
            int outIndex = pos.Count / 3;
            pos.Add((float)(v[vi * 3] * s)); pos.Add((float)(v[vi * 3 + 1] * s)); pos.Add((float)(-v[vi * 3 + 2] * s));
            if (uvData != null)
            {
                int ui = uvIndex != null ? (int)uvIndex[c] : c;
                uv.Add((float)uvData[ui * 2]); uv.Add((float)uvData[ui * 2 + 1]);
            }
            face.Add(outIndex);
            if (last)
            {
                // fan, with the winding reversed by the Z mirror
                for (int k = 1; k + 1 < face.Count; k++) { tris.Add(face[0]); tris.Add(face[k + 1]); tris.Add(face[k]); }
                face.Clear();
            }
        }
        return new MeshOut { Positions = pos.ToArray(), Triangles = tris.ToArray(), Uv = uvData != null ? uv.ToArray() : null };
    }

    /// <summary>globalScale from the importer's .meta beside the file (1 when absent).</summary>
    public static double GlobalScale(string fbxPath)
    {
        string meta = fbxPath + ".meta";
        if (!File.Exists(meta)) return 1;
        foreach (var line in File.ReadAllLines(meta))
        {
            var t = line.Trim();
            if (t.StartsWith("globalScale:") && double.TryParse(t.Substring(12).Trim(), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out double g)) return g;
        }
        return 1;
    }
}
