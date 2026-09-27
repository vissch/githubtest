// Tools/otr.py's stand-in for UnityEngine.Object lifetimes and Mesh, so code that builds procedural meshes (the
// battlefield kit, house kit, the strategic map, trench variants) runs with no editor and tests can read real bounds.
//   objects    Internal_Create gives an object a fake native block (its instance id at the offset the managed side
//              asks for) and a registry entry; names, hide flags, instance-id lookups; DestroyImmediate frees it;
//              Destroy outside play logs Unity's own error ("Destroy may not be called from edit mode"), so a test that
//              calls it fails here as it does in the editor; ScriptableObject.CreateInstance works
//   meshes     vertex channels (position, normal, tangent, colour, eight UV sets) in any float/unorm8 layout,
//              sub-meshes, index format, bounds (recalculated on vertices and on SetTriangles as Unity does),
//              RecalculateNormals (area-weighted by shared index), Clear, CombineMeshes (matrices, winding flip on a
//              mirror, merged or one sub-mesh an instance)
//   built-ins  Resources.GetBuiltinResource<Mesh>: Cube, Quad, Plane exactly as Unity builds them; Cylinder, Sphere
//              and Capsule with Unity's sizes and bounds but not its vertex counts (never assert those here)
// Not here: GPU buffers, blend shapes, bone weights, textures, materials, renderers, GameObjects.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;

static unsafe class FakeObjects
{
    sealed class Entry { public object Managed; public string Name = ""; public int Id; public int HideFlags; public MeshData Mesh; }
    sealed class Channel { public int Format, Dim; public byte[] Data; public int ElementSize => Dim * FormatSize(Format); }
    sealed class Sub { public int Topology; public int[] Indices = new int[0]; }
    sealed class MeshData
    {
        public int VertexCount;
        public readonly Dictionary<int, Channel> Channels = new Dictionary<int, Channel>();
        public List<Sub> Subs = new List<Sub> { new Sub() };
        public int IndexFormat;
        public float[] Bounds = new float[6];   // centre xyz, extents xyz
    }

    static readonly Dictionary<IntPtr, Entry> byPtr = new Dictionary<IntPtr, Entry>();
    static readonly Dictionary<int, IntPtr> byId = new Dictionary<int, IntPtr>();
    static int nextId = 2;
    const int IdOffset = 8;
    static FieldInfo cachedPtr, instanceId;

    static int FormatSize(int f) => f == 0 ? 4 : f == 1 ? 2 : (f == 2 || f == 3 || f == 6 || f == 7) ? 1 : (f == 4 || f == 5 || f == 8 || f == 9) ? 2 : 4;
    // VertexAttributeFormat: Float32 0, Float16 1, UNorm8 2, SNorm8 3, UNorm16 4, SNorm16 5, UInt8 6, SInt8 7, UInt16 8, SInt16 9, UInt32 10, SInt32 11

    /// <summary>An imported mesh's geometry into a registered Mesh (normals recalculated, bounds from the vertices).</summary>
    internal static void FillMesh(object mesh, float[] positions, int[] triangles, float[] uv, string name)
    {
        var p = (IntPtr)cachedPtr.GetValue(mesh);
        var e = byPtr[p]; var m = e.Mesh;
        e.Name = name;
        m.VertexCount = positions.Length / 3;
        m.Channels.Clear();
        m.Channels[0] = FromFloats(positions, 3);
        if (uv != null && uv.Length == m.VertexCount * 2) m.Channels[4] = FromFloats(uv, 2);
        m.Subs = new List<Sub> { new Sub { Indices = triangles } };
        m.IndexFormat = m.VertexCount > 65535 ? 1 : 0;
        Normals(m);
        Recalc(m);
    }

    internal static IntPtr RegisterObject(object o)
    {
        if (cachedPtr != null && cachedPtr.GetValue(o) is IntPtr existing && existing != IntPtr.Zero && byPtr.ContainsKey(existing)) return existing;
        return Register(o);
    }

    static IntPtr Register(object o)
    {
        var objType = o.GetType();
        while (objType != null && objType.FullName != "UnityEngine.Object") objType = objType.BaseType;
        if (cachedPtr == null)
        {
            cachedPtr = objType.GetField("m_CachedPtr", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            instanceId = objType.GetField("m_InstanceID", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
        }
        int id = nextId; nextId += 2;
        IntPtr block = Marshal.AllocHGlobal(64);
        for (int i = 0; i < 64; i++) ((byte*)block)[i] = 0;
        *(int*)((byte*)block + IdOffset) = id;
        var e = new Entry { Managed = o, Id = id };
        if (o.GetType().FullName == "UnityEngine.Mesh") e.Mesh = new MeshData();
        byPtr[block] = e; byId[id] = block;
        cachedPtr.SetValue(o, block);
        instanceId.SetValue(o, id);
        return block;
    }

    static Entry E(IntPtr self) => self != IntPtr.Zero && byPtr.TryGetValue(self, out var e) ? e : throw new NullReferenceException("otr objects: a destroyed or unknown object");
    static MeshData M(IntPtr self) => E(self).Mesh ?? throw new InvalidOperationException("otr objects: not a mesh");

    static void Kill(IntPtr p)
    {
        if (p == IntPtr.Zero || !byPtr.TryGetValue(p, out var e)) return;
        byPtr.Remove(p); byId.Remove(e.Id);
        cachedPtr.SetValue(e.Managed, IntPtr.Zero);
    }

    static IntPtr Handle(object o) => o == null ? IntPtr.Zero : GCHandle.ToIntPtr(GCHandle.Alloc(o));

    // ---- delegate shapes ----
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPF(IntPtr a, float b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPB(IntPtr a, [MarshalAs(UnmanagedType.U1)] bool b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate uint UPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int I();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool B();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BI(int a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PI(int a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPB(IntPtr a, [MarshalAs(UnmanagedType.U1)] bool b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PIPP(int a, IntPtr b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPIII(IntPtr self, int channel, int format, int dim);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIIIPIIII(IntPtr self, int channel, int format, int dim, IntPtr values, int arraySize, int start, int count, int flags);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIIIPIIBI(IntPtr self, int submesh, int topology, int fmt, IntPtr indices, int start, int size, [MarshalAs(UnmanagedType.U1)] bool calcBounds, int baseVertex);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPBBB(IntPtr self, IntPtr combine, [MarshalAs(UnmanagedType.U1)] bool merge, [MarshalAs(UnmanagedType.U1)] bool useMatrices, [MarshalAs(UnmanagedType.U1)] bool lightmap);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIBP(IntPtr self, int submesh, [MarshalAs(UnmanagedType.U1)] bool applyBase, IntPtr ret);

    public static void Install(Action<string, Delegate> reg, Func<IntPtr, object> obj, Func<IntPtr, string> span, Action<IntPtr, string> outString, Action<int, string> log, Func<bool> isPlaying)
    {
        const string O = "UnityEngine.Object::";
        reg(O + "CurrentThreadIsMainThread", new B(() => true));
        reg(O + "GetOffsetOfInstanceIDInCPlusPlusObject", new I(() => IdOffset));
        reg(O + "DoesObjectWithInstanceIDExist", new BI(id => byId.ContainsKey(id)));
        reg(O + "FindObjectFromInstanceID_Injected", new PI(id => byId.TryGetValue(id, out var p) ? Handle(byPtr[p].Managed) : IntPtr.Zero));
        reg(O + "ForceLoadFromInstanceID_Injected", new PI(id => byId.TryGetValue(id, out var p) ? Handle(byPtr[p].Managed) : IntPtr.Zero));
        reg("UnityEngine.Resources::InstanceIDToObject_Injected", new PI(id => byId.TryGetValue(id, out var p) ? Handle(byPtr[p].Managed) : IntPtr.Zero));
        reg("UnityEngine.Resources::InstanceIDIsValid", new BI(id => byId.ContainsKey(id)));
        reg(O + "DestroyImmediate_Injected", new VPB((p, assets) => Kill(p)));
        reg(O + "Destroy_Injected", new VPF((p, t) =>
        {
            if (!isPlaying()) { log(0, "Destroy may not be called from edit mode! Use DestroyImmediate instead.\nDestroying an object in edit mode destroys it permanently."); return; }
            Kill(p);
        }));
        reg(O + "DontDestroyOnLoad_Injected", new VP(p => { }));
        reg(O + "GetName_Injected", new VPP((p, r) => outString(r, E(p).Name)));
        reg(O + "SetName_Injected", new VPP((p, n) => E(p).Name = span(n)));
        reg(O + "get_hideFlags_Injected", new IP(p => E(p).HideFlags));
        reg(O + "set_hideFlags_Injected", new VPI((p, v) => E(p).HideFlags = v));
        reg(O + "IsPersistent_Injected", new BP(p => false));
        reg(O + "MarkDirty_Injected", new VP(p => { }));
        reg(O + "ToString_Injected", new VPP((p, r) => { var e = E(p); outString(r, e.Name + " (" + e.Managed.GetType().FullName + ")"); }));

        reg("UnityEngine.Mesh::Internal_Create", new VP(m => Register(obj(m))));
        reg("UnityEngine.ScriptableObject::CreateScriptableObject", new VP(m => Register(obj(m))));
        reg("UnityEngine.ScriptableObject::CreateScriptableObjectInstanceFromType", new PPB((t, apply) =>
        {
            var type = (Type)obj(t);
            var so = Activator.CreateInstance(type, true);   // its constructor calls CreateScriptableObject, which registers it
            return Handle(so);
        }));

        const string MS = "UnityEngine.Mesh::";
        reg(MS + "get_vertexCount_Injected", new IP(s => M(s).VertexCount));
        reg(MS + "get_subMeshCount_Injected", new IP(s => M(s).Subs.Count));
        reg(MS + "set_subMeshCount_Injected", new VPI((s, n) =>
        {
            var m = M(s);
            while (m.Subs.Count < n) m.Subs.Add(new Sub());
            if (m.Subs.Count > n) m.Subs.RemoveRange(n, m.Subs.Count - n);
        }));
        reg(MS + "get_indexFormat_Injected", new IP(s => M(s).IndexFormat));
        reg(MS + "set_indexFormat_Injected", new VPI((s, f) => M(s).IndexFormat = f));
        reg(MS + "get_bounds_Injected", new VPP((s, r) => { var b = M(s).Bounds; for (int i = 0; i < 6; i++) ((float*)r)[i] = b[i]; }));
        reg(MS + "set_bounds_Injected", new VPP((s, v) => { var b = M(s).Bounds; for (int i = 0; i < 6; i++) b[i] = ((float*)v)[i]; }));
        reg(MS + "RecalculateBoundsImpl_Injected", new VPI((s, f) => Recalc(M(s))));
        reg(MS + "RecalculateNormalsImpl_Injected", new VPI((s, f) => Normals(M(s))));
        reg(MS + "RecalculateTangentsImpl_Injected", new VPI((s, f) => { }));
        reg(MS + "MarkDynamicImpl_Injected", new VP(s => { }));
        reg(MS + "UploadMeshDataImpl_Injected", new VPB((s, b) => { }));
        reg(MS + "ClearImpl_Injected", new VPB((s, keep) =>
        {
            var m = M(s);
            m.VertexCount = 0; m.Channels.Clear(); m.Subs = new List<Sub> { new Sub() }; m.Bounds = new float[6];
        }));
        reg(MS + "HasVertexAttribute_Injected", new BPI((s, a) => M(s).Channels.ContainsKey(a)));
        reg(MS + "GetVertexAttributeDimension_Injected", new IPI((s, a) => M(s).Channels.TryGetValue(a, out var c) ? c.Dim : 0));
        reg(MS + "GetVertexAttributeFormat_Injected", new IPI((s, a) => M(s).Channels.TryGetValue(a, out var c) ? c.Format : 0));
        reg(MS + "GetIndexCountImpl_Injected", new UPI((s, sub) => (uint)M(s).Subs[sub].Indices.Length));
        reg(MS + "GetTrianglesCountImpl_Injected", new UPI((s, sub) => (uint)M(s).Subs[sub].Indices.Length));
        reg(MS + "GetIndexStartImpl_Injected", new UPI((s, sub) => { var m = M(s); uint n = 0; for (int i = 0; i < sub; i++) n += (uint)m.Subs[i].Indices.Length; return n; }));
        reg(MS + "GetBaseVertexImpl_Injected", new UPI((s, sub) => 0));
        reg(MS + "GetTotalIndexCount_Injected", new IP(s => M(s).Subs.Sum(x => x.Indices.Length)));
        reg(MS + "PrintErrorCantAccessChannel_Injected", new VPI((s, ch) => log(0, "Mesh.channel " + ch + " is out of bounds")));
        reg(MS + "get_isReadable_Injected", new BP(s => true));
        reg(MS + "get_canAccess_Injected", new BP(s => true));

        reg(MS + "SetArrayForChannelImpl_Injected", new VPIIIPIIII((s, channel, format, dim, values, arraySize, start, count, flags) =>
        {
            var m = M(s);
            var arr = (Array)obj(values);
            var c = new Channel { Format = format, Dim = dim };
            int es = c.ElementSize;
            c.Data = new byte[count * es];
            if (count > 0)
            {
                var h = GCHandle.Alloc(arr, GCHandleType.Pinned);
                try { Marshal.Copy(IntPtr.Add(h.AddrOfPinnedObject(), start * es), c.Data, 0, c.Data.Length); }
                finally { h.Free(); }
            }
            if (channel == 0)
            {
                m.VertexCount = count;
                m.Channels[0] = c;
                // other channels of another length are dropped, as Unity resizes them
                foreach (var k in m.Channels.Keys.ToList()) if (k != 0 && m.Channels[k].Data.Length / m.Channels[k].ElementSize != count) m.Channels.Remove(k);
                if ((flags & 8) == 0) Recalc(m);
            }
            else
            {
                if (count != m.VertexCount) { log(0, "Mesh.SetArrayForChannel: the supplied vertex array has less vertices than are referenced by the triangles array."); return; }
                m.Channels[channel] = c;
            }
        }));
        reg(MS + "GetAllocArrayFromChannelImpl_Injected", new PPIII((s, channel, format, dim) =>
        {
            var m = M(s);
            Type et = ElementType(channel, format, dim);
            int n = m.VertexCount;
            var arr = Array.CreateInstance(et, m.Channels.ContainsKey(channel) ? n : 0);
            if (arr.Length > 0) Fill(arr, m.Channels[channel], format, dim);
            return FakeEngineAccess.Ptr(arr);
        }));
        reg(MS + "SetIndicesImpl_Injected", new VPIIIPIIBI((s, submesh, topology, fmt, indices, start, size, calc, baseVertex) =>
        {
            var m = M(s);
            var arr = (Array)obj(indices);
            var ix = new int[size];
            for (int i = 0; i < size; i++) ix[i] = System.Convert.ToInt32(arr.GetValue(start + i)) + baseVertex;
            // submesh -1: mesh.triangles = ..., the whole mesh as one sub-mesh
            if (submesh < 0) { m.Subs = new List<Sub> { new Sub() }; submesh = 0; }
            while (m.Subs.Count <= submesh) m.Subs.Add(new Sub());
            foreach (int v in ix) if (v < 0 || v >= m.VertexCount) { log(0, "Failed setting triangles. Some indices are referencing out of bounds vertices. IndexCount: " + size + ", VertexCount: " + m.VertexCount); return; }
            m.Subs[submesh] = new Sub { Topology = topology, Indices = ix };
            if (calc) Recalc(m);
        }));
        // submesh -1: every sub-mesh's indices, in order (mesh.triangles)
        reg(MS + "GetTrianglesImpl_Injected", new VPIBP((s, sub, applyBase, ret) => OutArray(ret, sub < 0 ? M(s).Subs.SelectMany(x => x.Indices).ToArray() : M(s).Subs[sub].Indices)));
        reg(MS + "GetIndicesImpl_Injected", new VPIBP((s, sub, applyBase, ret) => OutArray(ret, sub < 0 ? M(s).Subs.SelectMany(x => x.Indices).ToArray() : M(s).Subs[sub].Indices)));
        reg(MS + "CombineMeshesImpl_Injected", new VPPBBB((s, combine, merge, useMatrices, lightmap) => Combine(M(s), combine, merge, useMatrices)));

        reg("UnityEngine.Resources::GetBuiltinResource_Injected", new PPP((t, path) =>
        {
            var type = (Type)obj(t);
            string name = span(path);
            if (type.FullName != "UnityEngine.Mesh") return IntPtr.Zero;
            var mesh = Activator.CreateInstance(type);
            var m = M((IntPtr)cachedPtr.GetValue(mesh));
            if (!Builtin(m, name)) return IntPtr.Zero;
            byPtr[(IntPtr)cachedPtr.GetValue(mesh)].Name = name.Replace(".fbx", "");
            return Handle(mesh);
        }));
    }

    // BlittableArrayWrapper for an int[] return: {void* data; int size; UpdateFlags} filled with a pinned managed array
    static void OutArray(IntPtr ret, int[] values)
    {
        var copy = (int[])values.Clone();
        var h = GCHandle.Alloc(copy, GCHandleType.Pinned);
        FakeEngineAccess.Keep(h);
        *(IntPtr*)ret = h.AddrOfPinnedObject();
        *(int*)((byte*)ret + IntPtr.Size) = copy.Length;
        *(int*)((byte*)ret + IntPtr.Size + 4) = 1;   // UpdateFlags: the data is a new managed array
    }

    static Type U(string name) => Type.GetType("UnityEngine." + name + ", UnityEngine.CoreModule", true);
    static Type ElementType(int channel, int format, int dim)
    {
        if (channel == 3) return format == 2 ? U("Color32") : U("Color");
        if (dim == 2) return U("Vector2");
        if (dim == 3) return U("Vector3");
        return U("Vector4");
    }

    static float Read(Channel c, int v, int k)
    {
        if (k >= c.Dim) return k == 3 ? 1f : 0f;
        int o = v * c.ElementSize + k * FormatSize(c.Format);
        switch (c.Format)
        {
            case 0: return BitConverter.ToSingle(c.Data, o);
            case 2: return c.Data[o] / 255f;
            case 6: return c.Data[o];
            default: return BitConverter.ToSingle(c.Data, o);
        }
    }

    static void Fill(Array arr, Channel c, int format, int dim)
    {
        int n = arr.Length;
        var h = GCHandle.Alloc(arr, GCHandleType.Pinned);
        try
        {
            byte* dst = (byte*)h.AddrOfPinnedObject();
            for (int v = 0; v < n; v++)
                for (int k = 0; k < dim; k++)
                {
                    float x = Read(c, v, k);
                    if (format == 2) dst[v * dim + k] = (byte)Math.Round(Math.Min(1f, Math.Max(0f, x)) * 255f);
                    else ((float*)dst)[v * dim + k] = x;
                }
        }
        finally { h.Free(); }
    }

    static void Recalc(MeshData m)
    {
        if (!m.Channels.TryGetValue(0, out var p) || m.VertexCount == 0) { m.Bounds = new float[6]; return; }
        float[] lo = { float.MaxValue, float.MaxValue, float.MaxValue }, hi = { float.MinValue, float.MinValue, float.MinValue };
        for (int v = 0; v < m.VertexCount; v++)
            for (int k = 0; k < 3; k++) { float x = Read(p, v, k); if (x < lo[k]) lo[k] = x; if (x > hi[k]) hi[k] = x; }
        for (int k = 0; k < 3; k++) { m.Bounds[k] = (lo[k] + hi[k]) * 0.5f; m.Bounds[3 + k] = (hi[k] - lo[k]) * 0.5f; }
    }

    static float[] Positions(MeshData m)
    {
        var r = new float[m.VertexCount * 3];
        if (m.Channels.TryGetValue(0, out var p)) for (int v = 0; v < m.VertexCount; v++) for (int k = 0; k < 3; k++) r[v * 3 + k] = Read(p, v, k);
        return r;
    }

    static Channel FromFloats(float[] data, int dim)
    {
        var c = new Channel { Format = 0, Dim = dim, Data = new byte[data.Length * 4] };
        Buffer.BlockCopy(data, 0, c.Data, 0, c.Data.Length);
        return c;
    }

    static void Normals(MeshData m)
    {
        var pos = Positions(m);
        var n = new float[m.VertexCount * 3];
        foreach (var s in m.Subs)
            for (int t = 0; t + 2 < s.Indices.Length; t += 3)
            {
                int a = s.Indices[t], b = s.Indices[t + 1], c = s.Indices[t + 2];
                float ux = pos[b * 3] - pos[a * 3], uy = pos[b * 3 + 1] - pos[a * 3 + 1], uz = pos[b * 3 + 2] - pos[a * 3 + 2];
                float vx = pos[c * 3] - pos[a * 3], vy = pos[c * 3 + 1] - pos[a * 3 + 1], vz = pos[c * 3 + 2] - pos[a * 3 + 2];
                float nx = uy * vz - uz * vy, ny = uz * vx - ux * vz, nz = ux * vy - uy * vx;
                foreach (int i in new[] { a, b, c }) { n[i * 3] += nx; n[i * 3 + 1] += ny; n[i * 3 + 2] += nz; }
            }
        for (int v = 0; v < m.VertexCount; v++)
        {
            float l = (float)Math.Sqrt(n[v * 3] * n[v * 3] + n[v * 3 + 1] * n[v * 3 + 1] + n[v * 3 + 2] * n[v * 3 + 2]);
            if (l > 1e-12f) { n[v * 3] /= l; n[v * 3 + 1] /= l; n[v * 3 + 2] /= l; }
        }
        m.Channels[1] = FromFloats(n, 3);
    }

    static int offMesh = -1, offSub, offMatrix, ciSize;
    static void Combine(MeshData dst, IntPtr combine, bool merge, bool useMatrices)
    {
        if (offMesh < 0)
        {
            var ci = U("CombineInstance");
            offMesh = (int)Marshal.OffsetOf(ci, "m_MeshInstanceID"); offSub = (int)Marshal.OffsetOf(ci, "m_SubMeshIndex");
            offMatrix = (int)Marshal.OffsetOf(ci, "m_Transform"); ciSize = Marshal.SizeOf(ci);
        }
        byte* begin = *(byte**)combine; int count = *(int*)((byte*)combine + IntPtr.Size);
        var pos = new List<float>(); var nrm = new List<float>(); var col = new List<float>();
        var uvs = new Dictionary<int, List<float>>();
        var subs = new List<Sub>();
        var merged = new List<int>();
        bool anyNormals = false, anyColors = false;
        var uvDims = new Dictionary<int, int>();
        var sources = new List<(MeshData m, int sub, float[] mat)>();
        for (int i = 0; i < count; i++)
        {
            byte* e = begin + i * ciSize;
            int id = *(int*)(e + offMesh);
            if (!byId.TryGetValue(id, out var ptr) || byPtr[ptr].Mesh == null) continue;
            var mat = new float[16];
            for (int k = 0; k < 16; k++) mat[k] = ((float*)(e + offMatrix))[k];   // column-major m00 m10 m20 m30 m01 ...
            var src = byPtr[ptr].Mesh;
            sources.Add((src, *(int*)(e + offSub), mat));
            anyNormals |= src.Channels.ContainsKey(1); anyColors |= src.Channels.ContainsKey(3);
            for (int ch = 4; ch <= 11; ch++) if (src.Channels.TryGetValue(ch, out var uc)) uvDims[ch] = Math.Max(uvDims.TryGetValue(ch, out var d) ? d : 0, uc.Dim);
        }
        foreach (var (src, sub, mat) in sources)
        {
            int baseV = pos.Count / 3;
            float M(int r, int c) => useMatrices ? mat[c * 4 + r] : (r == c ? 1f : 0f);
            float det = M(0, 0) * (M(1, 1) * M(2, 2) - M(1, 2) * M(2, 1)) - M(0, 1) * (M(1, 0) * M(2, 2) - M(1, 2) * M(2, 0)) + M(0, 2) * (M(1, 0) * M(2, 1) - M(1, 1) * M(2, 0));
            // normals by the inverse transpose of the 3x3 (cofactor matrix; scale is normalised away)
            float[,] cof = {
                { M(1,1)*M(2,2)-M(1,2)*M(2,1), -(M(1,0)*M(2,2)-M(1,2)*M(2,0)), M(1,0)*M(2,1)-M(1,1)*M(2,0) },
                { -(M(0,1)*M(2,2)-M(0,2)*M(2,1)), M(0,0)*M(2,2)-M(0,2)*M(2,0), -(M(0,0)*M(2,1)-M(0,1)*M(2,0)) },
                { M(0,1)*M(1,2)-M(0,2)*M(1,1), -(M(0,0)*M(1,2)-M(0,2)*M(1,0)), M(0,0)*M(1,1)-M(0,1)*M(1,0) } };
            src.Channels.TryGetValue(0, out var p); src.Channels.TryGetValue(1, out var nc); src.Channels.TryGetValue(3, out var cc);
            for (int v = 0; v < src.VertexCount; v++)
            {
                float x = Read(p, v, 0), y = Read(p, v, 1), z = Read(p, v, 2);
                for (int r = 0; r < 3; r++) pos.Add(M(r, 0) * x + M(r, 1) * y + M(r, 2) * z + M(r, 3));
                if (anyNormals)
                {
                    if (nc != null)
                    {
                        float a = Read(nc, v, 0), b = Read(nc, v, 1), c = Read(nc, v, 2);
                        float nx = cof[0, 0] * a + cof[0, 1] * b + cof[0, 2] * c, ny = cof[1, 0] * a + cof[1, 1] * b + cof[1, 2] * c, nz = cof[2, 0] * a + cof[2, 1] * b + cof[2, 2] * c;
                        float l = (float)Math.Sqrt(nx * nx + ny * ny + nz * nz); if (l > 1e-12f) { nx /= l; ny /= l; nz /= l; }
                        nrm.Add(nx); nrm.Add(ny); nrm.Add(nz);
                    }
                    else { nrm.Add(0); nrm.Add(0); nrm.Add(0); }
                }
                if (anyColors) for (int k = 0; k < 4; k++) col.Add(cc != null ? Read(cc, v, k) : 1f);
                foreach (var kv in uvDims)
                {
                    if (!uvs.TryGetValue(kv.Key, out var l)) uvs[kv.Key] = l = new List<float>();
                    src.Channels.TryGetValue(kv.Key, out var uc);
                    for (int k = 0; k < kv.Value; k++) l.Add(uc != null ? Read(uc, v, k) : 0f);
                }
            }
            var chosen = sub >= 0 && sub < src.Subs.Count ? src.Subs[sub] : new Sub();
            var ix = new int[chosen.Indices.Length];
            for (int t = 0; t < ix.Length; t++) ix[t] = chosen.Indices[t] + baseV;
            if (det < 0) for (int t = 0; t + 2 < ix.Length; t += 3) { int tmp = ix[t + 1]; ix[t + 1] = ix[t + 2]; ix[t + 2] = tmp; }
            if (merge) merged.AddRange(ix); else subs.Add(new Sub { Topology = chosen.Topology, Indices = ix });
        }
        dst.Channels.Clear();
        dst.VertexCount = pos.Count / 3;
        dst.Channels[0] = FromFloats(pos.ToArray(), 3);
        if (anyNormals) dst.Channels[1] = FromFloats(nrm.ToArray(), 3);
        if (anyColors) dst.Channels[3] = FromFloats(col.ToArray(), 4);
        foreach (var kv in uvs) dst.Channels[kv.Key] = FromFloats(kv.Value.ToArray(), uvDims[kv.Key]);
        dst.Subs = merge ? new List<Sub> { new Sub { Indices = merged.ToArray() } } : (subs.Count > 0 ? subs : new List<Sub> { new Sub() });
        if (dst.VertexCount > 65535) dst.IndexFormat = 1;
        Recalc(dst);
    }

    // ---- built-in meshes ----
    static void Set(MeshData m, List<float> p, List<float> n, List<float> uv, List<int> ix)
    {
        m.VertexCount = p.Count / 3;
        m.Channels.Clear();
        m.Channels[0] = FromFloats(p.ToArray(), 3);
        m.Channels[1] = FromFloats(n.ToArray(), 3);
        m.Channels[4] = FromFloats(uv.ToArray(), 2);
        m.Subs = new List<Sub> { new Sub { Indices = ix.ToArray() } };
        Recalc(m);
    }

    static bool Builtin(MeshData m, string name)
    {
        var p = new List<float>(); var n = new List<float>(); var uv = new List<float>(); var ix = new List<int>();
        void V(float x, float y, float z, float nx, float ny, float nz, float u, float w) { p.Add(x); p.Add(y); p.Add(z); n.Add(nx); n.Add(ny); n.Add(nz); uv.Add(u); uv.Add(w); }
        switch (name)
        {
            case "Cube.fbx":
            {
                // six faces, four vertices each, outward normals, 0.5 half extents
                float[][] f = {
                    new float[] { 0, 0, 1 }, new float[] { 0, 0, -1 }, new float[] { 0, 1, 0 }, new float[] { 0, -1, 0 }, new float[] { 1, 0, 0 }, new float[] { -1, 0, 0 } };
                foreach (var d in f)
                {
                    float[] a = Math.Abs(d[1]) > 0 ? new float[] { 1, 0, 0 } : new float[] { 0, 1, 0 };
                    float[] b = { d[1] * a[2] - d[2] * a[1], d[2] * a[0] - d[0] * a[2], d[0] * a[1] - d[1] * a[0] };
                    int s = p.Count / 3;
                    for (int k = 0; k < 4; k++)
                    {
                        float su = (k == 1 || k == 2) ? 0.5f : -0.5f, sv = k >= 2 ? 0.5f : -0.5f;
                        V(d[0] * 0.5f + a[0] * su + b[0] * sv, d[1] * 0.5f + a[1] * su + b[1] * sv, d[2] * 0.5f + a[2] * su + b[2] * sv, d[0], d[1], d[2], su + 0.5f, sv + 0.5f);
                    }
                    ix.AddRange(new[] { s, s + 2, s + 1, s, s + 3, s + 2 });
                }
                break;
            }
            case "Quad.fbx":
                V(-0.5f, -0.5f, 0, 0, 0, -1, 0, 0); V(0.5f, -0.5f, 0, 0, 0, -1, 1, 0); V(-0.5f, 0.5f, 0, 0, 0, -1, 0, 1); V(0.5f, 0.5f, 0, 0, 0, -1, 1, 1);
                ix.AddRange(new[] { 0, 3, 1, 3, 0, 2 });
                break;
            case "Plane.fbx":
                for (int z = 0; z <= 10; z++) for (int x = 0; x <= 10; x++) V(5 - x, 0, 5 - z, 0, 1, 0, x / 10f, z / 10f);
                for (int z = 0; z < 10; z++) for (int x = 0; x < 10; x++) { int a = z * 11 + x; ix.AddRange(new[] { a, a + 1, a + 11, a + 1, a + 12, a + 11 }); }
                break;
            case "Cylinder.fbx":
            case "Capsule.fbx":
            case "Sphere.fbx":
            {
                // latitude/longitude solids with Unity's sizes: sphere r 0.5; cylinder r 0.5, y -1..1 with caps; capsule r 0.5, y -1..1
                const int seg = 24, rings = 12;
                if (name == "Cylinder.fbx")
                {
                    for (int k = 0; k <= seg; k++)
                    {
                        double a = 2 * Math.PI * k / seg; float cx = (float)Math.Sin(a) * 0.5f, cz = (float)Math.Cos(a) * 0.5f;
                        V(cx, -1, cz, cx * 2, 0, cz * 2, k / (float)seg, 0); V(cx, 1, cz, cx * 2, 0, cz * 2, k / (float)seg, 1);
                    }
                    for (int k = 0; k < seg; k++) { int a = k * 2; ix.AddRange(new[] { a, a + 1, a + 2, a + 1, a + 3, a + 2 }); }
                    foreach (int side in new[] { -1, 1 })
                    {
                        int c0 = p.Count / 3; V(0, side, 0, 0, side, 0, 0.5f, 0.5f);
                        for (int k = 0; k <= seg; k++) { double a = 2 * Math.PI * k / seg; V((float)Math.Sin(a) * 0.5f, side, (float)Math.Cos(a) * 0.5f, 0, side, 0, 0.5f + (float)Math.Sin(a) * 0.5f, 0.5f + (float)Math.Cos(a) * 0.5f); }
                        for (int k = 0; k < seg; k++) if (side > 0) ix.AddRange(new[] { c0, c0 + 1 + k, c0 + 2 + k }); else ix.AddRange(new[] { c0, c0 + 2 + k, c0 + 1 + k });
                    }
                    break;
                }
                float half = name == "Capsule.fbx" ? 0.5f : 0f;   // the capsule's straight part
                for (int r = 0; r <= rings; r++)
                {
                    double lat = Math.PI * r / rings - Math.PI / 2; float y = (float)Math.Sin(lat) * 0.5f, rad = (float)Math.Cos(lat) * 0.5f;
                    float off = half == 0 ? 0 : (r <= rings / 2 ? -half : half);
                    for (int k = 0; k <= seg; k++)
                    {
                        double a = 2 * Math.PI * k / seg; float x = (float)Math.Sin(a) * rad, z = (float)Math.Cos(a) * rad;
                        V(x, y + off, z, x * 2, y * 2, z * 2, k / (float)seg, r / (float)rings);
                    }
                    if (half > 0 && r == rings / 2)
                        for (int k = 0; k <= seg; k++) { double a = 2 * Math.PI * k / seg; float x = (float)Math.Sin(a) * rad, z = (float)Math.Cos(a) * rad; V(x, half, z, x * 2, 0, z * 2, k / (float)seg, 0.5f); }
                }
                int rows = p.Count / 3 / (seg + 1);
                for (int r = 0; r < rows - 1; r++) for (int k = 0; k < seg; k++) { int a = r * (seg + 1) + k, b = a + seg + 1; ix.AddRange(new[] { a, b, a + 1, a + 1, b, b + 1 }); }
                break;
            }
            default: return false;
        }
        Set(m, p, n, uv, ix);
        return true;
    }
}
