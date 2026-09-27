// Tools/otr.py's stand-in for assets and the rendering objects code builds around meshes:
//   Resources.Load  a TextAsset is served from the project's Resources folders (Assets/**/Resources/<path>.json|txt|
//                   bytes|xml|csv); any other type (an imported mesh, a YAML asset, UXML) THROWS as "needs the
//                   engine", so a test never passes on an asset that silently came back null
//   Shader.Find     a shader object for any name that exists as a .shader file in the project (by its Shader "..."
//                   line), null otherwise, as in a build that did not include it
//   Material, MaterialPropertyBlock, Texture2D: created and registered; every other internal call of the rendering
//                   classes (Material, Shader, Texture*, RenderTexture, Graphics, ComputeShader, MaterialPropertyBlock,
//                   Cubemap) is an inert stub returning zero: drawing state only, never read back by the sim or the
//                   geometry the tests measure. A test that reads a material property back gets zero here: do not
//                   trust such a test's verdict under otr.
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;

static unsafe class FakeRender
{
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void V0();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate long L0();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate float F0();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate double D0();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate long LP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr P0();

    static readonly Dictionary<IntPtr, byte[]> texts = new Dictionary<IntPtr, byte[]>();
    static readonly List<GCHandle> pinned = new List<GCHandle>();
    static Dictionary<string, string> shaderFiles;

    public static void Install(Action<string, Delegate> reg, HashSet<string> registered, Func<IntPtr, object> obj, Func<IntPtr, string> span,
                               Func<object, IntPtr> register, Func<object, IntPtr> ptrOf, Func<object, IntPtr> ptr, string projectDir)
    {
        Action<string, Delegate> r = (n, d) => { registered.Add(n); reg(n, d); };

        // ---- TextAsset ----
        const string TA = "UnityEngine.TextAsset::";
        r(TA + "Internal_CreateInstance_Injected", new VPP((self, text) => { var o = obj(self); texts[register(o)] = Encoding.UTF8.GetBytes(span(text)); }));
        r(TA + "Internal_CreateInstanceFromBytes_Injected", new VPP((self, bytes) =>
        {
            var o = obj(self);
            IntPtr b = *(IntPtr*)bytes; int n = *(int*)((byte*)bytes + IntPtr.Size);
            var data = new byte[n]; if (n > 0) Marshal.Copy(b, data, 0, n);
            texts[register(o)] = data;
        }));
        r(TA + "get_bytes_Injected", new PP(self => ptr(texts.TryGetValue(self, out var b) ? (byte[])b.Clone() : new byte[0])));
        r(TA + "GetPreviewBytes_Injected", new PPI((self, max) => ptr(texts.TryGetValue(self, out var b) ? b.Take(max).ToArray() : new byte[0])));
        r(TA + "GetDataPtr_Injected", new PP(self =>
        {
            if (!texts.TryGetValue(self, out var b)) return IntPtr.Zero;
            var h = GCHandle.Alloc(b, GCHandleType.Pinned); pinned.Add(h); return h.AddrOfPinnedObject();
        }));
        r(TA + "GetDataSize_Injected", new LP(self => texts.TryGetValue(self, out var b) ? b.Length : 0));

        // ---- Resources.Load ----
        r("UnityEngine.ResourcesAPIInternal::Load_Injected", new PPP((pathSpan, typePtr) =>
        {
            string path = span(pathSpan); var type = (Type)obj(typePtr);
            if (type.FullName == "UnityEngine.TextAsset" || type.FullName == "UnityEngine.Object")
            {
                string file = FindResource(projectDir, path, new[] { ".json", ".txt", ".bytes", ".xml", ".csv", ".yaml", ".html", ".md" });
                if (file != null)
                {
                    var ta = Activator.CreateInstance(Type.GetType("UnityEngine.TextAsset, UnityEngine.CoreModule", true), new object[] { File.ReadAllText(file) });
                    return GCHandle.ToIntPtr(GCHandle.Alloc(ta));
                }
                if (type.FullName == "UnityEngine.TextAsset") return IntPtr.Zero;   // truly absent: Unity returns null too
            }
            // an imported mesh: read the FBX the way Unity's importer sees this project's exports (Fbx.cs)
            if (type.FullName == "UnityEngine.Mesh")
            {
                string fbx = FindResource(projectDir, path, new[] { ".fbx" });
                if (fbx == null) return IntPtr.Zero;   // truly absent: Unity returns null too
                var geo = FakeFbx.Load(fbx, FakeFbx.GlobalScale(fbx));
                var mesh = Activator.CreateInstance(type);   // its constructor registers it
                FakeObjects.FillMesh(mesh, geo.Positions, geo.Triangles, geo.Uv, Path.GetFileNameWithoutExtension(fbx));
                return GCHandle.ToIntPtr(GCHandle.Alloc(mesh));
            }
            // an image: a placeholder texture made in code (the geometry and the sim never read its pixels; a test that
            // does would read zeros here, so never trust a pixel-reading test's verdict under otr)
            if (type.FullName == "UnityEngine.Texture2D" && FindResource(projectDir, path, new[] { ".png", ".jpg", ".jpeg", ".tga", ".psd", ".exr" }) != null)
            {
                var tex = Activator.CreateInstance(type, new object[] { 4, 4 });
                return GCHandle.ToIntPtr(GCHandle.Alloc(tex));
            }
            // a ScriptableObject saved as a text .asset: create it and fill its fields from the YAML
            string asset = FindResource(projectDir, path, new[] { ".asset" });
            if (asset != null && IsScriptableObject(type))
            {
                var fields = FakeYaml.AssetFields(File.ReadAllText(asset));
                // a link to another asset ({fileID: n, guid: ...}) cannot be resolved here: say so, never leave it null
                // (a test would read the asset as absent and ignore itself, as the VAT bake test did)
                foreach (var kv in fields)
                    if (!kv.Key.StartsWith("m_") && kv.Value is Dictionary<string, object> link && link.ContainsKey("guid") && link.TryGetValue("fileID", out var fid) && fid as string != "0")
                        throw new MissingMethodException("otr engine: " + path + ".asset links another asset (" + kv.Key + ") by guid, which only the editor resolves");
                var so = Activator.CreateInstance(type, true);   // its constructor registers it
                register(so);
                FakeJson.FillFrom(so, fields);
                return GCHandle.ToIntPtr(GCHandle.Alloc(so));
            }
            throw new MissingMethodException("otr engine: Resources.Load<" + type.Name + ">(\"" + path + "\") needs the editor's asset importer");
        }));
        r("UnityEngine.ResourcesAPIInternal::FindShaderByName_Injected", new PP(nameSpan =>
        {
            string name = span(nameSpan);
            if (shaderFiles == null) shaderFiles = ScanShaders(projectDir);
            if (!shaderFiles.ContainsKey(name) && !name.StartsWith("Hidden/") && !name.StartsWith("Universal Render Pipeline/") && !name.StartsWith("Sprites/") && !name.StartsWith("UI/") && name != "Standard" && !name.StartsWith("Unlit/")) return IntPtr.Zero;
            var st = Type.GetType("UnityEngine.Shader, UnityEngine.CoreModule", true);
            var sh = Activator.CreateInstance(st, true);
            register(sh);
            return GCHandle.ToIntPtr(GCHandle.Alloc(sh));
        }));

        // ---- creation of rendering objects ----
        r("UnityEngine.Material::CreateWithShader_Injected", new VPP((self, shader) => register(obj(self))));
        r("UnityEngine.Material::CreateWithMaterial_Injected", new VPP((self, source) => register(obj(self))));
        r("UnityEngine.Material::CreateWithString", new VP(self => register(obj(self))));
        r("UnityEngine.MaterialPropertyBlock::CreateImpl", new P0(() => Marshal.AllocHGlobal(16)));
        r("UnityEngine.MaterialPropertyBlock::DestroyImpl", new VP(p => { }));
        r("UnityEngine.Texture2D::Internal_CreateEmptyImpl", new BP(self => { register(obj(self)); return true; }));
        r("UnityEngine.Texture2D::Internal_CreateImpl_Injected", new BP(self => { register(obj(self)); return true; }));

        // ---- the machine: a graphics card that supports every format ----
        r("UnityEngine.SystemInfo::SupportsTextureFormatNative", new BP(f => true));
        r("UnityEngine.SystemInfo::SupportsRenderTextureFormat", new BP(f => true));
        r("UnityEngine.SystemInfo::IsFormatSupported", new BP(f => true));

        // ---- every other rendering call: an inert stub by return type ----
        var core = Type.GetType("UnityEngine.Material, UnityEngine.CoreModule", true).Assembly;
        foreach (var tn in new[] { "Material", "Shader", "Texture", "Texture2D", "Texture3D", "Cubemap", "RenderTexture", "Graphics", "ComputeShader", "MaterialPropertyBlock", "Rendering.CommandBuffer", "Experimental.Rendering.GraphicsFormatUtility", "Texture2DArray", "CubemapArray", "SparseTexture", "Sprite" })
        {
            var t = core.GetType("UnityEngine." + tn);
            if (t == null) continue;
            foreach (var m in t.GetMethods(BindingFlags.Static | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly))
            {
                if ((m.GetMethodImplementationFlags() & MethodImplAttributes.InternalCall) == 0) continue;
                string name = t.FullName + "::" + m.Name;
                if (registered.Contains(name)) continue;
                var rt = m.ReturnType;
                var ps = m.GetParameters();
                if (m.Name.StartsWith("Internal_Create") && ps.Length > 0 && IsUnityObject(ps[0].ParameterType))
                {
                    // a texture or similar being made: register it so it counts as alive
                    Delegate c = rt == typeof(bool) ? (Delegate)new BP(self => { register(obj(self)); return true; }) : new VP(self => register(obj(self)));
                    registered.Add(name); reg(name, c);
                    continue;
                }
                if (rt == typeof(bool) && m.Name.Contains("isReadable")) { registered.Add(name); reg(name, new BP(self => true)); continue; }   // a texture made in code is readable
                if (rt != typeof(void) && Array.IndexOf(TextureTypes, tn) >= 0)
                {
                    // a texture's contents, size or format read back: the stand-in keeps no pixels, so say so, never zero
                    string what = name;
                    registered.Add(name); reg(name, new V0(() => throw new MissingMethodException("otr engine: " + what + " reads texture contents, which the stand-in does not keep")));
                    continue;
                }
                Delegate d = rt == typeof(void) ? new V0(() => { }) : rt == typeof(float) ? new F0(() => 0f) : rt == typeof(double) ? new D0(() => 0.0) : (Delegate)new L0(() => 0L);
                if (!rt.IsValueType && rt != typeof(void)) d = new L0(() => 0L);   // an object return: null
                registered.Add(name); reg(name, d);
            }
        }
    }

    static readonly string[] TextureTypes = { "Texture", "Texture2D", "Texture3D", "Cubemap", "RenderTexture", "Texture2DArray", "CubemapArray", "SparseTexture" };
    static bool IsUnityObject(Type t) { for (var b = t; b != null; b = b.BaseType) if (b.FullName == "UnityEngine.Object") return true; return false; }
    static bool IsScriptableObject(Type t) { for (var b = t; b != null; b = b.BaseType) if (b.FullName == "UnityEngine.ScriptableObject") return true; return false; }

    static string FindResource(string projectDir, string path, string[] exts)
    {
        foreach (var dir in Directory.GetDirectories(Path.Combine(projectDir, "Assets"), "Resources", SearchOption.AllDirectories))
            foreach (var e in exts)
            {
                var f = Path.Combine(dir, path.Replace('/', Path.DirectorySeparatorChar) + e);
                if (File.Exists(f)) return f;
            }
        return null;
    }

    static Dictionary<string, string> ScanShaders(string projectDir)
    {
        var d = new Dictionary<string, string>();
        foreach (var f in Directory.GetFiles(Path.Combine(projectDir, "Assets"), "*.shader", SearchOption.AllDirectories))
        {
            var m = Regex.Match(File.ReadAllText(f), "Shader\\s+\"([^\"]+)\"");
            if (m.Success) d[m.Groups[1].Value] = f;
        }
        return d;
    }
}
