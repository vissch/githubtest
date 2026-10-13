// Stand-ins for the otr files Render.cs names, so Render.cs compiles on its own (no Unity dlls, no Mono engine).
// Only the members Render.cs calls are here; the [O12]/[O13] cases drive its extracted statics, not Install.
using System;
using System.Collections.Generic;

static class FakeYaml
{
    public static Dictionary<string, object> AssetFields(string text) => new Dictionary<string, object>();
}

static class FakeJson
{
    public static void FillFrom(object o, Dictionary<string, object> tree) { }
}

static class FakeFbx
{
    public sealed class MeshOut { public float[] Positions; public int[] Triangles; public float[] Uv; }
    public static MeshOut Load(string path, double globalScale) => new MeshOut();
    public static double GlobalScale(string fbxPath) => 1.0;
}

static class FakeObjects
{
    internal static void FillMesh(object mesh, float[] positions, int[] triangles, float[] uv, string name) { }
    internal static IntPtr RegisterObject(object o) => IntPtr.Zero;
}
