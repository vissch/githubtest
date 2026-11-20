// Phase: CelFx orbital shadow lab prototype (2026-11-14). Shared caster registry.
using System;
using UnityEngine;

namespace TW.Presentation
{
    [Flags] public enum OrbitalCasterRole { None = 0, Dummy = 1, BattlefieldProps = 2 }

    // A Core bridge avoids introducing a Terrain <-> VFX assembly dependency.
    public interface IOrbitalShadowCasterSource
    {
        OrbitalCasterRole ShadowCasterRole { get; }
        void CollectOrbitalShadowCasters(OrbitalShadowCasterBuffer into, Vector3 source, float range);
    }

    public struct OrbitalShadowCaster
    {
        public Mesh Mesh;
        public Material Material;
        public Matrix4x4 Matrix;
        public Vector4 ChunkMask;
        public Bounds Bounds;
        public int Pass;
    }

    public sealed class OrbitalShadowCasterBuffer
    {
        public const int Capacity = 512;
        public readonly OrbitalShadowCaster[] Items = new OrbitalShadowCaster[Capacity];
        readonly Material[] cachedMaterials = new Material[128];
        readonly int[] cachedPasses = new int[128];
        int cachedCount;
        public int Count { get; private set; }
        public int Props { get; private set; }
        public int Dummies { get; private set; }
        public int Unsupported { get; private set; }
        public bool Overflow { get; private set; }
        public OrbitalCasterRole Sources { get; internal set; }
        Vector4 wind;
        static readonly int SwayId = Shader.PropertyToID("_Sway");
        static readonly int WindId = Shader.PropertyToID("_TWWind");

        public void Begin()
        {
            Count = Props = Dummies = Unsupported = 0;
            Overflow = false; Sources = OrbitalCasterRole.None;
            wind = Shader.GetGlobalVector(WindId);
        }

        public float SwayPadding(Mesh mesh, Material material)
        {
            if (mesh == null || material == null || !material.HasProperty(SwayId)) return 0f;
            float h = Mathf.Max(Mathf.Abs(mesh.bounds.min.y), Mathf.Abs(mesh.bounds.max.y));
            // TWSway maximum gust: .55 + .60 + .25 = 1.40. Wind uses .xy, exactly as Toon.
            return new Vector2(wind.x, wind.y).magnitude * Mathf.Max(0f, material.GetFloat(SwayId)) * h * h * 1.4f;
        }

        public static Bounds WorldBounds(Mesh mesh, in Matrix4x4 m, float padding)
        {
            Bounds local = mesh.bounds;
            Vector3 e = local.extents;
            Vector3 a = m.MultiplyVector(new Vector3(e.x, 0, 0));
            Vector3 b = m.MultiplyVector(new Vector3(0, e.y, 0));
            Vector3 c = m.MultiplyVector(new Vector3(0, 0, e.z));
            var bounds = new Bounds(m.MultiplyPoint3x4(local.center), new Vector3(
                Mathf.Abs(a.x) + Mathf.Abs(b.x) + Mathf.Abs(c.x),
                Mathf.Abs(a.y) + Mathf.Abs(b.y) + Mathf.Abs(c.y),
                Mathf.Abs(a.z) + Mathf.Abs(b.z) + Mathf.Abs(c.z)) * 2f);
            bounds.Expand(padding * 2f);
            return bounds;
        }

        int PassFor(Material material)
        {
            for (int i = 0; i < cachedCount; i++) if (cachedMaterials[i] == material) return cachedPasses[i];
            int pass = material.FindPass("OrbitalRadialDepth");
            if (cachedCount == cachedMaterials.Length) { Overflow = true; return -1; }
            cachedMaterials[cachedCount] = material; cachedPasses[cachedCount++] = pass;
            return pass;
        }

        public void Add(Mesh mesh, Material material, in Matrix4x4 matrix, Vector4 chunkMask,
            Bounds bounds, Vector3 source, float range, OrbitalCasterRole role)
        {
            if (mesh == null || material == null || bounds.SqrDistance(source) > range * range) return;
            int pass = PassFor(material);
            if (pass < 0) { Unsupported++; return; }
            if (Count == Items.Length) { Overflow = true; return; }
            Items[Count++] = new OrbitalShadowCaster { Mesh = mesh, Material = material, Matrix = matrix,
                ChunkMask = chunkMask, Bounds = bounds, Pass = pass };
            if (role == OrbitalCasterRole.Dummy) Dummies++;
            if (role == OrbitalCasterRole.BattlefieldProps) Props++;
        }
    }

    public static class OrbitalShadowCasters
    {
        static readonly IOrbitalShadowCasterSource[] sources = new IOrbitalShadowCasterSource[16];
        static int count;
        public static bool RegistrationOverflow { get; private set; }
        static OrbitalShadowCasters() => SceneStatics.Register(nameof(OrbitalShadowCasters), ResetSession);

        public static void Register(IOrbitalShadowCasterSource source)
        {
            for (int i = 0; i < count; i++) if (ReferenceEquals(sources[i], source)) return;
            if (count == sources.Length) { RegistrationOverflow = true; return; }
            sources[count++] = source;
        }
        public static void Unregister(IOrbitalShadowCasterSource source)
        {
            for (int i = 0; i < count; i++) if (ReferenceEquals(sources[i], source))
            { sources[i] = sources[--count]; sources[count] = null; return; }
        }
        public static void Collect(OrbitalShadowCasterBuffer into, Vector3 position, float range)
        {
            into.Begin();
            for (int i = 0; i < count; i++)
            {
                var source = sources[i];
                if (source is UnityEngine.Object obj && obj == null) continue;
                into.Sources |= source.ShadowCasterRole;
                source.CollectOrbitalShadowCasters(into, position, range);
            }
        }
        public static void ResetSession()
        { Array.Clear(sources, 0, sources.Length); count = 0; RegistrationOverflow = false; }
    }
}
