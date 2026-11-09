// Phase: CelFx kit (2026-11-09) - the record every cel primitive is drawn from, and the small value types recipes are
// written in. A record is written once when it is spawned and never touched again: the kind's vertex shader works out
// where it is from the record and the clock (Shaders/Cel/CelFx.hlsl holds the same layout and the field meanings).
using System;
using System.Runtime.InteropServices;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>One primitive on the GPU: 128 bytes, the layout of CelRecord in CelFx.hlsl. What each Vector4 means is
    /// the kind's (each shader's header); the common fields are the same for every kind.</summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct CelRecord
    {
        /// <summary>Where it was spawned (world) and when (CelFx.Now seconds; a time in the future hides it until then).</summary>
        public Vector3 Pos; public float Born;
        /// <summary>Its speed at birth (m/s) and how long it lives (s).</summary>
        public Vector3 Vel; public float Life;
        /// <summary>A direction of its own (a spin axis, a beam's end offset) and a seed 0..1.</summary>
        public Vector3 Axis; public float Seed;
        /// <summary>x size at birth (m), y at death, z the ease power, w kind-specific (a streak's trail seconds, a lobe's squash).</summary>
        public Vector4 Size;
        /// <summary>x gravity (m/s2 down), y drag (1/s), z spin (rad/s), w buoyancy (m/s2 up).</summary>
        public Vector4 Motion;
        /// <summary>x erosion at birth, y at death, z the ease power, w feather.</summary>
        public Vector4 Erode;
        /// <summary>x palette row (+ 256 x the row it cools to), y HDR core multiplier, z opacity, w ink width (screen px).</summary>
        public Vector4 Tone;
        /// <summary>Kind-specific.</summary>
        public Vector4 Extra;

        public const int Bytes = 128;

        /// <summary>The palette field: a row, or a row that cools into another over the life.</summary>
        public static float PackPalette(int row, int coolTo = -1) => row + 256 * (coolTo < 0 ? row : coolTo);
    }

    /// <summary>A value over a primitive's life: From at birth, To at death, eased by k^Pow (0 or 1 = linear).</summary>
    [Serializable]
    public struct CelCurve
    {
        public float From, To, Pow;
        public CelCurve(float from, float to, float pow = 1f) { From = from; To = to; Pow = pow; }
        public static CelCurve Const(float v) => new CelCurve(v, v, 1f);
        public float At(float k) => Mathf.Lerp(From, To, Mathf.Pow(Mathf.Clamp01(k), Pow > 0f ? Pow : 1f));
        /// <summary>The packed (from, to, pow) the shader reads, with w left for the caller.</summary>
        public Vector4 Pack(float w) => new Vector4(From, To, Pow > 0f ? Pow : 1f, w);
    }

    /// <summary>A small deterministic generator (xorshift), seeded from a number, so an effect plays the same way twice.</summary>
    public struct CelRng
    {
        uint s;
        public CelRng(uint seed) { s = FxQuality.Hash(seed ^ 0x9E3779B9u); if (s == 0) s = 0x1234567u; }
        public float Next() { s ^= s << 13; s ^= s >> 17; s ^= s << 5; return (s & 0xFFFFFF) / (float)0x1000000; }
        public float Range(float a, float b) => a + (b - a) * Next();
        /// <summary>v x (1 +- jitter).</summary>
        public float Jitter(float v, float jitter) => v * (1f + Range(-jitter, jitter));
        public Vector3 OnSphere()
        {
            float z = Range(-1f, 1f), a = Range(0f, Mathf.PI * 2f), r = Mathf.Sqrt(Mathf.Max(0f, 1f - z * z));
            return new Vector3(Mathf.Cos(a) * r, z, Mathf.Sin(a) * r);
        }
        public Vector3 InSphere() => OnSphere() * Mathf.Pow(Next(), 1f / 3f);
        public Vector2 InDisc() { float a = Range(0f, Mathf.PI * 2f), r = Mathf.Sqrt(Next()); return new Vector2(Mathf.Cos(a) * r, Mathf.Sin(a) * r); }
        /// <summary>A direction within `degrees` of the frame's forward (uniform over the cap).</summary>
        public Vector3 InCone(Vector3 forward, Vector3 right, Vector3 up, float degrees)
        {
            float cosMax = Mathf.Cos(Mathf.Clamp(degrees, 0f, 180f) * Mathf.Deg2Rad);
            float z = Range(cosMax, 1f), a = Range(0f, Mathf.PI * 2f), r = Mathf.Sqrt(Mathf.Max(0f, 1f - z * z));
            return forward * z + right * (Mathf.Cos(a) * r) + up * (Mathf.Sin(a) * r);
        }
    }
}
