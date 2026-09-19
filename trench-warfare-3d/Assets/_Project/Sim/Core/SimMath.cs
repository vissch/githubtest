// Phase: P0 (implemented)
// Platform-independent transcendental functions for the sim. Built only from +, -, *, /, sqrt (all IEEE
// correctly rounded under Burst FloatMode.Strict) so the result is bit-identical on every target CPU.
// If the Phase 0 platform gate fails anyway, replace the bodies with the Q32.32 fixed-point variants
// (SimMath.Fixed) — callers do not change.
using Unity.Burst;
using Unity.Mathematics;

namespace TW.Sim
{
    [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class SimMath
    {
        public const float Pi = 3.14159265358979f;
        public const float TwoPi = 6.28318530717959f;
        public const float HalfPi = 1.57079632679490f;

        /// <summary>Wrap an angle to [-pi, pi].</summary>
        [BurstCompile]
        public static float WrapAngle(float a)
        {
            a -= TwoPi * Floor((a + Pi) / TwoPi);
            return a;
        }

        [BurstCompile]
        public static float Floor(float x)
        {
            int i = (int)x;
            return (x < 0f && i != x) ? i - 1 : i;
        }

        /// <summary>Sine via range reduction + odd minimax polynomial (max abs error ~1e-6).</summary>
        [BurstCompile]
        public static float Sin(float x)
        {
            x = WrapAngle(x);
            // fold to [-pi/2, pi/2]
            if (x > HalfPi) x = Pi - x;
            else if (x < -HalfPi) x = -Pi - x;
            float x2 = x * x;
            // coefficients for sin on [-pi/2, pi/2]
            float p = -2.39e-8f;
            p = p * x2 + 2.7526e-6f;
            p = p * x2 - 1.98409e-4f;
            p = p * x2 + 8.3333315e-3f;
            p = p * x2 - 1.666666664e-1f;
            p = p * x2 + 1f;
            return x * p;
        }

        [BurstCompile]
        public static float Cos(float x) => Sin(x + HalfPi);

        /// <summary>Sine and cosine of one angle.</summary>
        public static float2 SinCos(float x) => new float2(Sin(x), Cos(x));

        /// <summary>atan on [-1,1] via polynomial (max abs error ~1e-5 rad).</summary>
        [BurstCompile]
        static float AtanUnit(float z)
        {
            float z2 = z * z;
            float p = 0.0208351f;
            p = p * z2 - 0.085133f;
            p = p * z2 + 0.180141f;
            p = p * z2 - 0.3302995f;
            p = p * z2 + 0.999866f;
            return z * p;
        }

        [BurstCompile]
        public static float Atan2(float y, float x)
        {
            if (x == 0f && y == 0f) return 0f;
            float ax = x < 0f ? -x : x;
            float ay = y < 0f ? -y : y;
            float a;
            if (ay <= ax)
            {
                a = AtanUnit(ay / ax);
            }
            else
            {
                a = HalfPi - AtanUnit(ax / ay);
            }
            if (x < 0f) a = Pi - a;
            return y < 0f ? -a : a;
        }

        /// <summary>IEEE sqrt is correctly rounded everywhere; exposed here so call sites stay uniform.</summary>
        public static float Sqrt(float x) => math.sqrt(x);

        public static float Length(float2 v) => Sqrt(v.x * v.x + v.y * v.y);
        public static float Length(float3 v) => Sqrt(v.x * v.x + v.y * v.y + v.z * v.z);

        public static float2 Normalize(float2 v)
        {
            float l = Length(v);
            return l > 1e-6f ? v / l : float2.zero;
        }

        /// <summary>Yaw (radians, around +Y) of a direction in the XZ plane.</summary>
        public static float YawOf(float3 dir) => Atan2(dir.x, dir.z);

        public static float3 DirFromYaw(float yaw) => new float3(Sin(yaw), 0f, Cos(yaw));

        /// <summary>Reserved for the fixed-point fallback (Phase 0 platform gate). Not implemented yet.</summary>
        public static class Fixed
        {
            public const int Shift = 32;
            public static long FromFloat(float f) => (long)(f * 4294967296.0);
            public static float ToFloat(long v) => (float)(v / 4294967296.0);
        }
    }
}
