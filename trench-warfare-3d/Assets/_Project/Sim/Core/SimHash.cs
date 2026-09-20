// Phase: P0 (implemented)
// 64-bit FNV-1a over raw bytes. Used for per-tick state hashes (desync detection) and replay verification.
// Any array that is part of authoritative state must be folded into SimWorld.Hash().
using Unity.Burst;
using Unity.Collections;
using Unity.Collections.LowLevel.Unsafe;

namespace TW.Sim
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static unsafe class SimHash
    {
        public const ulong Offset = 14695981039346656037ul;
        public const ulong Prime = 1099511628211ul;

        [BurstCompile]
        public static ulong Bytes(byte* data, int length, ulong h)
        {
            for (int i = 0; i < length; i++)
            {
                h ^= data[i];
                h *= Prime;
            }
            return h;
        }

        public static ulong Array<T>(NativeArray<T> array, ulong h) where T : unmanaged
            => Bytes((byte*)array.GetUnsafeReadOnlyPtr(), array.Length * UnsafeUtility.SizeOf<T>(), h);

        public static ulong Array<T>(NativeArray<T> array, int count, ulong h) where T : unmanaged
            => Bytes((byte*)array.GetUnsafeReadOnlyPtr(), count * UnsafeUtility.SizeOf<T>(), h);

        public static ulong Value<T>(T value, ulong h) where T : unmanaged
            => Bytes((byte*)&value, UnsafeUtility.SizeOf<T>(), h);

        public static ulong Combine(ulong a, ulong b)
        {
            ulong h = a ^ 0x9E3779B97F4A7C15ul;
            h = (h ^ b) * Prime;
            h ^= h >> 29;
            return h;
        }
    }
}
