// Phase: P0 (implemented)
// All randomness in the sim derives from (seed, tick, systemId, slot). Never cache a Random across ticks.
using Unity.Burst;
using Unity.Mathematics;

namespace TW.Sim
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class SimRandom
    {
        /// <summary>System ids keep two systems drawing in the same tick from producing correlated streams.</summary>
        public enum SystemId : uint
        {
            Deployment = 1, DirectFire = 2, IndirectFire = 3, Suppression = 4, Armor = 5,
            Bog = 6, WaveAi = 7, Mission = 8, Separation = 9,
            TrenchGarrison = 11, TrenchPost = 12,   // registered so nobody reuses them by accident
            Abilities = 13, Mines = 14, Beam = 15, Burning = 16,   // the overhaul's systems (docs/21); registered ahead of use
            Test = 1000,
        }

        [BurstCompile]
        public static uint Mix(uint seed, uint tick, uint systemId, uint slot)
        {
            // xxHash32-style avalanche on four words; cheap and platform independent.
            uint h = seed ^ 0x9E3779B1u;
            h = math.rol(h + tick * 0x85EBCA77u, 13) * 0xC2B2AE3Du;
            h = math.rol(h + systemId * 0x27D4EB2Fu, 11) * 0x165667B1u;
            h = math.rol(h + slot * 0x9E3779B1u, 17) * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12; h *= 0x297A2D39u; h ^= h >> 15;
            return h == 0u ? 1u : h; // Unity.Mathematics.Random rejects a zero state
        }

        public static Random For(uint seed, uint tick, SystemId system, uint slot = 0)
            => new Random(Mix(seed, tick, (uint)system, slot));
    }
}
