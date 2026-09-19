// Phase: A5 (stub) — depends on: MapData (4 m field grid, wind), NavLayer.Trench/Crater sinks
// Two half-precision concentration grids (gas, smoke). Per tick: advect by wind, 5-point diffusion, sink into
// trench/crater cells (×2), decay. Read by DirectFire (smoke attenuation 8 %/m above conc 10) and by this
// system for damage (6/s per 10 conc, ignores armour and cover), flee-upwind and capture blocking.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    public sealed class GasSmokeSystem : ISimSystem
    {
        public int Order => SimSystemOrder.GasSmoke;
        public NativeArray<half> Gas, Smoke;
        public int Width, Length;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: GasSmokeSystem.Step");
        public float ConcentrationAlong(float3 a, float3 b, bool smoke) => throw new System.NotImplementedException("Phase A5: GasSmokeSystem.ConcentrationAlong");
        public ulong Hash(ulong h) => h;
        public void Dispose() { if (Gas.IsCreated) Gas.Dispose(); if (Smoke.IsCreated) Smoke.Dispose(); }
    }
}
