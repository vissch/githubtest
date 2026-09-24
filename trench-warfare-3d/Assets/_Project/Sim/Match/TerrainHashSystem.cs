// Phase: A4 (2026-09-24) — depends on: MapData.Hash
// The ground is compared state now, and this is the one line that makes it so.
//
// docs/03 recorded a known hole: the heightfield, the nav layers and the cover derived from them are mutated during
// a match (craters, trees, wrecks, wire) and were NOT folded into SimWorld.Hash, so two machines could walk on
// different ground and agree on the hash. Only MapData.Version (a mutation counter) was compared, which catches a
// missing mutation but not a different one. The fix was deferred "to the next replay-format break"; buildings, blast
// direction and the holes the shells dig are that break (Replay.FormatVersion 4).
//
// SimWorld cannot call MapData.Hash itself: TW.Sim.Core does not reference TW.Sim.Terrain, and should not. A system
// can, so this is a system that steps nothing. It is registered FIRST (SimSystemOrder.TerrainHash), which costs
// nothing at run time but means BattlefieldLockstepTests.FirstDifference names "TerrainHashSystem" when it is the
// ground that has diverged, instead of blaming whichever system happens to hash next.
//
// Cost: about 105 KB of FNV-1a per HASHED tick on the standard map (the heightfield is 49 KB of it). Single player
// sets SimWorld.HashInterval = 0 and never calls Hash at all, so this is paid only by the determinism canary, the
// lockstep tests and replay verification -- exactly the places that need it.
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    public sealed class TerrainHashSystem : ISimSystem
    {
        public int Order => SimSystemOrder.TerrainHash;
        readonly MapData map;

        public TerrainHashSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) { }
        public ulong Hash(ulong h) => map.Hash(h);
        public void Dispose() { }
    }
}
