// Phase: A5c (implemented 2026-09-25) — the weapon and the machine each archetype is, as tables.
//
// The Core half of the unit table lives in UnitCatalogue; these two specs cannot, because WeaponStats and TankSpec are
// Combat types and Core cannot see Combat. So they hang off the smallest thing that can own them: a system with no
// work to do per tick, registered first so that every system which reads a spec can resolve it while it initialises.
// TerrainHashSystem is the precedent for a system that exists to hold and hash state rather than to step it.
//
// Filled from CombatTables.WeaponFor and TankSpec.For, so nothing changes yet; filled from TW.Data once the bake
// lands. Jobs take the NativeArray as a [ReadOnly] field, because a Burst job cannot read a managed static.
using Unity.Collections;

namespace TW.Sim.Combat
{
    public sealed class CombatCatalogueSystem : ISimSystem
    {
        /// <summary>What each archetype shoots with, indexed by archetype.</summary>
        public NativeArray<WeaponStats> Weapon;

        /// <summary>The hull, guns, claws and cycle of each machine, indexed by archetype.</summary>
        public NativeArray<TankSpec> Tank;

        /// <summary>One number standing for both tables, folded into the tick hash.</summary>
        public ulong Fingerprint { get; private set; }

        // early: everything that reads a spec resolves this while it initialises, and nothing steps before it
        public int Order => SimSystemOrder.TerrainHash - 10;

        public void Initialize(SimWorld world)
        {
            Weapon = new NativeArray<WeaponStats>(Archetypes.Count, Allocator.Persistent);
            Tank = new NativeArray<TankSpec>(Archetypes.Count, Allocator.Persistent);
            for (int a = 0; a < Archetypes.Count; a++)
            {
                Weapon[a] = CombatTables.WeaponFor((byte)a);
                Tank[a] = TankSpec.For((byte)a);
            }
            Seal();
        }

        /// <summary>Recompute the fingerprint: called once the tables are filled, before the match runs.</summary>
        public void Seal()
        {
            ulong h = SimHash.Value(Archetypes.Count, SimHash.Offset);
            h = SimHash.Array(Weapon, h);
            h = SimHash.Array(Tank, h);
            Fingerprint = h;
        }

        public void Step(SimWorld world) { }

        public ulong Hash(ulong h) => SimHash.Value(Fingerprint, h);

        public void Dispose()
        {
            if (Weapon.IsCreated) Weapon.Dispose();
            if (Tank.IsCreated) Tank.Dispose();
        }
    }
}
