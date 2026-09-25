// Phase: A5c (implemented 2026-09-25) — the units of a match, as a table the world owns.
//
// Every per-archetype rule used to be a `switch (archetype)` compiled into the sim: what a unit costs and how fast it
// walks (RosterEntry), what it can do besides shoot (InfantrySpec), what it shoots with (CombatTables), its armour and
// guns (TankSpec), how it drives (VehicleProfile). Five switches in five files, each with a silent default — an id
// nobody had added fell through to a rifleman weapon and a Maw hull — and one new unit meant editing all five. Four
// historical armies of ten units each is not something anybody should hand-edit five times over.
//
// So a spec becomes a table indexed by archetype, built once per match. This class holds the tables whose types live
// in Core; Combat and Nav own theirs, for the same reason DirectFire cannot see Nav. Default() fills them from the
// compiled switches, which is why this commit changes no behaviour, and the bake fills them from TW.Data later.
//
// Jobs take the NativeArray as a [ReadOnly] field instead of calling the static: a Burst job cannot read a managed
// static, so passing the table in is what makes a data path possible at all.
//
// Hashing. The table is match state, not content. Two machines running one replay with different unit numbers would
// drift apart a hundred ticks in and the drift would read as a physics bug, so Fingerprint folds every byte of every
// entry, SimWorld.Hash folds the fingerprint, and a mismatch shows on the first tick instead.
using System;
using Unity.Collections;

namespace TW.Sim
{
    public sealed class UnitCatalogue : IDisposable
    {
        /// <summary>What each archetype costs to deploy and how it moves. Archetypes.Count long, indexed by archetype.</summary>
        public NativeArray<RosterEntry> Roster;

        /// <summary>What each archetype can do besides shoot: the aura, the plate, the satchel, the leap.</summary>
        public NativeArray<InfantrySpec> Infantry;

        /// <summary>One number standing for the whole table, folded into the tick hash.</summary>
        public ulong Fingerprint { get; private set; }

        UnitCatalogue() { }

        /// <summary>The table the game ships with: the compiled switches, entry by entry.</summary>
        public static UnitCatalogue Default()
        {
            var c = new UnitCatalogue
            {
                Roster = new NativeArray<RosterEntry>(Archetypes.Count, Allocator.Persistent),
                Infantry = new NativeArray<InfantrySpec>(Archetypes.Count, Allocator.Persistent),
            };
            for (int a = 0; a < Archetypes.Count; a++)
            {
                c.Roster[a] = RosterEntry.ForArchetype((byte)a);
                c.Infantry[a] = InfantrySpec.For((byte)a);
            }
            c.Seal();
            return c;
        }

        /// <summary>
        /// Compute the fingerprint, once the tables are filled and before the match runs. The arrays are hashed whole
        /// rather than entry by entry: NativeArray memory is zeroed on allocation, so the padding inside a struct is
        /// zero and identical on both machines, which a value copied onto the stack would not guarantee.
        /// </summary>
        public void Seal()
        {
            ulong h = SimHash.Value(Archetypes.Count, SimHash.Offset);
            h = SimHash.Array(Roster, h);
            h = SimHash.Array(Infantry, h);
            Fingerprint = h;
        }

        public ulong Hash(ulong h) => SimHash.Value(Fingerprint, h);

        public void Dispose()
        {
            if (Roster.IsCreated) Roster.Dispose();
            if (Infantry.IsCreated) Infantry.Dispose();
        }
    }
}
