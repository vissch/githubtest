// Phase: A3 (implemented 2026-09-25) — what each faction fields.
// One row per faction: the ten roster slots a player of it deploys from (slot order is the HUD card order and the
// hotkey order), the POOL of everything it may ever field (missions and the briefing swap from it), and the off-map
// abilities it may call. RosterEntry.FillDefault forwards here (player offset 0 = Iron, otherwise Brass) so every
// caller that built a roster before factions existed still gets the same table.
//
// It was two `faction == Iron ? a : b` ternaries per question, which does not survive six factions, so the rosters
// are a table now: one flat byte array read by (faction, slot).
//
// The four historical armies stand BESIDE the fiction (owner decision 2026-09-25) and field, for the moment, only
// units that already exist — deliberately no walkers and no jetpack, because crab machines and rocket packs are the
// fiction signature and would make the British Empire read as Iron in a different hat. Their own units (the Whippet,
// the Mark V, the A7V, the FT-17, the Vickers team, the Stosstrupp, the Chasseurs) arrive as data in phase 3, and
// each one replaces a placeholder slot in this table.
using Unity.Collections;

namespace TW.Sim
{
    public static class FactionRoster
    {
        /// <summary>Roster slots 0..2 are the same in every faction: whoever holds the parapet. A test holds it.</summary>
        public const int SharedSlots = 3;

        // Read by (faction, slot). Iron and Brass are exactly the tables of 2026-09-25.
        static readonly byte[] Slots =
        {
            // Iron: a line army with an officer over it, a plate in front, an engineer behind the machines
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Repair,
            VehicleArchetype.Maw, VehicleArchetype.Pincer, VehicleArchetype.Banner, VehicleArchetype.Breaker,
            // Brass: raiders — a sniper, a medic, and a man who leaps the parapet
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Sniper, InfantryArchetype.Medic, InfantryArchetype.Jetpack,
            VehicleArchetype.Tusk, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Redoubt,
            // British Empire: industrial attrition — massed line infantry, a marksman, the medical services, armour
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Sniper, InfantryArchetype.Officer, InfantryArchetype.Medic, InfantryArchetype.Repair,
            VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker,
            // German Empire: infiltration and engineering — assault troops behind a plate, engineers on the machines
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Sniper, InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Repair,
            VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker,
            // French Republic: combined arms — a shield pushed across the open, medics well forward, cheap armour
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Sniper, InfantryArchetype.Shield, InfantryArchetype.Medic, InfantryArchetype.Officer,
            VehicleArchetype.Tusk, VehicleArchetype.Maw, VehicleArchetype.Breaker,
            // Austria-Hungary: alpine defence — marksmen and engineers holding strongpoints
            InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
            InfantryArchetype.Sniper, InfantryArchetype.Officer, InfantryArchetype.Repair, InfantryArchetype.Medic,
            VehicleArchetype.Tusk, VehicleArchetype.Maw, VehicleArchetype.Breaker,
        };

        /// <summary>The roster a player of this faction deploys from: RosterEntry.SlotCount entries from
        /// <paramref name="playerOffset"/>.</summary>
        public static void Fill(NativeArray<RosterEntry> roster, int playerOffset, FactionId faction)
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++) roster[playerOffset + s] = Slot(faction, s);
        }

        /// <summary>The entry a faction fields in a roster slot (a default entry when the slot is empty).</summary>
        public static RosterEntry Slot(FactionId faction, int slot)
        {
            if (slot < 0 || slot >= RosterEntry.SlotCount) return default;
            int f = (int)faction;
            if (f < 0 || f >= Factions.Count) return default;
            return RosterEntry.ForArchetype(Slots[f * RosterEntry.SlotCount + slot]);
        }

        // ---- the pool: everything a faction may ever put on the field, roster or drop -------------------------
        static readonly byte[][] Pools =
        {
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Repair,
                         VehicleArchetype.Maw, VehicleArchetype.Pincer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Breaker },
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Medic, InfantryArchetype.Jetpack, InfantryArchetype.Para,
                         VehicleArchetype.Tusk, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Redoubt },
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Officer, InfantryArchetype.Medic, InfantryArchetype.Repair, InfantryArchetype.Shield,
                         VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker },
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Officer, InfantryArchetype.Medic, InfantryArchetype.Repair, InfantryArchetype.Shield,
                         VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker },
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Officer, InfantryArchetype.Medic, InfantryArchetype.Repair, InfantryArchetype.Shield,
                         VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker },
            new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                         InfantryArchetype.Officer, InfantryArchetype.Medic, InfantryArchetype.Repair, InfantryArchetype.Shield,
                         VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker },
        };

        static byte[] PoolOf(FactionId faction)
        {
            int f = (int)faction;
            return f >= 0 && f < Pools.Length ? Pools[f] : Pools[0];
        }

        public static int PoolCount(FactionId faction) => PoolOf(faction).Length;
        public static byte Pool(FactionId faction, int k) => PoolOf(faction)[k];

        /// <summary>May this faction ever field the archetype (roster or drop)?</summary>
        public static bool Fields(FactionId faction, byte archetype)
        {
            var pool = PoolOf(faction);
            for (int k = 0; k < pool.Length; k++) if (pool[k] == archetype) return true;
            return false;
        }

        /// <summary>Bit per off-map ability id (TW.Sim.Match.OffMapAbilityId, which Core cannot name): the support a
        /// faction may call. Everyone shells and gasses; Brass alone drops paratroopers. The air cards of phase 4 are
        /// added per faction here.</summary>
        public const int HeBarrageBit = 1, ChlorineGasBit = 3, ParaDropBit = 10;
        public static uint AbilityMask(FactionId faction)
            => (1u << HeBarrageBit) | (1u << ChlorineGasBit) | (faction == FactionId.Brass ? 1u << ParaDropBit : 0u);
        public static bool MayCall(FactionId faction, int abilityId)
            => abilityId >= 0 && abilityId < 32 && (AbilityMask(faction) & (1u << abilityId)) != 0;
    }
}
