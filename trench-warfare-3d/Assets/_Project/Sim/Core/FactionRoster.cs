// Phase: A3 (implemented 2026-09-25) — what each faction fields.
// One table per faction: the roster slots a player of that faction deploys from, the POOL of every unit the faction
// may ever field (missions and the Armoury swap from it), and the off-map abilities it may call. RosterEntry.FillDefault
// forwards here (player offset 0 = Iron, otherwise Brass) so every caller that built a roster before factions
// existed still gets the same table. Owner decision 2026-09-25: units are faction-specific.
using Unity.Collections;

namespace TW.Sim
{
    public static class FactionRoster
    {
        /// <summary>The roster a player of this faction deploys from: RosterEntry.SlotCount entries from
        /// <paramref name="playerOffset"/>. Slot order is the HUD's card order and the hotkey order.</summary>
        public static void Fill(NativeArray<RosterEntry> roster, int playerOffset, FactionId faction)
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++) roster[playerOffset + s] = Slot(faction, s);
        }

        /// <summary>The entry a faction fields in a roster slot (default when the slot is empty).</summary>
        public static RosterEntry Slot(FactionId faction, int slot)
        {
            switch (slot)
            {
                case 0: return RosterEntry.Rifleman;
                case 1: return RosterEntry.Assault;
                case 2: return RosterEntry.Machinegunner;
                case 3: return RosterEntry.Sniper;
                case 4: return faction == FactionId.Iron ? RosterEntry.Maw : RosterEntry.Tusk;
                case 5: return faction == FactionId.Iron ? RosterEntry.Pincer : RosterEntry.Kettle;
                case 6: return faction == FactionId.Iron ? RosterEntry.Pavise : RosterEntry.Censer;
                case 7: return faction == FactionId.Iron ? RosterEntry.Banner : RosterEntry.Redoubt;
                default: return default;
            }
        }

        // ---- the pool: everything a faction may ever put on the field ------------------------------------------
        static readonly byte[] IronPool = { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                                            InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Repair,
                                            VehicleArchetype.Maw, VehicleArchetype.Pincer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Breaker };
        static readonly byte[] BrassPool = { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                                             InfantryArchetype.Medic, InfantryArchetype.Jetpack, InfantryArchetype.Para,
                                             VehicleArchetype.Tusk, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Redoubt };

        public static int PoolCount(FactionId faction) => (faction == FactionId.Iron ? IronPool : BrassPool).Length;
        public static byte Pool(FactionId faction, int k) => (faction == FactionId.Iron ? IronPool : BrassPool)[k];

        /// <summary>May this faction ever field the archetype (roster or drop)?</summary>
        public static bool Fields(FactionId faction, byte archetype)
        {
            var pool = faction == FactionId.Iron ? IronPool : BrassPool;
            for (int k = 0; k < pool.Length; k++) if (pool[k] == archetype) return true;
            return false;
        }

        /// <summary>Bit per off-map ability id (TW.Sim.Match.OffMapAbilityId, which Core cannot name): the support
        /// a faction may call. Both call the barrage (1) and chlorine (3); Brass alone drops paratroopers (10).</summary>
        public const int HeBarrageBit = 1, ChlorineGasBit = 3, ParaDropBit = 10;
        public static uint AbilityMask(FactionId faction)
            => (1u << HeBarrageBit) | (1u << ChlorineGasBit) | (faction == FactionId.Brass ? 1u << ParaDropBit : 0u);
        public static bool MayCall(FactionId faction, int abilityId)
            => abilityId >= 0 && abilityId < 32 && (AbilityMask(faction) & (1u << abilityId)) != 0;
    }
}
