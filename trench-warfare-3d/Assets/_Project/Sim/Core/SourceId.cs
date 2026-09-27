// Phase: A5c (implemented 2026-09-25) — who or what made an explosion, as one banded id.
//
// BlastSystem copies Impact.Source into Explosion.a, and for a year that integer was hand-partitioned across four
// files that did not mention each other: ability ids 0..11 in OffMapAbilities, 30 for a tank blowing up in
// VehicleModules, 40 + archetype for a tank shell in TankGunnery, and 60 for a naval shell in SeaLanding. The
// consequence was not a bug but a CEILING: because 40 + id had to stay under 60, no vehicle archetype could be
// numbered past 19, and by 2026-09-25 exactly one id was left. Four historical factions cannot be numbered inside it.
//
// So the space is banded here instead, with room per band for every byte an archetype can hold. Nothing in the game
// reads Explosion.a to decide anything — the VFX and audio routers switch on the event TYPE, not on its source — so
// the values themselves are free to move; they exist to be read in a log, in a test, and by whatever attributes a
// kill later. Anything new that queues an Impact names its band here rather than inventing a number.
namespace TW.Sim
{
    public static class SourceId
    {
        /// <summary>An off-map ability: the OffMapAbilityId itself, so a barrage still reads as 1 in a log.</summary>
        public static int Ability(int abilityId) => abilityId;
        public const int AbilityMax = 999;

        /// <summary>A weapon carried by a unit of this archetype: a tank's shell, a jetpack man's landing.</summary>
        public const int UnitBase = 1000;
        public static int Unit(byte archetype) => UnitBase + archetype;
        public static bool IsUnit(int source) => source >= UnitBase && source <= UnitBase + 255;
        public static byte ArchetypeOf(int source) => (byte)(source - UnitBase);

        /// <summary>A static gun the map placed (EmplacementDef.Kind), for the batteries that come later.</summary>
        public const int EmplacementBase = 1300;
        public static int Emplacement(byte kind) => EmplacementBase + kind;

        /// <summary>Nobody fired these: a hull whose ammunition went up, and the fleet's own guns off the map.</summary>
        public const int CookOff = 2000;
        public const int Ship = 2001;
    }
}
