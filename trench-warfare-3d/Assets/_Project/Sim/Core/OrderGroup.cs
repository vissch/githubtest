// Phase: A5c (implemented 2026-09-25) — the handful of groups an order may name, and how many archetype ids exist.
//
// TrenchSelectAdvance used to carry a mask of `1 << archetype` in an int, which quietly made 31 the highest id any
// unit could ever have: past that the shift falls off the end and the man silently ignores every advance order. That
// was tolerable while there were nineteen units and impossible with four historical factions on the way, and it was
// the wrong key anyway — a player ordering "send the specialists over" does not mean "send ids 12, 14 and 15", and a
// German officer and a British one should answer the same order without the HUD knowing either id.
//
// So an order names GROUPS. Ids are then free to run to Archetypes.Count, and every table that is indexed by an
// archetype is sized by that one constant instead of by a number somebody remembered.
//
// The guns (2026-09-29, replay v22): machine gunners were Line, so no order could send the riflemen over and keep the
// guns on the parapet to cover them, the first thing an attack in this war did. They are their own group now. (The
// HUD's category chips had been sending `1 << archetype` all along, which this reads as groups: a player who picked his
// riflemen sent the guns too, and one who picked his gunners sent the officers and medics. The chips' side is SHOW.)
namespace TW.Sim
{
    /// <summary>What an order may ask for. A mask of these travels in SimCommand.B, so there is room for 32.</summary>
    public static class OrderGroup
    {
        public const int Line = 1 << 0;        // the rifle and the assault man: whoever goes over with the bayonet
        public const int Marksman = 1 << 1;    // snipers and anyone else whose job is one shot at a time
        public const int Support = 1 << 2;      // officer, medic, engineer, shield bearer: the men who keep the others up
        public const int Raider = 1 << 3;       // paratroopers, jetpack men: they arrive behind the line, not over the parapet
        public const int Gun = 1 << 4;          // machine gunners: they cover an advance from the parapet
        public const int All = Line | Marksman | Support | Raider | Gun;

        /// <summary>
        /// Which group a man belongs to. A switch for now; it becomes a UnitSpec field when the data path lands, and
        /// no system should read the archetype id for any other purpose.
        /// </summary>
        public static int Of(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Rifle:
                case InfantryArchetype.Assault: return Line;
                case InfantryArchetype.Machinegunner: return Gun;
                case InfantryArchetype.Sniper: return Marksman;
                case InfantryArchetype.Officer:
                case InfantryArchetype.Shield:
                case InfantryArchetype.Medic:
                case InfantryArchetype.Repair: return Support;
                case InfantryArchetype.Para:
                case InfantryArchetype.Jetpack: return Raider;
                default: return Line;   // an unknown man holds the parapet with everyone else rather than ignoring orders
            }
        }
    }

    /// <summary>
    /// How many archetype ids exist, and so how long every table indexed by one is: icons, portraits, commentary
    /// one-shots, vehicle meshes. Sixty-four leaves room for four historical factions on top of the fiction and keeps
    /// those tables small enough to allocate without thinking. The id itself is a byte, so this may grow to 256 if a
    /// faction ever needs it; nothing masks by id any more (see OrderGroup).
    /// </summary>
    public static class Archetypes
    {
        public const int Count = 64;
        public const byte Max = Count - 1;
    }
}
