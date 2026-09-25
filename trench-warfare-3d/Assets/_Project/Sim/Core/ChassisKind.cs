// Phase: A5c (implemented 2026-09-25) — what kind of machine a unit is, as a value instead of an id range.
namespace TW.Sim
{
    /// <summary>
    /// What a unit stands, rolls or walks on. It is a field on RosterEntry, so it travels in the match table and a
    /// new machine declares it in its UnitDef; it replaced `archetype >= Pincer &amp;&amp; archetype &lt;= Redoubt`,
    /// which forced every new walker into one contiguous band of ids and quietly called anything outside it a tank.
    ///
    /// Ask the table, never the id: w.ChassisOf(archetype) from managed code, Roster[Archetype[i]].Chassis in a job.
    /// </summary>
    public static class ChassisKind
    {
        public const byte Foot = 0;      // a man
        public const byte Tracked = 1;   // the Maw, the Tusk, the Breaker: tracks to break, a belly to ditch
        public const byte Legged = 2;    // the six crabs: steps over trenches and wire, stopped by losing its legs
        public const byte Wheeled = 3;   // armoured cars (phase 6b): declared now so the kind is not a two-case enum

        /// <summary>Has armour, guns and modules: what VehicleModulesSystem and TankGunnerySystem run for.</summary>
        public static bool IsArmoured(byte chassis) => chassis != Foot;
        public static bool IsTank(byte chassis) => chassis == Tracked;
        public static bool IsWalker(byte chassis) => chassis == Legged;
    }
}
