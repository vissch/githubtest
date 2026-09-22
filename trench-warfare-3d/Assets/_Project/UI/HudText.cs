// Phase: B6 (implemented) — every word the battle HUD shows, in one pure static so tests can read it without a panel.
// Moved from BattleHud.cs (84-143) so the Toolkit HUD and the IMGUI HUD say the same things during the flag window;
// BattleHud keeps its own copy until it is deleted, and HudTextTests points here from step A6 of the UI plan.
// Names, tooltips and portraits are keyed by ARCHETYPE, never by roster slot: the infantry slots are the same on
// both sides but the machine slots are not (player 0 fields the Maw, player 1 the Tusk), and a bar indexed by slot
// draws player 0's pictures for player 1. Dormant while multiplayer is deferred; cheaper never to have than to fix.
using TW.Sim;

namespace TW.UI
{
    public static class HudText
    {
        // ---- infantry: archetype == slot for 0..3 (RosterEntry.FillDefault) ------------------------------------
        static readonly string[] InfantryNames = { "Rifle", "Assault", "MG", "Sniper" };
        static readonly string[] InfantryPortraits = { "Rifleman", "Assault", "MG", "Sniper" };
        static readonly string[] InfantryTips =
        {
            "Rifleman: cheap line infantry", "Assault: fast, short range", "MG team: holds a trench, suppresses",
            "Sniper: long range, slow fire",
        };

        /// <summary>The short name on a card: "Rifle", "Maw", …</summary>
        public static string Name(byte archetype) =>
            archetype < InfantryNames.Length ? InfantryNames[archetype] : VehicleName(archetype);

        /// <summary>What a player needs in order to choose one; the tooltip body under the name.</summary>
        public static string Tip(byte archetype) =>
            archetype < InfantryTips.Length ? InfantryTips[archetype] : VehicleTip(archetype);

        /// <summary>The portrait file stem under Assets/_Project/UI/Skin/Portraits/, per SkinSpec.PortraitNames.</summary>
        public static string PortraitName(byte archetype) =>
            archetype < InfantryPortraits.Length ? InfantryPortraits[archetype] : VehicleName(archetype);

        /// <summary>
        /// How many unit portraits the skin has: archetypes 0..11 (four infantry, two tanks, six walkers). Held at or
        /// above every archetype FillDefault hands out for either side, so a new machine cannot inherit its
        /// neighbour's picture the way the IMGUI icon array once let two walkers share one.
        /// </summary>
        public const int PortraitCount = 12;

        /// <summary>The key printed in a card's badge: roster slots 0..7 are "1".."8"; support 0 and 1 are "9" and "0".</summary>
        public static string Hotkey(int slot) => slot < 8 ? ((slot + 1) % 10).ToString() : "";
        public static string SupportHotkey(int index) => index == 0 ? "9" : index == 1 ? "0" : "";

        public static string VehicleName(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Maw: return "Maw";
                case VehicleArchetype.Tusk: return "Tusk";
                case VehicleArchetype.Pincer: return "Pincer";
                case VehicleArchetype.Kettle: return "Kettle";
                case VehicleArchetype.Censer: return "Censer";
                case VehicleArchetype.Pavise: return "Pavise";
                case VehicleArchetype.Banner: return "Banner";
                case VehicleArchetype.Redoubt: return "Redoubt";
                default: return "Vehicle";
            }
        }

        /// <summary>
        /// Every number here was read back out of the sim rather than remembered, and HudTextTests holds each one to
        /// the constant it quotes: the Kettle's RangeMin, the Pavise's RangeMax and its claim to the longest reach, the
        /// Pincer's arcs, the Banner's radius. Keep these under about 100 characters: the tooltip plate is one line.
        /// </summary>
        public static string VehicleTip(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Maw: return "Maw, heavy tank: sponson guns, crosses wide trenches, crushes wire";
                case VehicleArchetype.Tusk: return "Tusk, light tank: turret gun, quick, ditches in wide trenches";
                case VehicleArchetype.Pincer: return "Pincer, heavy walker: sponson guns that cannot reach behind it, claws at 3 m. Steps over wire";
                case VehicleArchetype.Kettle: return "Kettle, mortar walker: fires without line of sight at men behind a parapet. Blind inside 46 m";
                case VehicleArchetype.Censer: return "Censer, gas walker: no gun. Lays chlorine as it walks; the drum is its ammunition and its weak spot";
                case VehicleArchetype.Pavise: return "Pavise, siege walker: a 360 m gun, the longest reach on the field. Halts to fire, shielded in front";
                case VehicleArchetype.Banner: return "Banner, command walker: a 300 m gun, and a standard that steadies your men within 26 m. Thin plate";
                case VehicleArchetype.Redoubt: return "Redoubt, blockhouse walker: no gun. 38 mm of front plate and the heaviest claws on the field";
                default: return "Vehicle: immune to small arms, grenades within 8 m hurt it";
            }
        }

        /// <summary>The two support cards' text. 12 shells, 25 m and a 4 s delay are WarmupTicks 80 at TickRate 20;
        /// HudTextTests checks all three against OffMapAbilitySystem so they cannot go stale silently.</summary>
        public const string BarrageName = "HE Barrage";
        public const string GasName = "Chlorine";
        public const string BarrageTip = "HE barrage: 12 shells in 25 m after 4 s; craters give cover";
        public const string GasTip = "Chlorine gas: drifts with the wind, pools in trenches, drives the garrison out";

        // ---- fixed strings, uppercase because USS has no text-transform ---------------------------------------
        public const string GroupInfantry = "INFANTRY";
        public const string GroupArmour = "ARMOUR";
        public const string GroupSupport = "SUPPORT";
        public const string ObjectivesTitle = "OBJECTIVES";
        public const string Locked = "LOCKED";
        public const string Aim = "AIM";
        public const string Paused = "PAUSED";
        public const string AimHint = "Click the map to fire.  Esc or right click cancels.";
        public const string LockedTip = "Locked: reinforcements pass through to the next trench. Click to open";
        public const string OpenTip = "Open: reinforcements stop here. Click to lock";
        public const string FallbackTip = "Fall back to the trench behind";
        public const string HoldingTip = "Holding fire. Click to fire at will";
        public const string FiringTip = "Firing at will. Click to hold fire";
        public const string AdvanceTip = "Over the top: the garrison advances on the next trench";
        public const string PauseTip = "Tactical pause (Space): orders still go through";
        public const string SpeedTip = "Game speed";

        /// <summary>Centre banners, as CombatFx.cs:554/566/569 word them.</summary>
        public static string CapturedBanner(int trench, bool mine) => mine ? $"Trench {trench} captured!" : $"Trench {trench} lost";
        public const string IncomingBanner = "INCOMING BARRAGE";
        public const string VictoryBanner = "VICTORY: enemy HQ taken";
        public const string DefeatBanner = "DEFEAT: your HQ has fallen";
    }
}
