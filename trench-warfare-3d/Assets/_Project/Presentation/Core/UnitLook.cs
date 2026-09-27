// Phase: A5b (implemented 2026-09-25) — one table for what a unit is CALLED, what it is FOR and which picture it
// wears, keyed by ARCHETYPE and never by roster slot.
//
// It lives in TW.Presentation.Core because both HUDs must read the same words and TW.Presentation.Camera (the IMGUI
// BattleHud) cannot see TW.UI: the dependency runs the other way. HudText forwards to this, the old bar forwards to
// this, and the two can no longer say different things about the same man during the flag window — they did, and the
// copies drifted the moment the roster grew.
//
// Slot is meaningless here on purpose. While both sides fielded the same eight, archetype == slot for the infantry
// and the tables were indexed by slot; since the factions split (Iron fields the officer, the shield bearer and the
// engineer in slots 3-5, Brass the sniper, the medic and the jetpack in the same three) a slot index draws Iron's
// officer for Brass's sniper. Everything below asks what the unit IS.
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    public static class UnitLook
    {
        // ---- line infantry: ids 0..3 -----------------------------------------------------------------------------
        static readonly string[] InfantryNames = { "Rifle", "Assault", "MG", "Sniper" };
        static readonly string[] InfantryPortraits = { "Rifleman", "Assault", "MG", "Sniper" };
        static readonly string[] InfantryTips =
        {
            "Rifleman: cheap line infantry", "Assault: fast, short range", "MG team: holds a trench, suppresses",
            "Sniper: long range, slow fire",
        };

        /// <summary>The short name on a card: "Rifle", "Officer", "Maw", …</summary>
        public static string Name(byte archetype) =>
            archetype < InfantryNames.Length ? InfantryNames[archetype] : FootName(archetype) ?? VehicleName(archetype);

        /// <summary>What a player needs in order to choose one; the tooltip body under the name.</summary>
        public static string Tip(byte archetype) =>
            archetype < InfantryTips.Length ? InfantryTips[archetype] : FootTip(archetype) ?? VehicleTip(archetype);

        /// <summary>The portrait file stem under Assets/_Project/UI/Skin/Portraits/, per SkinSpec.PortraitNames.</summary>
        public static string PortraitName(byte archetype) =>
            archetype < InfantryPortraits.Length ? InfantryPortraits[archetype] : FootName(archetype) ?? VehicleName(archetype);

        /// <summary>
        /// How many unit portraits the skin has: archetypes 0..18 — four line infantry, two tanks, six walkers, the six
        /// units of 2026-09-25 that go up the line on foot, and the Breaker. Held at or above every archetype either
        /// faction's roster hands out, so a new machine cannot inherit its neighbour's picture the way the IMGUI icon
        /// array once let two walkers share one.
        /// </summary>
        public const int PortraitCount = 19;

        /// <summary>The six units of 2026-09-25 that go up the line on foot; null for anything else, so machines fall through.</summary>
        static string FootName(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Officer: return "Officer";
                case InfantryArchetype.Shield: return "Shield";
                case InfantryArchetype.Medic: return "Medic";
                case InfantryArchetype.Repair: return "Engineer";
                case InfantryArchetype.Para: return "Para";
                case InfantryArchetype.Jetpack: return "Jetpack";
                default: return null;
            }
        }

        /// <summary>
        /// Every number here was read back out of the sim rather than remembered, and HudTextTests holds each one to
        /// the constant it quotes: InfantrySpec.For(id) carries the officer's 15 m ring and his 1.2x, the shield's 8 mm
        /// plate over a 60 degree arc, the medic's 8 m at 25 hp/s, the engineer's 6 m at 40 hp/s and the jetpack's 28 m
        /// leap. Keep these under about 100 characters: the tooltip plate is one line and clips rather than wraps.
        /// </summary>
        static string FootTip(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Officer: return "Officer: men within 15 m hit 1.2x harder, take half the suppression, will not stay pinned";
                case InfantryArchetype.Shield: return "Shield bearer: 8 mm plate over a 60 degree arc. Rifle fire aimed past him stops on the plate";
                case InfantryArchetype.Medic: return "Medic: unarmed. Patches the nearest wounded man within 8 m at 25 hp/s, one man at a time";
                case InfantryArchetype.Repair: return "Engineer: mends a machine within 6 m at 40 hp/s, beats out its fire, frees a jammed module";
                case InfantryArchetype.Para: return "Paratrooper: comes down behind the line on the air card, never deployed from a trench";
                case InfantryArchetype.Jetpack: return "Jetpack trooper: leaps 28 m into an enemy trench. Cannot be shot in the air; blast on landing";
                default: return null;
            }
        }

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
                case VehicleArchetype.Breaker: return "Breaker";
                default: return "Vehicle";
            }
        }

        /// <summary>
        /// What a player needs in order to choose one, not what it is made of. Every number was read back out of the
        /// sim: the walkers step over wire without slowing or breaking it (VehicleKinematics.cs), the Kettle's RangeMin
        /// really is 46 m and the Pavise's 360 m really is the longest RangeMax on the field (TankSpec.cs). Two things
        /// I had wrong before checking, which these must not repeat: the Pincer's guns are sponson mounts like the
        /// Maw's, so they cannot reach behind it; and TankSpec.Unmanned does NOT mean uncrewed — every walker carries
        /// Crew = 2 and a crew hit still calls LoseCrew, and what Unmanned buys is only that nobody bails out when it
        /// dies (VehicleModules.cs). Keep these under about 100 characters: the hint line clips rather than wraps.
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
                case VehicleArchetype.Breaker: return "Breaker, assault tank: winds up, charges a trench at 2.5x and strikes. Thin deck once it runs";
                default: return "Vehicle: immune to small arms, grenades within 8 m hurt it";
            }
        }

        /// <summary>What a unit shouts the first time it is sent up the line.</summary>
        public static string Bark(byte archetype)
        {
            switch (PortraitName(archetype))
            {
                case "Rifleman": return "Rifles up! Over the parapet we go.";
                case "Assault": return "Clear the trench! Grenades first, questions later.";
                case "MG": return "Gun's set. Nothing crosses that wire.";
                case "Sniper": return "Keep your heads down. I'll find their officers.";
                case "Officer": return "On me, and keep your dressing!";
                case "Shield": return "Get behind the plate and stay behind it.";
                case "Medic": return "Stretcher party! Who's hit?";
                case "Engineer": return "Spanners out. Let's see what's left of her.";
                case "Para": return "Out of the harness. Which way's their line?";
                case "Jetpack": return "Fuel's hot. I'll be over their parapet before they look up.";
                case "Pincer": return "Claws out. Pincer is walking.";
                case "Kettle": return "Kettle's on! Mortar ready to brew.";
                case "Maw": return "MAW HUNGRY. MAW CRUSH WIRE.";
                case "Breaker": return "BREAKER WINDING UP. CLEAR THE PARAPET.";
                default: return "Moving up.";
            }
        }

        // ---- the support cards ----------------------------------------------------------------------------------
        /// <summary>12 shells, 25 m and a 4 s delay are WarmupTicks 80 at TickRate 20; HudTextTests checks all three
        /// against OffMapAbilitySystem so they cannot go stale silently. The drop's 8 men are AbilityStats.Men.</summary>
        public const string BarrageName = "HE Barrage", GasName = "Chlorine", DropName = "Paratroopers";
        public const string BarrageCard = "BARRAGE", GasCard = "GAS", DropCard = "DROP";   // what fits a card's nameplate
        public const string BarrageTip = "HE barrage: 12 shells in 25 m after 4 s; craters give cover";
        public const string GasTip = "Chlorine gas: drifts with the wind, pools in trenches, drives the garrison out";
        public const string DropTip = "Paratroopers: 8 men onto open ground you pick, well clear of their rear line";

        /// <summary>How many support cards the bar has: barrage, gas, paratroopers.</summary>
        public const int SupportCards = 3;

        /// <summary>Which support card an ability sits on, and so which key it takes; -1 if it has no card.</summary>
        public static int SupportIndex(OffMapAbilityId id) =>
            id == OffMapAbilityId.HeBarrage ? 0 : id == OffMapAbilityId.ChlorineGas ? 1 : id == OffMapAbilityId.ParaDrop ? 2 : -1;

        public static string SupportName(OffMapAbilityId id) =>
            id == OffMapAbilityId.ChlorineGas ? GasName : id == OffMapAbilityId.ParaDrop ? DropName : BarrageName;
        public static string SupportCardLabel(OffMapAbilityId id) =>
            id == OffMapAbilityId.ChlorineGas ? GasCard : id == OffMapAbilityId.ParaDrop ? DropCard : BarrageCard;
        public static string SupportTip(OffMapAbilityId id) =>
            id == OffMapAbilityId.ChlorineGas ? GasTip : id == OffMapAbilityId.ParaDrop ? DropTip : BarrageTip;
        public static string SupportPortrait(OffMapAbilityId id) =>
            id == OffMapAbilityId.ChlorineGas ? "ChlorineGas" : id == OffMapAbilityId.ParaDrop ? "ParaDrop" : "HeBarrage";

        // ---- the keys ------------------------------------------------------------------------------------------
        /// <summary>
        /// The key printed in a card's badge. Ten roster slots take the whole digit row ("1".."9", then "0" for the
        /// tenth), which is why the support cards moved off 9 and 0 onto F5-F7: F1-F4 are the debug overlays, F9 the
        /// HUD toggle and F10 the debug panel, so F5, F6, F7 are the only free block on the function row.
        /// </summary>
        public static string Hotkey(int slot) => slot >= 0 && slot < RosterEntry.SlotCount ? ((slot + 1) % 10).ToString() : "";

        public static string SupportHotkey(int index) => index >= 0 && index < SupportCards ? "F" + (5 + index) : "";
    }
}
