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
using TW.Sim.Combat;
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
            archetype < InfantryPortraits.Length ? InfantryPortraits[archetype]
            : StandInPortrait(archetype) ?? FootName(archetype) ?? VehicleName(archetype);

        /// <summary>
        /// The Proving Ground's units (archetypes 21-36, 2026-09-28) have no portrait of their own yet: each wears the
        /// picture of the shipped unit it is nearest to, so a deploy bar that fields one shows a card and not a hole.
        /// Null for every other id. Their own portraits replace these lines one at a time.
        /// </summary>
        public static string StandInPortrait(byte archetype)
        {
            switch (archetype)
            {
                // the Brute, the Croaker, the Hopper and the Mercy wear their own (rendered off their models, 2026-09-28);
                // a machine that wears another's model (TankRenderer.StandIns) wears its picture too
                case VehicleArchetype.MarkIV: case VehicleArchetype.MarkV: return "Maw";
                case VehicleArchetype.A7V: return "Brute";
                case VehicleArchetype.RenaultFT: case VehicleArchetype.Whippet: case VehicleArchetype.Austin: return "Tusk";
                case InfantryArchetype.Frog: case InfantryArchetype.DeathBattalion: return "Rifleman";
                case InfantryArchetype.Sentry: return "MG";
                case InfantryArchetype.AtRifle: return "Sniper";
                case InfantryArchetype.Sapper: return "Engineer";
                case InfantryArchetype.Flamethrower: return "Assault";
                default: return null;
            }
        }

        /// <summary>
        /// How many unit portraits the skin has: archetypes 0..20 — four line infantry, two tanks, six walkers, the six
        /// units of 2026-09-25 that go up the line on foot, the Breaker, and the Skimmer and the Salvo (2026-09-28,
        /// fielded only from the Unit Sandbox). Held at or above every archetype either faction's roster hands out, so a
        /// new machine cannot inherit its neighbour's picture the way the IMGUI icon array once let two walkers share one.
        /// </summary>
        public const int PortraitCount = 21;

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
                // the Proving Ground's men (2026-09-28)
                case InfantryArchetype.Frog: return "Frog";
                case InfantryArchetype.Sentry: return "Sentry";
                case InfantryArchetype.AtRifle: return "AT Rifle";
                case InfantryArchetype.DeathBattalion: return "Death Bn";
                case InfantryArchetype.Sapper: return "Sapper";
                case InfantryArchetype.Flamethrower: return "Flamer";
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
                // the Proving Ground's men: numbers from UnitDefinitions (ProvingGroundTests holds them)
                case InfantryArchetype.Frog: return "Frog, prototype: a rifleman's numbers under the playground's frog";
                case InfantryArchetype.Sentry: return "Sentry, stand-in: a machine gun behind an 8 mm plate over a 60 degree arc";
                case InfantryArchetype.AtRifle: return "AT rifle, stand-in: pierces 20 mm at up to 70 m, one round in five seconds";
                case InfantryArchetype.DeathBattalion: return "Death Battalion, stand-in: a harder-hitting rifleman who never stays pinned";
                case InfantryArchetype.Sapper: return "Sapper, stand-in: carries two charges. Walks out and lays a mine or a tripwire";
                case InfantryArchetype.Flamethrower: return "Flamethrower, stand-in: a 12 m jet that sets men and ground alight, past a plate";
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
                case VehicleArchetype.Skimmer: return "Skimmer";
                case VehicleArchetype.Salvo: return "Salvo";
                // the Proving Ground's machines (2026-09-28)
                case VehicleArchetype.Brute: return "Brute";
                case VehicleArchetype.Croaker: return "Croaker";
                case VehicleArchetype.Bullfrog: return "Bullfrog";
                case VehicleArchetype.Hopper: return "Hopper";
                case VehicleArchetype.Mercy: return "Mercy";
                case VehicleArchetype.MarkIV: return "Mark IV";
                case VehicleArchetype.MarkV: return "Mark V";
                case VehicleArchetype.A7V: return "A7V";
                case VehicleArchetype.RenaultFT: return "Renault FT";
                case VehicleArchetype.Whippet: return "Whippet";
                case VehicleArchetype.Austin: return "Austin";
                default: return "Vehicle";
            }
        }

        /// <summary>
        /// What a player needs in order to choose one, not what it is made of. Every number was read back out of the
        /// sim: the walkers step over wire without slowing or breaking it (VehicleKinematics.cs), and every reach, radius
        /// and reload below is read off the machine's own spec (TankSpec.cs, UnitDefinitions.cs) when the tip is asked
        /// for, so a balance change cannot leave a tip quoting the old number (the ranges were cut on 2026-10-01). Two things
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
                case VehicleArchetype.Kettle: return $"Kettle, mortar walker: lobs shells over a parapet to {TankSpec.Kettle.Gun0.RangeMax:0} m. Blind inside {TankSpec.Kettle.Gun0.RangeMin:0} m";
                case VehicleArchetype.Censer: return "Censer, gas walker: no gun. Lays chlorine as it walks; the drum is its ammunition and its weak spot";
                case VehicleArchetype.Pavise: return $"Pavise, siege walker: a {TankSpec.Pavise.Gun0.RangeMax:0} m gun, the longest of the direct guns. Halts to fire, shielded";
                case VehicleArchetype.Banner: return $"Banner, command walker: a {TankSpec.Banner.Gun0.RangeMax:0} m gun, and a standard that steadies your men within {TankSpec.Banner.StandardRadius:0} m. Thin plate";
                case VehicleArchetype.Redoubt: return "Redoubt, blockhouse walker: no gun. 38 mm of front plate and the heaviest claws on the field";
                case VehicleArchetype.Breaker: return "Breaker, assault tank: winds up, charges a trench at 2.5x and strikes. Thin deck once it runs";
                case VehicleArchetype.Skimmer: { var w = UnitDefinitions.Skimmer.Weapon; return $"Skimmer, hovercraft: the fastest machine. {w.RangeMax:0} m {w.PenetrationMm:0} mm gun pierces light hulls. 8 mm"; }
                case VehicleArchetype.Salvo:
                {
                    var d = UnitDefinitions.Salvo;
                    return $"Salvo, rocket half-track: {d.Machine.Rockets} rockets to {d.Machine.Gun0.RangeMax:0} m, {d.Machine.Gun0.ReloadSeconds:0} s reload. A {d.Weapon.RangeMax:0} m machine gun up close";
                }
                // the Proving Ground's machines: placeholder numbers (UnitDefinitions), so the words say what to watch for
                case VehicleArchetype.Brute: return "Brute, prototype heavy tank: the Maw's guns under thicker plate. Slow";
                case VehicleArchetype.Croaker: return "Croaker, prototype biped: a turret gun and claws at 2.4 m. One leg a side";
                case VehicleArchetype.Bullfrog: return $"Bullfrog, prototype toad: twin gatlings to {UnitDefinitions.Bullfrog.Weapon.RangeMax:0} m. Hops trenches and wire";
                case VehicleArchetype.Hopper: return "Hopper, prototype gunship: strides over trenches and wire. No flight in the sim yet";
                case VehicleArchetype.Mercy: return "Mercy, prototype ambulance: unarmed. Heals the men within 10 m of its hull";
                case VehicleArchetype.MarkIV: return "Mark IV Male, stand-in: the Maw's numbers under a historical name";
                case VehicleArchetype.MarkV: return "Mark V, stand-in: a quicker heavy tank that bogs less, thinner plate";
                case VehicleArchetype.A7V: return "A7V, stand-in: 30 mm in front, one hull gun in a narrow arc. Ditches easily";
                case VehicleArchetype.RenaultFT: return "Renault FT, stand-in: the Tusk's turret gun in a small hull. Ditches in wide trenches";
                case VehicleArchetype.Whippet: return "Whippet, stand-in: a fast tank with machine guns only";
                case VehicleArchetype.Austin: return "Austin, stand-in: an armoured car with twin machine guns. Wheels bog in mud";
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
                case "Skimmer": return "Skimmer's up on the cushion. Mind the spray.";
                case "Salvo": return "Tubes loaded. Give us a grid and stand clear.";
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
        /// HUD toggle and F10 the feedback capture, so F5, F6, F7 are the only free block on the function row.
        /// </summary>
        public static string Hotkey(int slot) => slot >= 0 && slot < RosterEntry.SlotCount ? ((slot + 1) % 10).ToString() : "";

        public static string SupportHotkey(int index) => index >= 0 && index < SupportCards ? "F" + (5 + index) : "";
    }
}
