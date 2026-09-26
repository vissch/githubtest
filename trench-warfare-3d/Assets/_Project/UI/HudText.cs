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
        public static string SupportHotkey(int index) => index == 0 ? "9" : index == 1 ? "0" : index == 2 ? "C" : index == 3 ? "M" : index == 4 ? "V" : index == 5 ? "B" : "";

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
        public const string BarrageCard = "BARRAGE", GasCard = "GAS";   // what fits a card's nameplate
        public const string SilverGaugeTip = "Silver to spend on units and support; the figure beside it is income per second";
        public const string MenGaugeTip = "Your men on the field, and the enemy's beside VS";
        public const string TimeGaugeTip = "Time since the battle began; PAUSED shows here while the clock is stopped";
        public const string BarrageTip = "HE barrage: 12 shells in 25 m after 4 s; craters give cover. Tab: line or box";
        public const string GasTip = "Chlorine gas: drifts with the wind, pools in trenches, drives the garrison out. Tab: creeping";
        public const string CreepingName = "Creeping Barrage", SmokeName = "Smoke Screen", StrafeName = "Strafe Run", BeamName = "Beam";
        public const string CreepingCard = "CREEPING", SmokeCard = "SMOKE", StrafeCard = "STRAFE", BeamCard = "BEAM";
        public const string CreepingTip = "Creeping barrage: drag its advance; ten lifts of 4 shells walk 60 m; your men 15 m behind are safe";
        public const string SmokeTip = "Smoke screen: drag a 40 m line; 30 s of smoke that hides men from fire and spoils aim through it";
        public const string StrafeTip = "Strafe run: drag the corridor; one low pass lays 32 bursts along 80 m; men on the line die";
        public const string BeamTip = "Beam: drag the corridor; a 6 s sweep burns everything on 60 m, men catch fire, hulls lose armour";

        /// <summary>A support card's words: the full name (the tooltip's title), what fits the nameplate, the tip, and the
        /// portrait it shows (the four new abilities borrow the two baked portraits until the skin bakes their own).</summary>
        public readonly struct SupportText
        {
            public readonly string Name, Card, Tip, Portrait;
            public SupportText(string name, string card, string tip, string portrait) { Name = name; Card = card; Tip = tip; Portrait = portrait; }
        }

        public static SupportText Support(TW.Sim.Match.OffMapAbilityId id)
        {
            switch (id)
            {
                case TW.Sim.Match.OffMapAbilityId.ChlorineGas: return new SupportText(GasName, GasCard, GasTip, "ChlorineGas");
                case TW.Sim.Match.OffMapAbilityId.CreepingBarrage: return new SupportText(CreepingName, CreepingCard, CreepingTip, "HeBarrage");
                case TW.Sim.Match.OffMapAbilityId.SmokeScreen: return new SupportText(SmokeName, SmokeCard, SmokeTip, "ChlorineGas");
                case TW.Sim.Match.OffMapAbilityId.StrafeRun: return new SupportText(StrafeName, StrafeCard, StrafeTip, "HeBarrage");
                case TW.Sim.Match.OffMapAbilityId.Beam: return new SupportText(BeamName, BeamCard, BeamTip, "HeBarrage");
                default: return new SupportText(BarrageName, BarrageCard, BarrageTip, "HeBarrage");
            }
        }

        // ---- fixed strings, uppercase because USS has no text-transform ---------------------------------------
        public const string GroupInfantry = "INFANTRY";
        public const string GroupArmour = "ARMOUR";
        public const string GroupSupport = "SUPPORT";
        public const string ObjectivesTitle = "OBJECTIVES";
        public const string Locked = "LOCKED";
        public const string Aim = "AIM";
        public const string Paused = "PAUSED";
        public const string AimHint = "Click the map to fire.  Tab changes the pattern.  Esc or right click cancels.";
        public const string AimLineHint = "Press where the line starts, drag its heading and length, release to fire.  Shift snaps.  Tab changes the pattern.";
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

        // ---- the speaker strip (HudDialogue / HudCommentary) ----------------------------------------------------------
        public const string SergeantName = "SERGEANT";
        public const string TipDeploy = "Silver buys men. Click a card below, or press 1 to 8, and they go up the line.";
        public const string TipPause = "Space stops the war. Orders you give while it's stopped go out the moment you let it run.";
        public const string TrenchTaken = "That trench is ours! Get men into it before they come back.";
        public const string TrenchLost = "They've taken one of our trenches! Fall back and hold the next.";
        public const string WonLine = "Their line's broken. Well fought, all of you.";
        public const string LostLine = "We're finished here. Pull back what you can.";
        public const string MachineLost = "I'm done for... get the crew out!";
        public const string MachineStalled = "Engine's dead! I'm a sitting duck here!";

        /// <summary>What a unit shouts the first time it is sent up the line.</summary>
        public static string Bark(byte archetype)
        {
            switch (PortraitName(archetype))
            {
                case "Rifleman": return "Rifles up! Over the parapet we go.";
                case "Assault": return "Clear the trench! Grenades first, questions later.";
                case "MG": return "Gun's set. Nothing crosses that wire.";
                case "Sniper": return "Keep your heads down. I'll find their officers.";
                case "Pincer": return "Claws out. Pincer is walking.";
                case "Kettle": return "Kettle's on! Mortar ready to brew.";
                case "Maw": return "MAW HUNGRY. MAW CRUSH WIRE.";
                default: return "Moving up.";
            }
        }

        /// <summary>A status report from a type of ours that has lost men: wounded first, critical when it is cut up.</summary>
        public static string LossReport(byte archetype, int lost, bool critical) =>
            critical ? $"{Name(archetype).ToUpperInvariant()}: {lost} men down. We can't hold much longer!"
                     : $"{Name(archetype).ToUpperInvariant()}: {lost} men down. We need help up here!";

        public static string LostMachine(byte archetype) => $"We've lost a {Name(archetype)}!";
    }
}
