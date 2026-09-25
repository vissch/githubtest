// Phase: B6 (implemented) — every word the battle HUD shows, in one pure static so tests can read it without a panel.
// Moved from BattleHud.cs (84-143) so the Toolkit HUD and the IMGUI HUD say the same things during the flag window;
// BattleHud keeps its own copy until it is deleted, and HudTextTests points here from step A6 of the UI plan.
// Names, tooltips and portraits are keyed by ARCHETYPE, never by roster slot: the factions field different slots
// (Iron's officer sits where Brass's sniper does), so a bar indexed by slot draws one side's pictures for the other.
// Those words now live in TW.Presentation.UnitLook, which the IMGUI BattleHud reads too — it cannot see TW.UI, the
// dependency runs the other way, and while each HUD kept its own copy the two drifted apart. This is the face of that
// table for the Toolkit HUD and for tests; everything below the unit words is the Toolkit HUD's own.
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.UI
{
    public static class HudText
    {
        // ---- the unit words, all of them UnitLook's ------------------------------------------------------------
        /// <summary>The short name on a card: "Rifle", "Officer", "Maw", …</summary>
        public static string Name(byte archetype) => UnitLook.Name(archetype);

        /// <summary>What a player needs in order to choose one; the tooltip body under the name.</summary>
        public static string Tip(byte archetype) => UnitLook.Tip(archetype);

        /// <summary>The portrait file stem under Assets/_Project/UI/Skin/Portraits/, per SkinSpec.PortraitNames.</summary>
        public static string PortraitName(byte archetype) => UnitLook.PortraitName(archetype);

        /// <summary>How many unit portraits the skin has: archetypes 0..18, held at or above every archetype either
        /// faction's roster hands out so a new machine cannot inherit its neighbour's picture.</summary>
        public const int PortraitCount = UnitLook.PortraitCount;

        /// <summary>The key printed in a card's badge: ten roster slots take "1".."9" and "0", support takes F5-F7.</summary>
        public static string Hotkey(int slot) => UnitLook.Hotkey(slot);
        public static string SupportHotkey(int index) => UnitLook.SupportHotkey(index);
        public const int SupportCards = UnitLook.SupportCards;

        /// <summary>Which support card an ability sits on, and so which key it takes; -1 if it has no card.</summary>
        public static int SupportIndex(OffMapAbilityId id) => UnitLook.SupportIndex(id);
        public static string SupportName(OffMapAbilityId id) => UnitLook.SupportName(id);
        public static string SupportCardLabel(OffMapAbilityId id) => UnitLook.SupportCardLabel(id);
        public static string SupportTip(OffMapAbilityId id) => UnitLook.SupportTip(id);
        public static string SupportPortrait(OffMapAbilityId id) => UnitLook.SupportPortrait(id);

        public static string VehicleName(byte archetype) => UnitLook.VehicleName(archetype);

        /// <summary>UnitLook holds the words and the sim constants they quote; HudTextTests checks them there.</summary>
        public static string VehicleTip(byte archetype) => UnitLook.VehicleTip(archetype);

        /// <summary>The support cards' text, UnitLook's: 12 shells, 25 m and a 4 s delay are WarmupTicks 80 at
        /// TickRate 20, and HudBindTests checks all three against OffMapAbilitySystem so they cannot go stale.</summary>
        public const string BarrageName = UnitLook.BarrageName;
        public const string GasName = UnitLook.GasName;
        public const string DropName = UnitLook.DropName;
        public const string BarrageCard = UnitLook.BarrageCard, GasCard = UnitLook.GasCard, DropCard = UnitLook.DropCard;
        public const string SilverGaugeTip = "Silver to spend on units and support; the figure beside it is income per second";
        public const string MenGaugeTip = "Your men on the field, and the enemy's beside VS";
        public const string TimeGaugeTip = "Time since the battle began; PAUSED shows here while the clock is stopped";
        public const string BarrageTip = UnitLook.BarrageTip;
        public const string GasTip = UnitLook.GasTip;
        public const string DropTip = UnitLook.DropTip;

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

        // ---- the speaker strip (HudDialogue / HudCommentary) ----------------------------------------------------------
        public const string SergeantName = "SERGEANT";
        public const string TipDeploy = "Silver buys men. Click a card below, or press 1 to 0, and they go up the line.";
        public const string TipPause = "Space stops the war. Orders you give while it's stopped go out the moment you let it run.";
        public const string TrenchTaken = "That trench is ours! Get men into it before they come back.";
        public const string TrenchLost = "They've taken one of our trenches! Fall back and hold the next.";
        public const string WonLine = "Their line's broken. Well fought, all of you.";
        public const string LostLine = "We're finished here. Pull back what you can.";
        public const string MachineLost = "I'm done for... get the crew out!";
        public const string MachineStalled = "Engine's dead! I'm a sitting duck here!";

        /// <summary>What a unit shouts the first time it is sent up the line.</summary>
        public static string Bark(byte archetype) => UnitLook.Bark(archetype);

        /// <summary>A status report from a type of ours that has lost men: wounded first, critical when it is cut up.</summary>
        public static string LossReport(byte archetype, int lost, bool critical) =>
            critical ? $"{Name(archetype).ToUpperInvariant()}: {lost} men down. We can't hold much longer!"
                     : $"{Name(archetype).ToUpperInvariant()}: {lost} men down. We need help up here!";

        public static string LostMachine(byte archetype) => $"We've lost a {Name(archetype)}!";
    }
}
