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

        /// <summary>The key printed in a card's badge: ten roster slots take "1".."9" and "0" (UnitLook).</summary>
        public static string Hotkey(int slot) => UnitLook.Hotkey(slot);
        static readonly GameAction[] SupportActions = { GameAction.ArmBarrage, GameAction.ArmGas, GameAction.ArmDrop,
            GameAction.ArmCreeping, GameAction.ArmSmoke, GameAction.ArmStrafe, GameAction.ArmBeam };
        /// <summary>The key on a support card, in HudView.SupportAbilities order: whatever KeyMap binds the arm action to
        /// now, so a rebinding shows on the card.</summary>
        public static string SupportHotkey(int index) => index >= 0 && index < SupportActions.Length ? Badge(KeyMap.Display(KeyMap.Primary(SupportActions[index]))) : "";
        /// <summary>An unbound action's badge: a dash, so the card still reads and two unbound cards do not share an empty key.</summary>
        static string Badge(string key) => string.IsNullOrEmpty(key) ? "—" : key;
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
        public const string BarrageTip = "HE barrage: 12 shells in 25 m after 4 s; craters give cover. Tab: line or box";
        public const string GasTip = "Chlorine gas: drifts with the wind, pools in trenches, drives the garrison out. Tab: creeping";
        public const string DropTip = UnitLook.DropTip;
        public const string CreepingName = "Creeping Barrage", SmokeName = "Smoke Screen", StrafeName = "Strafe Run", BeamName = "Beam";
        public const string CreepingCard = "CREEP", SmokeCard = "SMOKE", StrafeCard = "STRAFE", BeamCard = "BEAM";
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
                case TW.Sim.Match.OffMapAbilityId.ParaDrop: return new SupportText(UnitLook.DropName, UnitLook.DropCard, DropTip, "ParaDrop");
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
        public const string AimLineHint = "Press where the line starts, drag its heading and length, release to fire.  Shift snaps.  Tab changes the pattern.  Esc or right click cancels.";
        /// <summary>The aim hint for the armed ability: the Tab clause only when it has patterns to cycle (smoke, strafe, beam
        /// and creeping have none; the hint told them to press Tab: critic r5, 2026-09-27).</summary>
        public static string AimHintFor(bool line, bool patterns)
        {
            string hint = line ? AimLineHint : AimHint;
            return patterns ? hint : hint.Replace("  Tab changes the pattern.", "");
        }
        public const string LockedTip = "Locked: reinforcements pass through to the next trench. Click to open";
        public const string OpenTip = "Open: reinforcements stop here. Click to lock";
        public const string FallbackTip = "Fall back to the trench behind";
        public const string HoldingTip = "Holding fire. Click to fire at will";
        public const string FiringTip = "Firing at will. Click to hold fire";
        public const string AdvanceTip = "Over the top: the garrison advances on the next trench";
        public const string PauseTip = "Tactical pause (Space): orders still go through";
        public const string SpeedTip = "Game speed";

        /// <summary>Centre banners. The HUD's is the only one (CombatFx drew an IMGUI copy of each over it until 2026-09-27).</summary>
        public static string CapturedBanner(int trench, bool mine) => mine ? $"Trench {trench} captured!" : $"Trench {trench} lost";
        /// <summary>A support ability fired: yours on its way, or the enemy's coming in, named as its card names it.</summary>
        public static string AbilityBanner(TW.Sim.Match.OffMapAbilityId id, bool mine)
            => mine ? Support(id).Name + " on its way" : "INCOMING " + Support(id).Name.ToUpperInvariant();
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
