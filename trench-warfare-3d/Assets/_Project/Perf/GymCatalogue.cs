// Phase: tooling (the gym, 2026-09-28) — the gym's list of everything that can be played on its own: every infantry clip
// on each VAT figure, every unit the match's table fields, every off-map ability, every way a man dies, every sim event,
// and the zoom bands to look at each one in. The lists are read from the enums and the unit table, never typed out,
// so a new clip, ability, death or event is in the gym the moment it exists; GymCatalogueTests fails when an event or
// ability has no row saying how the gym shows it. Pure data: GymDirector stages an entry, Editor/Gym captures it.
using System;
using System.Collections.Generic;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Perf
{
    public enum GymTab : byte { Clips, Units, Abilities, Deaths, Events, Scenes }

    /// <summary>Scenes: the owner's "see your actions affect the battlefield", men where a player has them and the thing
    /// that hits them (GymDirector.Scene stages each; the result counts what happened to the watched men).</summary>
    public enum GymScene : byte
    {
        /// <summary>Six riflemen in our fire trench, an enemy line 80 m out: how men in a trench stand and shoot (foxholes).</summary>
        TrenchLine,
        /// <summary>The same, and the enemy's HE barrage on the trench: the men's reactions, the craters, the dead.</summary>
        BarrageOnTrench,
        /// <summary>The same, and the enemy's chlorine drifting into the trench.</summary>
        GasOnTrench,
        /// <summary>A barrage makes craters, then six men stand in them facing an enemy line: men in shell holes.</summary>
        CraterMen,
    }

    /// <summary>What the gym expects when it triggers an entry.</summary>
    public enum GymExpect : byte
    {
        /// <summary>It happens: the entry's events arrive.</summary>
        Fires,
        /// <summary>The sim refuses it (CommandRejected): the ability has no stats in this build.</summary>
        Rejected,
        /// <summary>Only one faction may call it, so the gym issues it from that faction's seat.</summary>
        FactionSeat,
        /// <summary>Staged by other entries (an ability, a death, a unit): listed so the event is covered, not triggered alone.</summary>
        Covered,
        /// <summary>Too costly to stage for real: the event is replayed into the presentation's frame, marked "preview".</summary>
        Preview,
        /// <summary>No picture of its own (match state, orders, debug): listed with the reason.</summary>
        Excluded,
    }

    public struct GymEntry
    {
        public GymTab Tab;
        public int Id;          // Clip, archetype, OffMapAbilityId, DeathKind or SimEventType, per Tab
        public int Figure;      // Clips: VATRenderer.FigureNames index; otherwise 0
        public string Name;
        public GymExpect Expect;
        public string Note;
        public override string ToString() => $"{Tab}/{Name}";
    }

    public struct GymBand { public string Name; public float Zoom; }

    public static class GymCatalogue
    {
        /// <summary>The bands every entry is looked at in: T3, T2, T1 (the AOSA tiers), the two overviews and the far view.</summary>
        public static readonly GymBand[] Bands =
        {
            new GymBand { Name = "T3", Zoom = 7.5f }, new GymBand { Name = "T2", Zoom = 16f }, new GymBand { Name = "T1", Zoom = 30f },
            new GymBand { Name = "O120", Zoom = 120f }, new GymBand { Name = "O240", Zoom = 240f }, new GymBand { Name = "Far", Zoom = 600f },
        };

        /// <summary>An archetype drawn with each figure, for pinning a clip on it (the sniper is the hooded figure).</summary>
        public static int ArchetypeForFigure(int figure) => figure == 1 ? 3 : 0;

        public static List<GymEntry> Clips()
        {
            var list = new List<GymEntry>();
            for (int f = 0; f < VATRenderer.FigureNames.Length; f++)
                for (int c = 1; c < (int)Clip.Count; c++)
                    list.Add(new GymEntry { Tab = GymTab.Clips, Id = c, Figure = f, Name = VATRenderer.FigureNames[f] + "/" + (Clip)c, Expect = GymExpect.Fires });
            return list;
        }

        /// <summary>Every archetype the world's unit table fields (Hp above 0), the Unit Sandbox's rule.</summary>
        public static List<GymEntry> Units(SimWorld w)
        {
            var list = new List<GymEntry>();
            if (w == null) return list;
            for (int a = 0; a < Archetypes.Count; a++)
            {
                if (w.Units.Roster[a].Hp <= 0f) continue;
                string name = UnitLook.Name((byte)a);
                if (string.IsNullOrEmpty(name)) continue;
                list.Add(new GymEntry { Tab = GymTab.Units, Id = a, Name = name, Expect = GymExpect.Fires });
            }
            return list;
        }

        public static List<GymEntry> Abilities()
        {
            var list = new List<GymEntry>();
            foreach (OffMapAbilityId id in Enum.GetValues(typeof(OffMapAbilityId)))
            {
                if (id == OffMapAbilityId.None) continue;
                var e = new GymEntry { Tab = GymTab.Abilities, Id = (int)id, Name = id.ToString() };
                if (!OffMapAbilitySystem.TryGetStats((int)id, out _)) { e.Expect = GymExpect.Rejected; e.Note = "no stats in this build: the sim refuses it"; }
                else if (id == OffMapAbilityId.ParaDrop) { e.Expect = GymExpect.FactionSeat; e.Note = "Brass only (FactionRoster.MayCall): issued from the Brass seat"; }
                else e.Expect = GymExpect.Fires;
                list.Add(e);
            }
            return list;
        }

        public static List<GymEntry> Deaths()
        {
            var list = new List<GymEntry>();
            foreach (DeathKind k in Enum.GetValues(typeof(DeathKind)))
                list.Add(new GymEntry { Tab = GymTab.Deaths, Id = (int)k, Name = k.ToString(), Expect = GymExpect.Fires, Note = DeathStaging(k) });
            return list;
        }

        /// <summary>How the gym kills a man for each death kind. Deaths come from a sim system stepping inside a tick:
        /// an event raised inside WriteWorlds is cleared by the next SimWorld.Step and never reaches the picture.</summary>
        public static string DeathStaging(DeathKind k)
        {
            switch (k)
            {
                case DeathKind.Shot: return "he is left 1 hp; an enemy rifle line in range fires";
                case DeathKind.Blast: return "HE barrage on him";
                case DeathKind.Gas: return "chlorine gas on him (slow: 40 s allowed)";
                case DeathKind.Crushed: return "best effort: an enemy Maw driven over him";
                case DeathKind.Burning: return "set alight (Burning.Ignite via WriteWorlds); the burning system kills him";
                case DeathKind.Beam: return "the Beam along a line through him";
                default: return null;
            }
        }

        public static List<GymEntry> Events()
        {
            var list = new List<GymEntry>();
            foreach (SimEventType t in Enum.GetValues(typeof(SimEventType)))
            {
                if (t == SimEventType.None) continue;
                var how = EventHow(t);
                list.Add(new GymEntry { Tab = GymTab.Events, Id = (int)t, Name = t.ToString(), Expect = how.expect, Note = how.note });
            }
            return list;
        }

        /// <summary>How the gym shows each sim event. There is no default: a new SimEventType answers null here until
        /// someone decides, and GymCatalogueTests fails on it.</summary>
        public static (GymExpect expect, string note) EventHow(SimEventType t)
        {
            switch (t)
            {
                // staged by the ability, death and unit entries
                case SimEventType.Shot: case SimEventType.Hit: case SimEventType.NearMiss: case SimEventType.Death:
                case SimEventType.Suppressed: case SimEventType.Pinned: case SimEventType.StanceChanged:
                    return (GymExpect.Covered, "the Deaths tab's rifle line and the Units tab");
                case SimEventType.Explosion: case SimEventType.CraterStamp: case SimEventType.AbilityFired:
                case SimEventType.GasCloudSpawned: case SimEventType.SmokeSpawned: case SimEventType.CellBurning:
                case SimEventType.DropInbound: case SimEventType.DropLanded:
                    return (GymExpect.Covered, "the Abilities tab");
                case SimEventType.UnitSpawned: case SimEventType.UnitDeployed:
                    return (GymExpect.Covered, "the Units tab");
                case SimEventType.UnitAlight:
                    return (GymExpect.Covered, "the Deaths tab (Burning)");
                case SimEventType.LeapStarted: case SimEventType.BreakerPhase: case SimEventType.CriticalHit:
                case SimEventType.UnitHealed: case SimEventType.VehicleHullMended: case SimEventType.ShieldBlocked:
                case SimEventType.VehicleFired: case SimEventType.VehicleArmourHit: case SimEventType.VehicleModuleHit:
                case SimEventType.RocketFired:   // the Salvo's rack, fired at the Units tab's enemy line
                    return (GymExpect.Covered, "the Units tab, staged against an enemy line");
                // hard to stage alone: replayed into the presentation, marked preview
                case SimEventType.UnitEnteredTrench: case SimEventType.UnitLeftTrench: case SimEventType.TrenchCaptured:
                case SimEventType.VehicleTrackHit: case SimEventType.VehicleStalled: case SimEventType.VehicleDestroyed:
                case SimEventType.WireBreached: case SimEventType.PropChanged: case SimEventType.VehicleOnFire:
                case SimEventType.VehicleCrewLost: case SimEventType.VehicleBailedOut: case SimEventType.VehicleKnockedOut:
                case SimEventType.VehicleCookOff: case SimEventType.VehicleBogged: case SimEventType.VehicleDitched:
                case SimEventType.VehicleCrushed: case SimEventType.VehicleRepaired: case SimEventType.VehicleLegLost:
                case SimEventType.VehicleClawed: case SimEventType.CraftInbound: case SimEventType.CraftBeached:
                case SimEventType.ShipFired: case SimEventType.MinePlaced: case SimEventType.MineTriggered:
                case SimEventType.MineCleared: case SimEventType.HeroMoment: case SimEventType.HeroFeat:
                case SimEventType.HeroSurvived: case SimEventType.HeroFallen: case SimEventType.VeteranDeployed:
                case SimEventType.WreckRecorded:
                case SimEventType.SapperOrdered: case SimEventType.SapperLaying:   // a sapper at work needs an order and time
                case SimEventType.GrenadeThrown:   // a man in the open 5-22 m from a trench: an assault, not a staged line
                // hand to hand (replay v25) needs two men within 8 m and the seconds of a fight, a pounce a crab and a man 10 m
                // in front of it; a worn wreck (v26) a dead machine and fire on it. Not staged yet: replayed
                case SimEventType.MeleeBlow: case SimEventType.WeaponDropped: case SimEventType.WeaponPickedUp:
                case SimEventType.PounceCrouched: case SimEventType.PounceLanded: case SimEventType.PropWorn:
                    return (GymExpect.Preview, "preview (effects only): replayed into the presentation's frame at a staged unit; the men never see it");
                // no picture of their own
                case SimEventType.ObjectiveCaptured: return (GymExpect.Excluded, "HUD only");
                case SimEventType.WaveStarted: case SimEventType.MissionTriggerFired: return (GymExpect.Excluded, "mission state");
                case SimEventType.MatchEnded: return (GymExpect.Excluded, "ends the match");
                case SimEventType.CommandRejected: return (GymExpect.Excluded, "debug only; the Abilities tab counts it");
                case SimEventType.OrderFoundNoOne: return (GymExpect.Excluded, "an order note for the HUD");
                default: return (GymExpect.Excluded, null);
            }
        }

        public static List<GymEntry> Scenes()
        {
            var list = new List<GymEntry>();
            foreach (GymScene sc in Enum.GetValues(typeof(GymScene)))
                list.Add(new GymEntry { Tab = GymTab.Scenes, Id = (int)sc, Name = sc.ToString(), Expect = GymExpect.Fires,
                    Note = sc == GymScene.TrenchLine ? "men in our trench, an enemy line at 80 m" : sc == GymScene.CraterMen ? "men in fresh craters, an enemy line at 80 m" : "men in our trench, hit by the enemy" });
            return list;
        }

        public static List<GymEntry> All(SimWorld w)
        {
            var all = new List<GymEntry>();
            all.AddRange(Scenes()); all.AddRange(Clips()); all.AddRange(Units(w)); all.AddRange(Abilities()); all.AddRange(Deaths()); all.AddRange(Events());
            return all;
        }
    }
}
