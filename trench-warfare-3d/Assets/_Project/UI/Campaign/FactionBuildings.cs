// Phase: B6 / docs/21 phase 6 (implemented) — the Home Front's buildings and what they sell, one code table per
// faction (like the roster tables in the sim: nothing to bake). A building has three stages past its ground floor
// (40 / 80 / 150 gold) that change its exterior on the diorama and cap the tiers it sells (stage n sells tiers up to
// 2n); a line is one upgrade track: a unit's health, damage or accuracy at 15 / 20 / 40 / 60 / 80 / 100 gold a
// tier, or a global row (armour plate, starting silver, income, ability cooldown and cost, veteran survival) at
// 30 / 60 / 120, or an ability unlock bought once. The profile holds the stages and tiers; this table prices and
// caps them, and applies what the launch request can carry today (starting silver and income). The rest waits for
// the sim's upgrade seam (docs/21 B1): the tiers are stored so nothing bought is lost.
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public enum LineKind : byte { UnitTrack = 0, ArmourPlate = 1, StartingSilver = 2, Income = 3, AbilityCooldown = 4, AbilityCost = 5, VeteranSurvival = 6, AbilityUnlock = 7 }
    public enum UnitTrack : byte { Health = 0, Damage = 1, Accuracy = 2 }

    public sealed class UpgradeLine
    {
        public string Name, Blurb;
        public LineKind Kind;
        /// <summary>UnitTrack: the roster archetype (RosterEntry.Archetype / VehicleArchetype).</summary>
        public byte Archetype;
        public UnitTrack Track;
        /// <summary>AbilityUnlock: the ability the line unlocks.</summary>
        public OffMapAbilityId Ability;
        public int MaxTier => Kind == LineKind.UnitTrack ? FactionBuildings.TrackCosts.Length : Kind == LineKind.AbilityUnlock ? 1 : FactionBuildings.GlobalCosts.Length;
        /// <summary>Gold for the given tier (1-based); 0 past the last tier.</summary>
        public int Cost(int tier)
        {
            if (tier < 1 || tier > MaxTier) return 0;
            if (Kind == LineKind.UnitTrack) return FactionBuildings.TrackCosts[tier - 1];
            if (Kind == LineKind.AbilityUnlock) return FactionBuildings.UnlockCost;
            return FactionBuildings.GlobalCosts[tier - 1];
        }
    }

    public sealed class BuildingStage
    {
        /// <summary>Gold to reach this stage from the one before (0 for the ground floor).</summary>
        public int Cost;
        /// <summary>How much of the model's height the diorama shows, 0..1.</summary>
        public float ShownHeight;
        public int Chimneys, Lamps;
        public string Blurb;
    }

    public sealed class Building
    {
        public string Id, Name, Blurb;
        public byte Faction;
        /// <summary>The house set (Resources/Env/&lt;Set&gt;) and the model in it the diorama draws.</summary>
        public string Set, Model;
        public Vector3 Place;
        public float Yaw;
        public BuildingStage[] Stages = Array.Empty<BuildingStage>();
        public UpgradeLine[] Lines = Array.Empty<UpgradeLine>();
    }

    public static class FactionBuildings
    {
        public const byte Iron = 0, Brass = 1;
        public const int MaxStage = CampaignProfile.MaxStage;
        public const int UnlockCost = 60;
        public static readonly int[] StageCosts = { 40, 80, 150 };               // to reach stage 1, 2, 3
        public static readonly int[] TrackCosts = { 15, 20, 40, 60, 80, 100 };   // tier 1..6 of a unit track
        public static readonly int[] GlobalCosts = { 30, 60, 120 };              // tier 1..3 of a global row
        public static readonly float[] StageHeights = { 0.3f, 0.55f, 0.8f, 1f };
        public const int SilverPerTier = 50;
        public const float IncomePerTier = 0.25f;
        public const int ArmourMmPerTier = 2, AbilityPercentPerTier = 8;
        public const float PerTier = 0.05f;

        public static readonly Building[] IronBuildings = BuildIron();
        public static readonly Building[] BrassBuildings = BuildBrass();

        /// <summary>The highest tier a building at this stage sells.</summary>
        public static int StageCap(int stage) => 2 * Mathf.Clamp(stage, 0, MaxStage);

        public static Building[] Of(byte faction) => faction == Brass ? BrassBuildings : IronBuildings;

        public static Building Find(byte faction, string id)
        {
            foreach (var b in Of(faction)) if (b.Id == id) return b;
            return null;
        }

        static BuildingStage[] Stages(string s0, string s1, string s2, string s3, int chimneys = 0, int lampsAtTop = 3) => new[]
        {
            new BuildingStage { Cost = 0, ShownHeight = StageHeights[0], Chimneys = 0, Lamps = 0, Blurb = s0 },
            new BuildingStage { Cost = StageCosts[0], ShownHeight = StageHeights[1], Chimneys = chimneys > 0 ? 1 : 0, Lamps = 1, Blurb = s1 },
            new BuildingStage { Cost = StageCosts[1], ShownHeight = StageHeights[2], Chimneys = chimneys > 1 ? 2 : chimneys, Lamps = 2, Blurb = s2 },
            new BuildingStage { Cost = StageCosts[2], ShownHeight = StageHeights[3], Chimneys = chimneys, Lamps = lampsAtTop, Blurb = s3 },
        };

        static UpgradeLine Track(string unit, byte archetype, UnitTrack track) => new UpgradeLine
        {
            Name = unit.ToUpperInvariant() + " " + track.ToString().ToUpperInvariant(), Kind = LineKind.UnitTrack, Archetype = archetype, Track = track,
            Blurb = track == UnitTrack.Health ? "+5 % hit points a tier." : track == UnitTrack.Damage ? "+5 % damage a tier." : "+5 % accuracy a tier.",
        };

        static UpgradeLine Global(string name, LineKind kind, string blurb) => new UpgradeLine { Name = name, Kind = kind, Blurb = blurb };

        static UpgradeLine Unlock(string name, OffMapAbilityId id, string blurb) => new UpgradeLine { Name = name, Kind = LineKind.AbilityUnlock, Ability = id, Blurb = blurb };

        const byte Rifleman = 0, Assault = 1, Machinegunner = 2, Sniper = 3;

        static UpgradeLine[] RifleWorks() => new[]
        {
            Track("Rifleman", Rifleman, UnitTrack.Health), Track("Rifleman", Rifleman, UnitTrack.Damage), Track("Rifleman", Rifleman, UnitTrack.Accuracy),
            Track("Assault", Assault, UnitTrack.Damage), Track("Machinegunner", Machinegunner, UnitTrack.Damage),
        };

        static UpgradeLine[] Signals() => new[]
        {
            Global("SIGNAL DISCIPLINE", LineKind.AbilityCooldown, "Support abilities return 8 % sooner a tier."),
            Global("REQUISITION", LineKind.AbilityCost, "Support abilities cost 8 % less a tier."),
            Unlock("CREEPING BARRAGE", OffMapAbilityId.CreepingBarrage, "A barrage that walks up the field ahead of your men."),
            Unlock("SMOKE SCREEN", OffMapAbilityId.SmokeScreen, "A line of smoke that blinds the guns behind it."),
            Unlock("STRAFE RUN", OffMapAbilityId.StrafeRun, "One low pass of machine-gun fire along a line."),
            Unlock("THE BEAM", OffMapAbilityId.Beam, "A line of fire from above that walks its corridor."),
        };

        static UpgradeLine[] Depot() => new[]
        {
            Global("WAR CHEST", LineKind.StartingSilver, "+50 silver at the start of every battle, a tier."),
            Global("SUPPLY TRAINS", LineKind.Income, "+0.25 silver a second, a tier."),
        };

        static Building[] BuildIron() => new[]
        {
            new Building
            {
                Id = "maw-foundry", Name = "MAW FOUNDRY", Faction = Iron, Set = "Military", Model = "Blockhouse", Place = new Vector3(-14f, 0f, 6f), Yaw = 20f,
                Blurb = "Where the heavy tank is cast. Each stage adds a furnace hall and a heavier hull.",
                Stages = Stages("A casting shed and a cold furnace.", "The first furnace lit: the Maw's hull plates.", "A second hall: sponson guns bored here.", "The full foundry, three furnaces roaring.", chimneys: 3),
                Lines = new[] { Track("Maw", VehicleArchetype.Maw, UnitTrack.Health), Track("Maw", VehicleArchetype.Maw, UnitTrack.Damage), Track("Maw", VehicleArchetype.Maw, UnitTrack.Accuracy) },
            },
            new Building
            {
                Id = "walker-assembly", Name = "WALKER ASSEMBLY", Faction = Iron, Set = "Military", Model = "Blockhouse", Place = new Vector3(-4f, 0f, 14f), Yaw = -10f,
                Blurb = "The legs and claws of the walking machines are fitted here.",
                Stages = Stages("A frame shop and a gantry.", "The Pincer's legs on the line.", "A gun shop for the Pavise's long piece.", "The Banner's standard raised over the doors.", chimneys: 1),
                Lines = new[] { Track("Pincer", VehicleArchetype.Pincer, UnitTrack.Health), Track("Pincer", VehicleArchetype.Pincer, UnitTrack.Damage), Track("Pavise", VehicleArchetype.Pavise, UnitTrack.Damage), Track("Banner", VehicleArchetype.Banner, UnitTrack.Health) },
            },
            new Building
            {
                Id = "rifle-works", Name = "RIFLE WORKS", Faction = Iron, Set = "Military", Model = "GuardPost", Place = new Vector3(8f, 0f, 14f), Yaw = 0f,
                Blurb = "Rifles, belts and bombs for the line battalions.",
                Stages = Stages("A gunsmith's bench.", "A proofing range behind the works.", "A second floor of lathes.", "The works at full stretch, day and night."),
                Lines = RifleWorks(),
            },
            new Building
            {
                Id = "officers-school", Name = "OFFICERS' SCHOOL", Faction = Iron, Set = "Military", Model = "CommandPost", Place = new Vector3(16f, 0f, 4f), Yaw = -30f,
                Blurb = "Marksmen and the men who bring the rest home.",
                Stages = Stages("A lecture room and a sand table.", "A rifle range for the marksmen.", "A staff wing.", "The academy, with its own parade ground."),
                Lines = new[] { Track("Sniper", Sniper, UnitTrack.Accuracy), Track("Sniper", Sniper, UnitTrack.Health), Global("BRING THEM HOME", LineKind.VeteranSurvival, "More of a battalion's veterans survive a lost battle, a tier.") },
            },
            new Building
            {
                Id = "blast-furnaces", Name = "BLAST FURNACES", Faction = Iron, Set = "Military", Model = "Blockhouse", Place = new Vector3(14f, 0f, -8f), Yaw = 15f,
                Blurb = "Thicker plate for every machine that leaves the city.",
                Stages = Stages("One furnace, banked.", "The first stack smoking.", "Two stacks and a rolling mill.", "Three stacks: the sky over the city is brown.", chimneys: 3),
                Lines = new[] { Global("ARMOUR PLATE", LineKind.ArmourPlate, "+2 mm on every face of every machine, a tier.") },
            },
            new Building
            {
                Id = "supply-depot", Name = "SUPPLY DEPOT", Faction = Iron, Set = "Military", Model = "GuardPost", Place = new Vector3(2f, 0f, -14f), Yaw = 0f,
                Blurb = "Silver at the start and silver through the battle.",
                Stages = Stages("A yard and a hand cart.", "A warehouse and a siding.", "Two warehouses and a crane.", "The railhead: trains every hour."),
                Lines = Depot(),
            },
            new Building
            {
                Id = "signals-station", Name = "SIGNALS STATION", Faction = Iron, Set = "Military", Model = "Watchtower", Place = new Vector3(-12f, 0f, -10f), Yaw = 40f,
                Blurb = "The wire to the guns, the aerodrome and the beam.",
                Stages = Stages("A field telephone in a hut.", "A mast and a wireless set.", "A tower: the guns answer faster.", "The full station, lit all night.", lampsAtTop: 4),
                Lines = Signals(),
            },
        };

        static Building[] BuildBrass() => new[]
        {
            new Building
            {
                Id = "tusk-yard", Name = "TUSK YARD", Faction = Brass, Set = "Houses", Model = "House4", Place = new Vector3(-14f, 0f, 6f), Yaw = 20f,
                Blurb = "The light tank is built in the open, under awnings.",
                Stages = Stages("A yard and a tarpaulin.", "A shed for the turrets.", "A second shed and a test track.", "The whole yard roofed over.", chimneys: 1),
                Lines = new[] { Track("Tusk", VehicleArchetype.Tusk, UnitTrack.Health), Track("Tusk", VehicleArchetype.Tusk, UnitTrack.Damage), Track("Tusk", VehicleArchetype.Tusk, UnitTrack.Accuracy) },
            },
            new Building
            {
                Id = "crab-pens", Name = "CRAB PENS", Faction = Brass, Set = "Houses", Model = "House3", Place = new Vector3(-4f, 0f, 14f), Yaw = -10f,
                Blurb = "The walking machines are kept, fed and armed here.",
                Stages = Stages("A pen and a chain.", "The Kettle's mortar shop.", "A gas shed for the Censer's drums.", "The Redoubt's plate hung on the walls.", chimneys: 1),
                Lines = new[] { Track("Kettle", VehicleArchetype.Kettle, UnitTrack.Damage), Track("Kettle", VehicleArchetype.Kettle, UnitTrack.Health), Track("Censer", VehicleArchetype.Censer, UnitTrack.Health), Track("Redoubt", VehicleArchetype.Redoubt, UnitTrack.Health) },
            },
            new Building
            {
                Id = "rifle-works", Name = "RIFLE WORKS", Faction = Brass, Set = "Houses", Model = "House1", Place = new Vector3(8f, 0f, 14f), Yaw = 0f,
                Blurb = "Rifles, belts and bombs for the line battalions.",
                Stages = Stages("A gunsmith's bench.", "A proofing range behind the works.", "A second floor of lathes.", "The works at full stretch, day and night."),
                Lines = RifleWorks(),
            },
            new Building
            {
                Id = "raiders-lodge", Name = "RAIDERS' LODGE", Faction = Brass, Set = "Houses", Model = "House2", Place = new Vector3(16f, 0f, 4f), Yaw = -30f,
                Blurb = "Where the trench raiders drink, and where they learn.",
                Stages = Stages("A tavern with a back room.", "A bombing pit behind it.", "An upper floor of bunks.", "The lodge in full: songs until dawn."),
                Lines = new[] { Track("Assault", Assault, UnitTrack.Health), Track("Assault", Assault, UnitTrack.Accuracy), Track("Machinegunner", Machinegunner, UnitTrack.Health) },
            },
            new Building
            {
                Id = "plate-mill", Name = "PLATE MILL", Faction = Brass, Set = "Houses", Model = "House0", Place = new Vector3(14f, 0f, -8f), Yaw = 15f,
                Blurb = "Rolled brass over iron: thicker plate for every machine.",
                Stages = Stages("One roller, hand-turned.", "A steam roller and a stack.", "Two rollers and a furnace.", "The mill in full: plate by the ton.", chimneys: 2),
                Lines = new[] { Global("ARMOUR PLATE", LineKind.ArmourPlate, "+2 mm on every face of every machine, a tier.") },
            },
            new Building
            {
                Id = "supply-depot", Name = "SUPPLY DEPOT", Faction = Brass, Set = "Houses", Model = "House5", Place = new Vector3(2f, 0f, -14f), Yaw = 0f,
                Blurb = "Silver at the start and silver through the battle.",
                Stages = Stages("A yard and a hand cart.", "A warehouse and a siding.", "Two warehouses and a crane.", "The railhead: trains every hour."),
                Lines = Depot(),
            },
            new Building
            {
                Id = "signals-station", Name = "SIGNALS STATION", Faction = Brass, Set = "Houses", Model = "House2", Place = new Vector3(-12f, 0f, -10f), Yaw = 40f,
                Blurb = "The wire to the guns, the aerodrome and the beam.",
                Stages = Stages("A field telephone in a hut.", "A mast and a wireless set.", "A tower: the guns answer faster.", "The full station, lit all night.", lampsAtTop: 4),
                Lines = Signals(),
            },
            new Building
            {
                Id = "field-hospital", Name = "FIELD HOSPITAL", Faction = Brass, Set = "Houses", Model = "House1", Place = new Vector3(-2f, 0f, -2f), Yaw = 90f,
                Blurb = "The wounded come back; the marksmen come back harder.",
                Stages = Stages("A dressing station in a barn.", "A ward and a surgeon.", "Two wards and a dispensary.", "The hospital in full, a train of its own."),
                Lines = new[] { Global("BRING THEM HOME", LineKind.VeteranSurvival, "More of a battalion's veterans survive a lost battle, a tier."), Track("Sniper", Sniper, UnitTrack.Health) },
            },
        };

        // ---- the profile against the table ----

        public static int LineTier(CampaignProfile p, Building b, int line) => p.TierOf(b.Faction, b.Id, line);

        /// <summary>Can the building be expanded to its next stage? The reason says why not.</summary>
        public static bool CanExpand(CampaignProfile p, Building b, out string reason)
        {
            int stage = p.StageOf(b.Faction, b.Id);
            if (stage >= MaxStage) { reason = "FULLY BUILT"; return false; }
            int cost = b.Stages[stage + 1].Cost;
            if (p.Gold < cost) { reason = "NEED " + cost + " GOLD"; return false; }
            reason = "";
            return true;
        }

        public static bool Expand(CampaignProfile p, Building b)
        {
            if (!CanExpand(p, b, out _)) return false;
            int stage = p.StageOf(b.Faction, b.Id);
            p.Gold -= b.Stages[stage + 1].Cost;
            p.SetStage(b.Faction, b.Id, stage + 1);
            return true;
        }

        /// <summary>Can the next tier of a line be bought? The reason says why not (the stage cap, the tier cap, the gold).</summary>
        public static bool CanBuy(CampaignProfile p, Building b, int line, out string reason)
        {
            if (line < 0 || line >= b.Lines.Length) { reason = "NO SUCH LINE"; return false; }
            var l = b.Lines[line];
            int tier = p.TierOf(b.Faction, b.Id, line);
            if (tier >= l.MaxTier) { reason = "AT THE TOP"; return false; }
            int cap = StageCap(p.StageOf(b.Faction, b.Id));
            if (tier + 1 > cap) { reason = cap == 0 ? "EXPAND THE BUILDING" : "EXPAND FOR TIER " + (tier + 1); return false; }
            int cost = l.Cost(tier + 1);
            if (p.Gold < cost) { reason = "NEED " + cost + " GOLD"; return false; }
            reason = "";
            return true;
        }

        public static bool Buy(CampaignProfile p, Building b, int line)
        {
            if (!CanBuy(p, b, line, out _)) return false;
            var l = b.Lines[line];
            int tier = p.TierOf(b.Faction, b.Id, line) + 1;
            p.Gold -= l.Cost(tier);
            p.SetTier(b.Faction, b.Id, line, tier);
            if (l.Kind == LineKind.AbilityUnlock) p.UnlockedAbilities |= 1u << (int)l.Ability;
            return true;
        }

        /// <summary>The tier of a global row across the faction's buildings (the highest bought).</summary>
        public static int GlobalTier(CampaignProfile p, byte faction, LineKind kind)
        {
            int best = 0;
            foreach (var b in Of(faction))
                for (int i = 0; i < b.Lines.Length; i++)
                    if (b.Lines[i].Kind == kind) best = Mathf.Max(best, p.TierOf(faction, b.Id, i));
            return best;
        }

        /// <summary>The abilities the faction may fire: the two every side starts with plus the unlocks bought.</summary>
        public static uint AbilityMask(CampaignProfile p, byte faction)
        {
            uint mask = (1u << (int)OffMapAbilityId.HeBarrage) | (1u << (int)OffMapAbilityId.ChlorineGas);
            foreach (var b in Of(faction))
                for (int i = 0; i < b.Lines.Length; i++)
                    if (b.Lines[i].Kind == LineKind.AbilityUnlock && p.TierOf(faction, b.Id, i) > 0) mask |= 1u << (int)b.Lines[i].Ability;
            return mask;
        }

        public static int StartingSilverBonus(CampaignProfile p, byte faction) => SilverPerTier * GlobalTier(p, faction, LineKind.StartingSilver);
        public static float IncomeBonus(CampaignProfile p, byte faction) => IncomePerTier * GlobalTier(p, faction, LineKind.Income);

        /// <summary>What the launch request can carry today: the depot's silver and income. Unit tiers, armour,
        /// ability cooldown/cost and the mask wait for the sim's upgrade seam (docs/21 B1).</summary>
        public static void ApplyTo(MatchLaunch.Request r, CampaignProfile p, byte faction)
        {
            r.StartingSilver += StartingSilverBonus(p, faction);
            r.SilverPerSecond += IncomeBonus(p, faction);
        }

        public static List<BuildingView> Views(CampaignProfile p, byte faction)
        {
            var list = new List<BuildingView>();
            foreach (var b in Of(faction))
                list.Add(new BuildingView { Id = b.Id, Name = b.Name, Set = b.Set, Model = b.Model, Stage = p.StageOf(faction, b.Id), Place = b.Place, Yaw = b.Yaw });
            return list;
        }
    }
}
