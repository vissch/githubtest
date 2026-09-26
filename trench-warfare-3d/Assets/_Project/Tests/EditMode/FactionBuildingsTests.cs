// Phase: B6 / docs/21 phase 6 (implemented) — the Home Front tables: each faction has its buildings with four stages
// priced 40 / 80 / 150, every model named exists in its house set, nothing sells until a building is expanded and a
// stage sells two tiers, buying spends gold and raises the tier, an unlock joins the ability mask, the depot adds
// silver and income to a launch request, unit tracks name the faction's own machines, and the views follow the
// profile. Reads Resources (the house sets' json), no scene.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class FactionBuildingsTests
    {
        static readonly byte[] Factions = { FactionBuildings.Iron, FactionBuildings.Brass };

        [Test]
        public void Each_Faction_Has_Its_Buildings_With_Four_Stages_Priced_40_80_150()
        {
            foreach (var f in Factions)
            {
                var all = FactionBuildings.Of(f);
                Assert.That(all.Length, Is.GreaterThanOrEqualTo(7), "faction " + f);
                var ids = new HashSet<string>();
                foreach (var b in all)
                {
                    Assert.That(ids.Add(b.Id), b.Id + " twice");
                    Assert.That(b.Faction, Is.EqualTo(f), b.Id);
                    Assert.That(b.Stages.Length, Is.EqualTo(FactionBuildings.MaxStage + 1), b.Id);
                    Assert.That(b.Stages[0].Cost, Is.Zero, b.Id);
                    for (int s = 1; s < b.Stages.Length; s++)
                    {
                        Assert.That(b.Stages[s].Cost, Is.EqualTo(FactionBuildings.StageCosts[s - 1]), b.Id + " stage " + s);
                        Assert.That(b.Stages[s].ShownHeight, Is.GreaterThan(b.Stages[s - 1].ShownHeight), b.Id + " grows each stage");
                        Assert.That(string.IsNullOrEmpty(b.Stages[s].Blurb), Is.False, b.Id);
                    }
                    Assert.That(b.Stages[b.Stages.Length - 1].ShownHeight, Is.EqualTo(1f), b.Id + " is whole at the top");
                    Assert.That(b.Lines.Length, Is.GreaterThan(0), b.Id + " sells nothing");
                    foreach (var l in b.Lines)
                    {
                        Assert.That(string.IsNullOrEmpty(l.Name) || string.IsNullOrEmpty(l.Blurb), Is.False, b.Id);
                        Assert.That(l.Cost(1), Is.GreaterThan(0), l.Name);
                        Assert.That(l.Cost(l.MaxTier + 1), Is.Zero, l.Name + " past the top");
                        for (int t = 2; t <= l.MaxTier; t++) Assert.That(l.Cost(t), Is.GreaterThanOrEqualTo(l.Cost(t - 1)), l.Name + " tiers get dearer");
                    }
                }
                Assert.That(FactionBuildings.Find(f, "supply-depot"), Is.Not.Null);
                Assert.That(FactionBuildings.Find(f, "signals-station"), Is.Not.Null);
                Assert.That(FactionBuildings.Find(f, "no-such-building"), Is.Null);
            }
        }

        [Test]
        public void Every_Building_Model_Exists_In_Its_Set()
        {
            var sets = new Dictionary<string, string>();
            foreach (var f in Factions)
                foreach (var b in FactionBuildings.Of(f))
                {
                    if (!sets.TryGetValue(b.Set, out var json))
                    {
                        var text = Resources.Load<TextAsset>("Env/" + b.Set + "/houses");
                        Assert.That(text, Is.Not.Null, b.Id + " names set " + b.Set + ", which has no Resources/Env/" + b.Set + "/houses.json");
                        sets[b.Set] = json = text.text;
                    }
                    Assert.That(json, Does.Contain("\"" + b.Model + "_"), b.Id + " names model " + b.Model + ", which is not in the " + b.Set + " set");
                }
        }

        [Test]
        public void Nothing_Sells_Until_The_Building_Is_Expanded_And_A_Stage_Sells_Two_Tiers()
        {
            Assert.That(FactionBuildings.StageCap(0), Is.Zero);
            Assert.That(FactionBuildings.StageCap(1), Is.EqualTo(2));
            Assert.That(FactionBuildings.StageCap(3), Is.EqualTo(6));
            var p = new CampaignProfile { Gold = 1000 };
            var b = FactionBuildings.Find(FactionBuildings.Iron, "rifle-works");
            Assert.That(FactionBuildings.CanBuy(p, b, 0, out var why), Is.False);
            Assert.That(why, Is.EqualTo("EXPAND THE BUILDING"));
            Assert.That(FactionBuildings.Buy(p, b, 0), Is.False);
            Assert.That(p.Gold, Is.EqualTo(1000), "a refused purchase costs nothing");

            Assert.That(FactionBuildings.CanExpand(p, b, out _), Is.True);
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(p.StageOf(b.Faction, b.Id), Is.EqualTo(1));
            Assert.That(FactionBuildings.Buy(p, b, 0), Is.True);
            Assert.That(FactionBuildings.Buy(p, b, 0), Is.True);
            Assert.That(FactionBuildings.CanBuy(p, b, 0, out why), Is.False);
            Assert.That(why, Is.EqualTo("EXPAND FOR TIER 3"));
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(FactionBuildings.CanBuy(p, b, 0, out why), Is.True, why);
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(FactionBuildings.CanExpand(p, b, out why), Is.False);
            Assert.That(why, Is.EqualTo("FULLY BUILT"));
            Assert.That(p.Gold, Is.EqualTo(1000 - 40 - 80 - 150 - 15 - 20));

            var poor = new CampaignProfile { Gold = 30 };
            Assert.That(FactionBuildings.CanExpand(poor, b, out why), Is.False);
            Assert.That(why, Is.EqualTo("NEED 40 GOLD"));
        }

        [Test]
        public void Buying_Spends_Gold_And_Raises_The_Tier()
        {
            var p = new CampaignProfile { Gold = 100 };
            var b = FactionBuildings.Find(FactionBuildings.Brass, "rifle-works");
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(p.Gold, Is.EqualTo(60));
            Assert.That(FactionBuildings.Buy(p, b, 0), Is.True);
            Assert.That(p.Gold, Is.EqualTo(45));
            Assert.That(FactionBuildings.LineTier(p, b, 0), Is.EqualTo(1));
            Assert.That(FactionBuildings.Buy(p, b, 0), Is.True);
            Assert.That(p.Gold, Is.EqualTo(25));
            Assert.That(FactionBuildings.LineTier(p, b, 0), Is.EqualTo(2));
            Assert.That(FactionBuildings.Buy(p, b, 1), Is.True);
            Assert.That(p.Gold, Is.EqualTo(10));
            Assert.That(FactionBuildings.CanBuy(p, b, 2, out var why), Is.False);
            Assert.That(why, Is.EqualTo("NEED 15 GOLD"));
            Assert.That(FactionBuildings.CanBuy(p, b, 99, out why), Is.False);
            Assert.That(FactionBuildings.LineTier(p, FactionBuildings.Find(FactionBuildings.Iron, "rifle-works"), 0), Is.Zero, "the other faction's works are untouched");
        }

        [Test]
        public void An_Unlock_Adds_The_Ability_To_The_Mask()
        {
            var p = new CampaignProfile { Gold = 1000 };
            uint baseMask = FactionBuildings.AbilityMask(p, FactionBuildings.Iron);
            Assert.That(baseMask & (1u << (int)OffMapAbilityId.HeBarrage), Is.Not.Zero);
            Assert.That(baseMask & (1u << (int)OffMapAbilityId.ChlorineGas), Is.Not.Zero);
            Assert.That(baseMask & (1u << (int)OffMapAbilityId.CreepingBarrage), Is.Zero);

            var b = FactionBuildings.Find(FactionBuildings.Iron, "signals-station");
            int line = -1;
            for (int i = 0; i < b.Lines.Length; i++) if (b.Lines[i].Kind == LineKind.AbilityUnlock && b.Lines[i].Ability == OffMapAbilityId.CreepingBarrage) line = i;
            Assert.That(line, Is.GreaterThanOrEqualTo(0), "the signals station sells the creeping barrage");
            Assert.That(b.Lines[line].MaxTier, Is.EqualTo(1));
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(FactionBuildings.Buy(p, b, line), Is.True);
            Assert.That(p.Gold, Is.EqualTo(1000 - 40 - FactionBuildings.UnlockCost));
            Assert.That(p.UnlockedAbilities & (1u << (int)OffMapAbilityId.CreepingBarrage), Is.Not.Zero);
            Assert.That(FactionBuildings.AbilityMask(p, FactionBuildings.Iron) & (1u << (int)OffMapAbilityId.CreepingBarrage), Is.Not.Zero);
            Assert.That(FactionBuildings.AbilityMask(p, FactionBuildings.Brass) & (1u << (int)OffMapAbilityId.CreepingBarrage), Is.Zero, "bought for Iron only");
            Assert.That(FactionBuildings.CanBuy(p, b, line, out var why), Is.False);
            Assert.That(why, Is.EqualTo("AT THE TOP"));
        }

        [Test]
        public void The_Depot_Adds_Silver_And_Income_To_The_Request()
        {
            var p = new CampaignProfile { Gold = 1000 };
            var b = FactionBuildings.Find(FactionBuildings.Iron, "supply-depot");
            int silver = -1, income = -1;
            for (int i = 0; i < b.Lines.Length; i++)
            {
                if (b.Lines[i].Kind == LineKind.StartingSilver) silver = i;
                if (b.Lines[i].Kind == LineKind.Income) income = i;
            }
            Assert.That(silver, Is.GreaterThanOrEqualTo(0));
            Assert.That(income, Is.GreaterThanOrEqualTo(0));
            var r = new MatchLaunch.Request { StartingSilver = 300, SilverPerSecond = 2f };
            FactionBuildings.ApplyTo(r, p, FactionBuildings.Iron);
            Assert.That(r.StartingSilver, Is.EqualTo(300), "nothing bought, nothing added");
            Assert.That(FactionBuildings.Expand(p, b), Is.True);
            Assert.That(FactionBuildings.Buy(p, b, silver), Is.True);
            Assert.That(FactionBuildings.Buy(p, b, silver), Is.True);
            Assert.That(FactionBuildings.Buy(p, b, income), Is.True);
            Assert.That(p.Gold, Is.EqualTo(1000 - 40 - 30 - 60 - 30));
            FactionBuildings.ApplyTo(r, p, FactionBuildings.Iron);
            Assert.That(r.StartingSilver, Is.EqualTo(300 + 2 * FactionBuildings.SilverPerTier));
            Assert.That(r.SilverPerSecond, Is.EqualTo(2f + FactionBuildings.IncomePerTier).Within(1e-5f));
            Assert.That(FactionBuildings.StartingSilverBonus(p, FactionBuildings.Brass), Is.Zero);
        }

        [Test]
        public void Unit_Tracks_Name_The_Factions_Own_Machines()
        {
            var iron = new HashSet<byte> { VehicleArchetype.Maw, VehicleArchetype.Pincer, VehicleArchetype.Pavise, VehicleArchetype.Banner };
            var brass = new HashSet<byte> { VehicleArchetype.Tusk, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Redoubt };
            foreach (var f in Factions)
                foreach (var b in FactionBuildings.Of(f))
                    foreach (var l in b.Lines)
                    {
                        if (l.Kind != LineKind.UnitTrack) continue;
                        if (l.Archetype < VehicleArchetype.Maw) continue;   // infantry: both sides field them
                        Assert.That(f == FactionBuildings.Iron ? iron : brass, Does.Contain(l.Archetype), b.Id + " sells " + l.Name + ", not this faction's machine");
                    }
        }

        [Test]
        public void Views_Follow_The_Profile()
        {
            var p = new CampaignProfile();
            var views = FactionBuildings.Views(p, FactionBuildings.Brass);
            Assert.That(views.Count, Is.EqualTo(FactionBuildings.BrassBuildings.Length));
            foreach (var v in views) Assert.That(v.Stage, Is.Zero);
            p.SetStage(FactionBuildings.Brass, "tusk-yard", 2);
            views = FactionBuildings.Views(p, FactionBuildings.Brass);
            int found = 0;
            foreach (var v in views)
                if (v.Id == "tusk-yard") { found++; Assert.That(v.Stage, Is.EqualTo(2)); Assert.That(v.Set, Is.EqualTo("Houses")); Assert.That(v.Model, Is.EqualTo("House4")); }
            Assert.That(found, Is.EqualTo(1));
            foreach (var v in FactionBuildings.Views(p, FactionBuildings.Iron)) Assert.That(v.Stage, Is.Zero, "Iron's city is untouched");
        }
    }
}
