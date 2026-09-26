// Phase: B6 / docs/21 phase 6 (implemented) — the campaign profile: it round-trips through JSON whole, a win is
// recorded once (a replay pays nothing), Migrate clamps what a hand-edited file may carry, the store writes a
// file through a .tmp swap and reads it back, a garbage file starts a fresh campaign, and a clone is independent.
// The store tests use a temp folder, never the player's profile.json.
using System;
using System.IO;
using NUnit.Framework;
using TW.Presentation;

namespace TW.Tests
{
    public class CampaignProfileTests
    {
        static CampaignProfile Sample()
        {
            var p = new CampaignProfile { Faction = 1, Gold = 123, LastNode = "river-line", HeroPity = 0.4f };
            p.Complete("lowlands", 0); p.Complete("lowlands", 1);
            p.SetStage(1, "tusk-yard", 2);
            p.SetTier(1, "tusk-yard", 0, 3);
            p.SetTier(0, "rifle-works", 2, 1);
            p.UnlockedAbilities = 1u << 6;
            return p;
        }

        [Test]
        public void A_Profile_Round_Trips_Through_Json()
        {
            var p = Sample();
            var back = CampaignProfile.FromJson(p.ToJson());
            Assert.That(back.Version, Is.EqualTo(CampaignProfile.CurrentVersion));
            Assert.That(back.Faction, Is.EqualTo(1));
            Assert.That(back.Gold, Is.EqualTo(123));
            Assert.That(back.LastNode, Is.EqualTo("river-line"));
            Assert.That(back.HeroPity, Is.EqualTo(0.4f).Within(1e-6f));
            Assert.That(back.IsComplete("lowlands", 0), Is.True);
            Assert.That(back.IsComplete("lowlands", 1), Is.True);
            Assert.That(back.IsComplete("lowlands", 2), Is.False);
            Assert.That(back.StageOf(1, "tusk-yard"), Is.EqualTo(2));
            Assert.That(back.StageOf(0, "tusk-yard"), Is.Zero, "stages are per faction");
            Assert.That(back.TierOf(1, "tusk-yard", 0), Is.EqualTo(3));
            Assert.That(back.TierOf(1, "tusk-yard", 1), Is.Zero);
            Assert.That(back.TierOf(0, "rifle-works", 2), Is.EqualTo(1));
            Assert.That(back.UnlockedAbilities, Is.EqualTo(1u << 6));
        }

        [Test]
        public void A_Win_Is_Recorded_Once()
        {
            var p = new CampaignProfile();
            Assert.That(p.Complete("lowlands", 0), Is.True);
            Assert.That(p.Complete("lowlands", 0), Is.False, "already won: nothing to pay");
            Assert.That(p.CompletedMissions.Count, Is.EqualTo(1));
            Assert.That(CampaignProfile.MissionKey("lowlands", 0), Is.EqualTo("lowlands/0"));
        }

        [Test]
        public void Migrate_Clamps_What_A_Hand_Edited_File_Carries()
        {
            var p = new CampaignProfile { Gold = -5, Faction = 7, Version = 0, HeroPity = float.NaN, LastNode = null, CompletedMissions = null, Buildings = null, Lines = null };
            p.Migrate();
            Assert.That(p.Gold, Is.Zero);
            Assert.That(p.Faction, Is.Zero);
            Assert.That(p.Version, Is.EqualTo(CampaignProfile.CurrentVersion));
            Assert.That(p.HeroPity, Is.Zero);
            Assert.That(p.LastNode, Is.EqualTo(""));
            Assert.That(p.CompletedMissions, Is.Not.Null);
            Assert.That(p.Buildings, Is.Not.Null);
            Assert.That(p.Lines, Is.Not.Null);

            p.Buildings.Add(new CampaignProfile.BuildingState { Faction = 0, Id = "x", Stage = 9 });
            p.Lines.Add(new CampaignProfile.LineState { Faction = 0, Building = "x", Line = 0, Tier = 40 });
            p.Migrate();
            Assert.That(p.StageOf(0, "x"), Is.EqualTo(CampaignProfile.MaxStage));
            Assert.That(p.TierOf(0, "x", 0), Is.EqualTo(CampaignProfile.MaxTier));
            p.SetStage(0, "x", 99); p.SetTier(0, "x", 0, -3);
            Assert.That(p.StageOf(0, "x"), Is.EqualTo(CampaignProfile.MaxStage), "SetStage clamps too");
            Assert.That(p.TierOf(0, "x", 0), Is.Zero);
            Assert.That(CampaignProfile.FromJson("{}").Gold, Is.EqualTo(CampaignProfile.StartingGold), "an empty file is a fresh campaign");
        }

        [Test]
        public void The_Store_Writes_And_Reads_A_File_And_Survives_Garbage()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-profile-" + Guid.NewGuid().ToString("N"));
            string path = Path.Combine(dir, ProfileStore.FileName);
            try
            {
                Assert.That(ProfileStore.LoadFrom(path).Gold, Is.EqualTo(CampaignProfile.StartingGold), "no file: a fresh campaign");
                var p = Sample();
                ProfileStore.SaveTo(p, path);
                Assert.That(File.Exists(path), Is.True);
                Assert.That(File.Exists(path + ".tmp"), Is.False, "the .tmp is swapped into place");
                p.Gold = 999;
                ProfileStore.SaveTo(p, path);                 // the second save goes through File.Replace
                var back = ProfileStore.LoadFrom(path);
                Assert.That(back.Gold, Is.EqualTo(999));
                Assert.That(back.StageOf(1, "tusk-yard"), Is.EqualTo(2));

                File.WriteAllText(path, "{ this is not json");
                Assert.That(ProfileStore.LoadFrom(path).Gold, Is.EqualTo(CampaignProfile.StartingGold), "garbage starts a fresh campaign");
            }
            finally
            {
                if (Directory.Exists(dir)) Directory.Delete(dir, true);
            }

            var mine = new CampaignProfile { Gold = 7 };
            ProfileStore.Use(mine);
            Assert.That(ProfileStore.Current, Is.SameAs(mine));
            ProfileStore.Use(null);
        }

        [Test]
        public void A_Clone_Is_Independent()
        {
            var p = Sample();
            var c = p.Clone();
            c.Gold = 1; c.Complete("lowlands", 2); c.SetStage(1, "tusk-yard", 3);
            Assert.That(p.Gold, Is.EqualTo(123));
            Assert.That(p.IsComplete("lowlands", 2), Is.False);
            Assert.That(p.StageOf(1, "tusk-yard"), Is.EqualTo(2));
        }
    }
}
