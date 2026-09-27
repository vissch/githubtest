// Phase: B6 / docs/21 phase 6 (implemented) — the country graph: every node has three or four missions paying
// 25 / 40 / 75, prerequisites name real nodes and only the lowlands is a root, the front line visits every node
// once, a fresh campaign opens only the lowlands, a won node opens what it leads to, missions are fought in order,
// and a mission builds a launch request with its ground, seed and the difficulty's enemy. No scene.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class CampaignGraphTests
    {
        static CampaignProfile Won(params string[] nodes)
        {
            var p = new CampaignProfile();
            foreach (var id in nodes)
            {
                var n = CampaignGraph.Find(id);
                for (int i = 0; i < n.Missions.Length; i++) p.Complete(id, i);
            }
            return p;
        }

        [Test]
        public void Every_Node_Has_Three_Or_Four_Missions_And_Pays_25_40_75()
        {
            Assert.That(CampaignGraph.Nodes.Length, Is.GreaterThanOrEqualTo(5));
            foreach (var n in CampaignGraph.Nodes)
            {
                Assert.That(n.Missions.Length, Is.InRange(CampaignGraph.MinMissions, CampaignGraph.MaxMissions), n.Id);
                Assert.That(n.GoldRewards.Length, Is.EqualTo(n.Missions.Length), n.Id);
                Assert.That(CampaignGraph.Reward(n, 0), Is.EqualTo(CampaignGraph.RewardFirst), n.Id);
                Assert.That(CampaignGraph.Reward(n, n.Missions.Length - 1), Is.EqualTo(CampaignGraph.RewardLast), n.Id);
                for (int i = 1; i < n.Missions.Length - 1; i++) Assert.That(CampaignGraph.Reward(n, i), Is.EqualTo(CampaignGraph.RewardMiddle), n.Id + "/" + i);
                Assert.That(n.MapPos.x, Is.InRange(0f, 1f), n.Id);
                Assert.That(n.MapPos.y, Is.InRange(0f, 1f), n.Id);
                foreach (var m in n.Missions) Assert.That(string.IsNullOrEmpty(m.Title) || string.IsNullOrEmpty(m.Description), Is.False, n.Id);
            }
            Assert.That(CampaignGraph.Reward(CampaignGraph.Nodes[0], 99), Is.Zero, "no such mission pays nothing");
        }

        [Test]
        public void Prerequisites_Name_Nodes_And_Only_The_Lowlands_Is_A_Root()
        {
            var roots = new List<string>();
            var ids = new HashSet<string>();
            foreach (var n in CampaignGraph.Nodes)
            {
                Assert.That(ids.Add(n.Id), n.Id + " appears twice");
                if (n.Prerequisites.Length == 0) roots.Add(n.Id);
                foreach (var pre in n.Prerequisites)
                {
                    Assert.That(CampaignGraph.Find(pre), Is.Not.Null, n.Id + " needs " + pre + ", which is no node");
                    Assert.That(pre, Is.Not.EqualTo(n.Id));
                }
            }
            Assert.That(roots, Is.EqualTo(new[] { "lowlands" }));
            Assert.That(CampaignGraph.Find("the-citadel").Finale, Is.True);
        }

        [Test]
        public void The_Front_Line_Runs_Through_Every_Node_Once()
        {
            Assert.That(CampaignGraph.FrontLine.Length, Is.EqualTo(CampaignGraph.Nodes.Length));
            var seen = new HashSet<string>();
            foreach (var id in CampaignGraph.FrontLine)
            {
                Assert.That(CampaignGraph.Find(id), Is.Not.Null, id);
                Assert.That(seen.Add(id), id + " twice on the line");
            }
        }

        [Test]
        public void A_Fresh_Campaign_Opens_Only_The_Lowlands()
        {
            var p = new CampaignProfile();
            foreach (var n in CampaignGraph.Nodes)
                Assert.That(CampaignGraph.StateOf(n, p), Is.EqualTo(n.Id == "lowlands" ? NodeState.Available : NodeState.Locked), n.Id);
            var views = CampaignGraph.Views(p);
            Assert.That(views.Count, Is.EqualTo(CampaignGraph.Nodes.Length));
            foreach (var v in views) { Assert.That(v.Done, Is.Zero); Assert.That(v.Total, Is.EqualTo(CampaignGraph.Find(v.Id).Missions.Length)); }
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("lowlands"), p, "lowlands"), Is.EqualTo(NodeState.Current));
        }

        [Test]
        public void Winning_The_Lowlands_Opens_The_Coast_And_The_River_But_Not_The_High_Ground()
        {
            var p = Won("lowlands");
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("lowlands"), p), Is.EqualTo(NodeState.Complete));
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("the-coast"), p), Is.EqualTo(NodeState.Available));
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("river-line"), p), Is.EqualTo(NodeState.Available));
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("high-ground"), p), Is.EqualTo(NodeState.Locked));
            Assert.That(CampaignGraph.StateOf(CampaignGraph.Find("the-citadel"), p), Is.EqualTo(NodeState.Locked));

            p = Won("lowlands", "the-coast");
            Assert.That(CampaignGraph.IsAvailable(CampaignGraph.Find("high-ground"), p), Is.False, "needs the river line too");
            p = Won("lowlands", "the-coast", "river-line");
            Assert.That(CampaignGraph.IsAvailable(CampaignGraph.Find("high-ground"), p), Is.True);
            Assert.That(CampaignGraph.IsAvailable(CampaignGraph.Find("the-citadel"), p), Is.False);
            p = Won("lowlands", "the-coast", "river-line", "high-ground");
            Assert.That(CampaignGraph.IsAvailable(CampaignGraph.Find("the-citadel"), p), Is.True);
        }

        [Test]
        public void Missions_Are_Fought_In_Order()
        {
            var p = new CampaignProfile();
            var n = CampaignGraph.Find("lowlands");
            Assert.That(CampaignGraph.CanFight(n, 0, p), Is.True);
            Assert.That(CampaignGraph.CanFight(n, 1, p), Is.False);
            Assert.That(CampaignGraph.NextMission(n, p), Is.Zero);
            Assert.That(p.Complete("lowlands", 0), Is.True);
            Assert.That(CampaignGraph.CanFight(n, 1, p), Is.True);
            Assert.That(CampaignGraph.CanFight(n, 2, p), Is.False);
            Assert.That(CampaignGraph.NextMission(n, p), Is.EqualTo(1));
            Assert.That(CampaignGraph.Done(n, p), Is.EqualTo(1));
            Assert.That(CampaignGraph.IsComplete(n, p), Is.False);
            Assert.That(CampaignGraph.CanFight(CampaignGraph.Find("the-coast"), 0, p), Is.False, "the coast is locked until the lowlands are won");
            p.Complete("lowlands", 1); p.Complete("lowlands", 2);
            Assert.That(CampaignGraph.IsComplete(n, p), Is.True);
            Assert.That(CampaignGraph.NextMission(n, p), Is.EqualTo(2), "a won node offers its last mission again");
            Assert.That(CampaignGraph.CanFight(n, 0, p), Is.True, "and any earlier one");
        }

        [Test]
        public void A_Mission_Builds_A_Request_With_Its_Ground_Seed_And_Difficulty()
        {
            var n = CampaignGraph.Find("the-coast");
            var r = n.Missions[0].Build(CampaignGraph.MissionId(n, 0), 2);
            Assert.That(r.MissionId, Is.EqualTo("the-coast/0"));
            Assert.That(r.Title, Is.EqualTo(n.Missions[0].Title));
            Assert.That(r.Ground, Is.EqualTo(Ground.Landing));
            Assert.That(r.BattlefieldSeed, Is.EqualTo(3001u));
            Assert.That(r.GeneratedBattlefield, Is.True);
            Assert.That(r.PlaytestMap, Is.False);
            Assert.That(r.Difficulty, Is.EqualTo("HARD"));
            Assert.That(r.PeerDeploysTanks, Is.True);
            Assert.That(r.PeerDeployEveryTicks, Is.EqualTo(CampaignDifficulty.Standard[2].PeerDeployEveryTicks));

            var easy = n.Missions[0].Build(CampaignGraph.MissionId(n, 0), 0);
            Assert.That(easy.Difficulty, Is.EqualTo("EASY"));
            Assert.That(easy.PeerUsesSupport, Is.False);
            Assert.That(easy.MatchSeed, Is.EqualTo(r.MatchSeed), "the difficulty changes the enemy, not the dice");
            Assert.That(n.Missions[0].Build("x", 99).Difficulty, Is.EqualTo("HARD"), "an out-of-range difficulty clamps");
        }

        [Test]
        public void Mission_Seeds_Are_Distinct()
        {
            var seeds = new HashSet<uint>();
            foreach (var n in CampaignGraph.Nodes)
                foreach (var m in n.Missions)
                    Assert.That(seeds.Add(m.Seed), n.Id + " reuses seed " + m.Seed);
        }
    }
}
