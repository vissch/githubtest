// Phase: B6 / docs/21 phase 6 (implemented) — the continental map's country nodes, each with three or four missions in
// order, its prerequisites and the gold it pays. A code table (like FactionRoster), so no asset has to be built to
// change it: a node's missions are MatchLaunch.Request presets (a ground, a seed, the enemy's knobs by difficulty).
// The graph: the lowlands first; from there the coast and the river line, either order; both open the high ground;
// the citadel is the end. A node is available when every prerequisite is complete, complete when its last mission
// is won; a replayed mission is available again but pays nothing (CampaignProfile.Complete says so).
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;

namespace TW.UI
{
    public sealed class CampaignDifficulty
    {
        public string Name, Blurb;
        public int PeerDeployEveryTicks = 40, PeerAttackGarrison = 8, PeerSupportReserve = 180;
        public bool PeerAttacks = true, PeerDeploysTanks, PeerUsesSupport = true;
        /// <summary>The enemy's upgrade tier (uniform), for the sim's upgrade seam when it lands (docs/21 B1).</summary>
        public int EnemyUpgradeTier;

        public static readonly CampaignDifficulty[] Standard =
        {
            new CampaignDifficulty { Name = "EASY", Blurb = "A slow enemy that attacks late and fields no armour or support.", PeerDeployEveryTicks = 60, PeerAttackGarrison = 12, PeerDeploysTanks = false, PeerUsesSupport = false, PeerSupportReserve = 400, EnemyUpgradeTier = 0 },
            new CampaignDifficulty { Name = "NORMAL", Blurb = "Deploys every two seconds, attacks at eight men, shells you when it can.", PeerDeployEveryTicks = 40, PeerAttackGarrison = 8, PeerDeploysTanks = false, PeerUsesSupport = true, PeerSupportReserve = 180, EnemyUpgradeTier = 1 },
            new CampaignDifficulty { Name = "HARD", Blurb = "Fast deploys, early attacks, tanks whenever it can afford them.", PeerDeployEveryTicks = 28, PeerAttackGarrison = 6, PeerDeploysTanks = true, PeerUsesSupport = true, PeerSupportReserve = 120, EnemyUpgradeTier = 2 },
        };
    }

    public sealed class CampaignMission
    {
        public string Title, Subtitle, Description;
        public Ground Ground;
        public uint Seed;
        public float Bombardment = 8f;
        public int StartingSilver = 300;
        public float SilverPerSecond = 2f;
        public string FrontLine = "90 x 240 M";

        /// <summary>The request the staging screen launches: the mission's ground and seed, the enemy at the difficulty.</summary>
        public MatchLaunch.Request Build(string missionId, int difficulty)
        {
            var d = CampaignDifficulty.Standard[Mathf.Clamp(difficulty, 0, CampaignDifficulty.Standard.Length - 1)];
            return new MatchLaunch.Request
            {
                MissionId = missionId, Title = Title, Difficulty = d.Name,
                // the same mission rolls the same dice every attempt (the seed is the mission's): a mission is a fixed
                // challenge, so the second try is the same fight fought better, not a different draw
                MatchSeed = 0xC0FFEEu ^ Seed, StartingSilver = StartingSilver, SilverPerSecond = SilverPerSecond,
                GeneratedBattlefield = true, PlaytestMap = false, BattlefieldSeed = Seed, Ground = Ground,
                Bombardment = Bombardment, ScriptedPeer = true,
                PeerDeployEveryTicks = Mathf.Max(1, d.PeerDeployEveryTicks), PeerAttacks = d.PeerAttacks, PeerAttackGarrison = d.PeerAttackGarrison,
                PeerDeploysTanks = d.PeerDeploysTanks, PeerUsesSupport = d.PeerUsesSupport, PeerSupportReserve = d.PeerSupportReserve,
            };
        }
    }

    public sealed class CampaignNode
    {
        public string Id, Name, Blurb;
        public Vector2 MapPos;
        public string[] Prerequisites = Array.Empty<string>();
        public CampaignMission[] Missions = Array.Empty<CampaignMission>();
        public int[] GoldRewards = Array.Empty<int>();
        public byte EnemyFaction;
        public bool Finale;
    }

    public static class CampaignGraph
    {
        public const int MinMissions = 3, MaxMissions = 4;
        public const int RewardFirst = 25, RewardMiddle = 40, RewardLast = 75;
        public static readonly CampaignNode[] Nodes = Build();
        /// <summary>The order the front-line ribbon runs through the nodes.</summary>
        public static readonly string[] FrontLine = { "lowlands", "the-coast", "river-line", "high-ground", "the-citadel" };

        static int[] Rewards(int missions)
        {
            var r = new int[missions];
            for (int i = 0; i < missions; i++) r[i] = i == 0 ? RewardFirst : i == missions - 1 ? RewardLast : RewardMiddle;
            return r;
        }

        static CampaignMission M(string title, string subtitle, Ground ground, uint seed, string description, float bombardment = 8f, string front = "90 x 240 M")
            => new CampaignMission { Title = title, Subtitle = subtitle, Ground = ground, Seed = seed, Description = description, Bombardment = bombardment, FrontLine = front };

        static CampaignNode[] Build()
        {
            var lowlands = new CampaignNode
            {
                Id = "lowlands", Name = "THE LOWLANDS", Blurb = "Flat wet country of shelled woods and one river: where the war began, and where the campaign does.",
                MapPos = new Vector2(0.30f, 0.62f), EnemyFaction = 1,
                Missions = new[]
                {
                    M("SHELLED WOOD, 1917", "THE LOWLANDS", Ground.ShelledForest, 1917, "A wood shelled to stumps, a river with fords and one bridge. Take the line."),
                    M("THE MILL CROSSING", "THE LOWLANDS", Ground.ShelledForest, 2201, "The river narrows at a mill. Hold the near bank, then cross under fire."),
                    M("BLACK ORCHARD", "THE LOWLANDS", Ground.ShelledForest, 2318, "An orchard the guns have been at for a year. The enemy's reserve line runs through it.", 12f),
                },
            };
            var coast = new CampaignNode
            {
                Id = "the-coast", Name = "THE COAST", Blurb = "The sea on the enemy's flank: reinforcements come ashore off the boats, and the gunboats have the beach.",
                MapPos = new Vector2(0.18f, 0.32f), Prerequisites = new[] { "lowlands" }, EnemyFaction = 1,
                Missions = new[]
                {
                    M("THE LANDING", "THE COAST", Ground.Landing, 3001, "Come ashore under the guns and hold the first dune line.", 6f, "90 x 260 M"),
                    M("THE DUNE ROAD", "THE COAST", Ground.Landing, 3102, "Push inland along the one road the sand allows.", 8f, "90 x 260 M"),
                    M("THE HARBOUR BATTERY", "THE COAST", Ground.Landing, 3203, "The battery that shelled the boats. Silence it.", 10f, "90 x 260 M"),
                },
            };
            var river = new CampaignNode
            {
                Id = "river-line", Name = "THE RIVER LINE", Blurb = "Four bridgeheads along the big river. Each one taken opens the next.",
                MapPos = new Vector2(0.50f, 0.50f), Prerequisites = new[] { "lowlands" }, EnemyFaction = 1,
                Missions = new[]
                {
                    M("FIRST BRIDGEHEAD", "THE RIVER LINE", Ground.ShelledForest, 4101, "The first crossing. The enemy has had a year to wire it."),
                    M("THE FLOODED FIELDS", "THE RIVER LINE", Ground.ShelledForest, 4202, "Between the crossings the fields are under water. Move on the causeways.", 6f),
                    M("THE BROKEN BRIDGE", "THE RIVER LINE", Ground.ShelledForest, 4303, "The bridge is down. The fords are the only way, and they know it.", 10f),
                    M("THE FAR BANK", "THE RIVER LINE", Ground.ShelledForest, 4404, "Everything they have left on the river. Break it.", 12f),
                },
            };
            var high = new CampaignNode
            {
                Id = "high-ground", Name = "THE HIGH GROUND", Blurb = "Winter on the ridge. The line freezes and the guns do not stop.",
                MapPos = new Vector2(0.68f, 0.30f), Prerequisites = new[] { "the-coast", "river-line" }, EnemyFaction = 1,
                Missions = new[]
                {
                    M("THE FROZEN LINE", "THE HIGH GROUND", Ground.WinterLine, 5001, "Snow on the wire. Take the first trench of the ridge."),
                    M("THE SAWMILL", "THE HIGH GROUND", Ground.WinterLine, 5102, "A sawmill hamlet holds the road up. Clear it house by house.", 8f),
                    M("THE WHITE SALIENT", "THE HIGH GROUND", Ground.WinterLine, 5203, "A salient into the enemy's line. Hold it against everything.", 12f),
                    M("THE SUMMIT", "THE HIGH GROUND", Ground.WinterLine, 5304, "The top of the ridge and the guns on it.", 14f),
                },
            };
            var citadel = new CampaignNode
            {
                Id = "the-citadel", Name = "THE CITADEL", Blurb = "The fortress city behind the ridge. The last line, and theirs.",
                MapPos = new Vector2(0.84f, 0.56f), Prerequisites = new[] { "high-ground" }, EnemyFaction = 1, Finale = true,
                Missions = new[]
                {
                    M("THE OUTWORKS", "THE CITADEL", Ground.WinterLine, 6001, "The forts before the walls. Reduce them one by one.", 12f),
                    M("THE BREACH", "THE CITADEL", Ground.WinterLine, 6102, "The guns have opened the wall. Go through it.", 14f),
                    M("THE CITADEL", "THE CITADEL", Ground.WinterLine, 6203, "The city itself. Take the headquarters and the war is over.", 16f),
                },
            };
            var nodes = new[] { lowlands, coast, river, high, citadel };
            foreach (var n in nodes) n.GoldRewards = Rewards(n.Missions.Length);
            return nodes;
        }

        public static CampaignNode Find(string id)
        {
            foreach (var n in Nodes) if (n.Id == id) return n;
            return null;
        }

        public static string MissionId(CampaignNode node, int index) => node.Id + "/" + index;

        public static int Reward(CampaignNode node, int index) => node != null && index >= 0 && index < node.GoldRewards.Length ? node.GoldRewards[index] : 0;

        /// <summary>Missions of the node the profile has won.</summary>
        public static int Done(CampaignNode node, CampaignProfile p)
        {
            int n = 0;
            for (int i = 0; i < node.Missions.Length; i++) if (p.IsComplete(node.Id, i)) n++;
            return n;
        }

        public static bool IsComplete(CampaignNode node, CampaignProfile p) => node.Missions.Length > 0 && p.IsComplete(node.Id, node.Missions.Length - 1);

        public static bool IsAvailable(CampaignNode node, CampaignProfile p)
        {
            foreach (var pre in node.Prerequisites)
            {
                var other = Find(pre);
                if (other == null || !IsComplete(other, p)) return false;
            }
            return true;
        }

        /// <summary>The next mission to fight in the node: the first not yet won, or the last again once all are.</summary>
        public static int NextMission(CampaignNode node, CampaignProfile p)
        {
            for (int i = 0; i < node.Missions.Length; i++) if (!p.IsComplete(node.Id, i)) return i;
            return Mathf.Max(0, node.Missions.Length - 1);
        }

        /// <summary>A mission can be fought when the node is available and every mission before it is won.</summary>
        public static bool CanFight(CampaignNode node, int index, CampaignProfile p)
        {
            if (!IsAvailable(node, p) || index < 0 || index >= node.Missions.Length) return false;
            for (int i = 0; i < index; i++) if (!p.IsComplete(node.Id, i)) return false;
            return true;
        }

        public static NodeState StateOf(CampaignNode node, CampaignProfile p, string current = null)
        {
            if (IsComplete(node, p)) return NodeState.Complete;
            if (!IsAvailable(node, p)) return NodeState.Locked;
            return node.Id == current || (current == null && node.Id == p.LastNode) ? NodeState.Current : NodeState.Available;
        }

        public static List<NodeView> Views(CampaignProfile p, string current = null)
        {
            var list = new List<NodeView>(Nodes.Length);
            foreach (var n in Nodes) list.Add(new NodeView { Id = n.Id, Name = n.Name, MapPos = n.MapPos, State = StateOf(n, p, current), Done = Done(n, p), Total = n.Missions.Length });
            return list;
        }
    }
}
